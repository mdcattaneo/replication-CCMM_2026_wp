################################################################################
# Attention Overload
# 2026 paper-table renderer
################################################################################

options(stringsAsFactors = FALSE)

script_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (!length(file_arg)) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }
  dirname(normalizePath(
    sub("^--file=", "", file_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
  ))
}

project_root <- script_root()
output_dir <- file.path(project_root, "output")
tables_dir <- file.path(project_root, "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

parse_options <- function(args) {
  list(pilot = "--pilot" %in% args)
}

read_block <- function(block, pilot) {
  suffix <- if (pilot) "--pilot" else ""
  path <- file.path(
    output_dir,
    paste0("CCMM_2026_wp--raw-", block, suffix, ".rds")
  )
  if (!file.exists(path)) {
    stop("Missing raw simulation file: ", path, call. = FALSE)
  }
  object <- readRDS(path)
  has_results <- is.data.frame(object$results)
  has_files <- length(object$result_files) > 0L
  if (!is.list(object) || (!has_results && !has_files)) {
    stop("Invalid raw simulation object: ", path, call. = FALSE)
  }
  object
}

result_data <- function(object, design = NULL) {
  if (is.data.frame(object$results)) {
    if (is.null(design)) return(object$results)
    return(object$results[object$results$design_id == design, , drop = FALSE])
  }
  files <- object$result_files
  if (!is.null(design)) {
    files <- files[names(files) == design]
    if (!length(files)) stop("No checkpoint for design: ", design, call. = FALSE)
  }
  pieces <- lapply(files, function(path) {
    readRDS(file.path(project_root, path))
  })
  do.call(rbind, pieces)
}

mean_na <- function(x) {
  if (!length(x) || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

format_number <- function(x, digits = 3L) {
  if (!length(x) || !is.finite(x)) return("--")
  formatC(x, format = "f", digits = digits)
}

format_rate <- function(x) format_number(x, 3L)

write_latex <- function(lines, filename, pilot) {
  prefix <- if (pilot) "pilot_" else ""
  path <- file.path(tables_dir, paste0(prefix, filename))
  writeLines(lines, path, useBytes = TRUE)
  message("Wrote table: ", path)
  invisible(path)
}

config_id <- function(design_id) {
  sub("^HLAO-(H[0-9]+)-.*$", "\\1", design_id)
}

method_group <- function(method) {
  ifelse(grepl("hoeffding", method), "Hoeffding", "Gaussian")
}

build_aom_table <- function(object, pilot) {
  data <- result_data(object)
  support_order <- c(
    "sizes-2-to-6", "sizes-3-to-6", "sizes-4-to-6", "sizes-5-to-6",
    "sizes-2-3-4-6", "sizes-2-3-6", "sizes-2-6"
  )
  support_label <- c(
    "2--6", "3--6", "4--6", "5--6", "2, 3, 4, 6", "2, 3, 6", "2, 6"
  )
  rows <- character(0L)
  for (support in support_order) {
    for (sample_size in c(50L, 100L, 200L)) {
      selected <- data[
        data$menu_support == support & data$n_per_menu == sample_size,
        ,
        drop = FALSE
      ]
      if (!nrow(selected)) next
      preferences <- sort(unique(selected$preference_id))
      rejection <- vapply(preferences, function(preference) {
        mean_na(selected$reject[selected$preference_id == preference])
      }, numeric(1L))
      null <- vapply(preferences, function(preference) {
        unique(selected$is_null[selected$preference_id == preference])[[1L]]
      }, logical(1L))
      entries <- vapply(seq_along(rejection), function(index) {
        value <- format_rate(rejection[[index]])
        if (isTRUE(null[[index]])) paste0(value, "$^{\\dagger}$") else value
      }, character(1L))
      rows <- c(rows, paste0(
        paste(
          support_label[match(support, support_order)],
          sample_size,
          unique(selected$n_inequalities)[[1L]],
          paste(entries, collapse = " & "),
          sep = " & "
        ),
        " \\\\"
      ))
    }
    rows <- c(rows, "\\addlinespace[2pt]")
  }
  rows <- rows[-length(rows)]
  lines <- c(
    "\\begin{table}[t]",
    "\\centering",
    "\\caption{Finite-sample rejection probabilities: homogeneous AOM}",
    "\\label{tab:simulation-aom-generated}",
    "\\begin{tabular}{lrrcccc}",
    "\\toprule",
    "Menu sizes & $n$ & Inequalities & $\\succ_1$ & $\\succ_2$ & $\\succ_3$ & $\\succ_4$ \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{0.94\\textwidth}",
    "\\footnotesize Notes: Entries are Monte Carlo rejection probabilities for the GMS test at the five-percent level. A dagger marks a preference ordering that satisfies the population inequalities. Each menu in the indicated support has the reported sample size.",
    "\\end{minipage}",
    "\\end{table}"
  )
  write_latex(lines, "CCMM_2026_wp--table-aom.tex", pilot)
}

estimation_summary <- function(data, estimand_type) {
  selected <- data[
    data$estimand_type == estimand_type,
    ,
    drop = FALSE
  ]
  c(bias = mean_na(selected$error), rmse = sqrt(mean_na(selected$squared_error)))
}

build_hlao_estimation_table <- function(object, pilot) {
  rows <- character(0L)
  for (config in sprintf("H%02d", 1:10)) {
    for (sample_size in c(100L, 500L)) {
      design <- paste0("HLAO-", config, "-N", sprintf("%03d", sample_size))
      data <- result_data(object, design)
      reach <- estimation_summary(data, "reach")
      full <- estimation_summary(data, "full-attention")
      rows <- c(rows, paste0(
        paste(
          config,
          sample_size,
          format_number(reach[["bias"]]),
          format_number(reach[["rmse"]]),
          format_number(full[["bias"]]),
          format_number(full[["rmse"]]),
          sep = " & "
        ),
        " \\\\"
      ))
    }
    rows <- c(rows, "\\addlinespace[2pt]")
  }
  rows <- rows[-length(rows)]
  lines <- c(
    "\\begin{table}[t]",
    "\\centering",
    "\\caption{H-LAO point estimation}",
    "\\label{tab:simulation-hlao-estimation}",
    "\\begin{tabular}{lrrrrr}",
    "\\toprule",
    "& & \\multicolumn{2}{c}{Reach probabilities} & \\multicolumn{2}{c}{Full-attention rule} \\\\",
    "\\cmidrule(lr){3-4} \\cmidrule(lr){5-6}",
    "Design & $n$ & Bias & RMSE & Bias & RMSE \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{0.88\\textwidth}",
    "\\footnotesize Notes: Bias and RMSE are averaged across replications and the estimands available in each design. Dashes indicate that the menu support does not identify the recursive full-attention rule.",
    "\\end{minipage}",
    "\\end{table}"
  )
  write_latex(lines, "CCMM_2026_wp--table-hlao-estimation.tex", pilot)
}

interval_summary <- function(data, estimand_type, group) {
  selected <- data[
    data$estimand_type == estimand_type &
      data$target_valid %in% TRUE & method_group(data$method) == group,
    ,
    drop = FALSE
  ]
  c(coverage = mean_na(selected$covered), width = mean_na(selected$width))
}

interval_panel <- function(object, configs, estimand_type) {
  rows <- character(0L)
  for (config in configs) {
    for (sample_size in c(100L, 500L)) {
      design <- paste0("HLAO-", config, "-N", sprintf("%03d", sample_size))
      data <- result_data(object, design)
      hoeffding <- interval_summary(data, estimand_type, "Hoeffding")
      gaussian <- interval_summary(data, estimand_type, "Gaussian")
      rows <- c(rows, paste0(
        paste(
          config,
          sample_size,
          format_rate(hoeffding[["coverage"]]),
          format_number(hoeffding[["width"]]),
          format_rate(gaussian[["coverage"]]),
          format_number(gaussian[["width"]]),
          sep = " & "
        ),
        " \\\\"
      ))
    }
    rows <- c(rows, "\\addlinespace[2pt]")
  }
  rows[-length(rows)]
}

build_hlao_inference_table <- function(object, pilot) {
  pairwise <- interval_panel(object, sprintf("H%02d", 1:10), "pairwise-share")
  events <- interval_panel(object, sprintf("H%02d", 1:8), "preference-event")
  lines <- c(
    "\\begin{table}[t]",
    "\\centering",
    "\\caption{H-LAO confidence intervals}",
    "\\label{tab:simulation-hlao-inference}",
    "\\begin{tabular}{lrrrrr}",
    "\\toprule",
    "& & \\multicolumn{2}{c}{Hoeffding} & \\multicolumn{2}{c}{Correlated Gaussian} \\\\",
    "\\cmidrule(lr){3-4} \\cmidrule(lr){5-6}",
    "Design & $n$ & Coverage & Width & Coverage & Width \\\\",
    "\\midrule",
    "\\multicolumn{6}{l}{\\textit{Panel A. Pairwise preference shares under weak reach}} \\\\",
    "\\addlinespace[2pt]",
    pairwise,
    "\\midrule",
    "\\multicolumn{6}{l}{\\textit{Panel B. General preference events under unrestricted dependence}} \\\\",
    "\\addlinespace[2pt]",
    events,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{0.91\\textwidth}",
    "\\footnotesize Notes: Coverage and average width are averaged over replications and valid targets. The pairwise target is omitted in the dependent preference--attention design H07 because the weak-reach interpretation requires independence. Event intervals are reported for the four-alternative designs.",
    "\\end{minipage}",
    "\\end{table}"
  )
  write_latex(lines, "CCMM_2026_wp--table-hlao-inference.tex", pilot)
}

diagnostic_method_label <- function(method) {
  labels <- c(
    hoeffding = "Hoeffding outer",
    `correlated-gaussian-exact-hybrid` = "Gaussian outer",
    `direct-delta-gaussian` = "Direct delta"
  )
  unname(labels[method])
}

build_hlao_diagnostic_table <- function(object, pilot) {
  data <- result_data(object)
  design_types <- data.frame(
    prefix = c("DIAG-NULL", "DIAG-RLA-L", "DIAG-RLA-F", "DIAG-BM-L", "DIAG-BM-F"),
    label = c("Null", "AO local", "AO fixed", "BM local", "BM fixed"),
    restriction = c("omnibus", "attention-overload", "attention-overload", "block-marschak", "block-marschak")
  )
  methods <- c(
    "hoeffding", "correlated-gaussian-exact-hybrid", "direct-delta-gaussian"
  )
  rows <- character(0L)
  for (index in seq_len(nrow(design_types))) {
    for (sample_size in c(100L, 200L, 500L)) {
      design <- paste0(design_types$prefix[index], "-N", sprintf("%03d", sample_size))
      selected <- data[
        data$design_id == design &
          data$restriction == design_types$restriction[index],
        ,
        drop = FALSE
      ]
      rates <- vapply(methods, function(method) {
        mean_na(selected$reject[selected$method == method])
      }, numeric(1L))
      violation <- unique(selected$population_violation)
      rows <- c(rows, paste0(
        paste(
          design_types$label[index],
          sample_size,
          format_number(if (length(violation)) violation[[1L]] else NA_real_),
          paste(vapply(rates, format_rate, character(1L)), collapse = " & "),
          sep = " & "
        ),
        " \\\\"
      ))
    }
    rows <- c(rows, "\\addlinespace[2pt]")
  }
  rows <- rows[-length(rows)]
  lines <- c(
    "\\begin{table}[t]",
    "\\centering",
    "\\caption{H-LAO specification diagnostics}",
    "\\label{tab:simulation-hlao-diagnostics}",
    "\\begin{tabular}{lrrrrr}",
    "\\toprule",
    "Design & $n$ & Violation & Hoeffding outer & Gaussian outer & Direct delta \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{0.94\\textwidth}",
    "\\footnotesize Notes: Entries in the final three columns are rejection probabilities at the five-percent level. The null row uses the omnibus diagnostic; AO and BM alternatives use their corresponding targeted restrictions. Local violations shrink at the parametric rate, while fixed violations do not depend on sample size.",
    "\\end{minipage}",
    "\\end{table}"
  )
  write_latex(lines, "CCMM_2026_wp--table-hlao-diagnostics.tex", pilot)
}

main <- function() {
  options <- parse_options(commandArgs(trailingOnly = TRUE))
  aom <- read_block("homogeneous-aom", options$pilot)
  hlao <- read_block("hlao", options$pilot)
  diagnostic <- read_block("hlao-diagnostic", options$pilot)
  build_aom_table(aom, options$pilot)
  build_hlao_estimation_table(hlao, options$pilot)
  build_hlao_inference_table(hlao, options$pilot)
  build_hlao_diagnostic_table(diagnostic, options$pilot)
}

if (sys.nframe() == 0L) main()
