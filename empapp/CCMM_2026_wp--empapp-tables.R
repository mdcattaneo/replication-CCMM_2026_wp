#!/usr/bin/env Rscript

script_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]))))
  }
  normalizePath(getwd())
}

option_value <- function(args, name, default = NULL) {
  prefix <- paste0("--", name, "=")
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) return(default)
  sub(paste0("^", prefix), "", hit[[length(hit)]])
}

args <- commandArgs(trailingOnly = TRUE)
root <- script_root()
input_path <- option_value(
  args, "input", file.path(root, "output", "CCMM_2026_wp--empapp-results.rds")
)
tables_dir <- option_value(args, "tables-dir", file.path(root, "tables"))
if (!file.exists(input_path)) {
  stop("Aggregate empirical results not found: ", input_path, call. = FALSE)
}
results <- readRDS(input_path)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

descriptive_lines <- c(
  "\\begin{tabular}{ccccc}",
  "\\toprule",
  "Inside-menu size & Observations & Mean displays & No-display share & Default-choice share \\\\",
  "\\midrule",
  vapply(seq_len(nrow(results$descriptives)), function(index) {
    row <- results$descriptives[index, ]
    sprintf(
      "$%d$ & %d & %.3f & %.3f & %.3f \\\\",
      row$inside_menu_size, row$observations, row$mean_displays,
      row$no_display_share, row$default_choice_share
    )
  }, character(1L)),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(
  descriptive_lines,
  file.path(tables_dir, "CCMM_2026_wp--table-empapp-descriptives.tex")
)

format_interval <- function(lower, upper) {
  ifelse(
    is.finite(lower) & is.finite(upper),
    sprintf("[%.3f, %.3f]", lower, upper),
    "--"
  )
}

pairwise_rows <- function(data) {
  vapply(seq_len(nrow(data)), function(index) {
    sprintf(
      "& $%s\\succ %s$ & %.3f & %.3f & %s & %s & %s & %s \\\\",
      data$later_mode[index],
      data$earlier_mode[index],
      data$estimate[index],
      data$reported_share[index],
      format_interval(data$iid_lower[index], data$iid_upper[index]),
      format_interval(
        data$iid_difference_lower[index],
        data$iid_difference_upper[index]
      ),
      format_interval(data$cluster_lower[index], data$cluster_upper[index]),
      format_interval(
        data$cluster_difference_lower[index],
        data$cluster_difference_upper[index]
      )
    )
  }, character(1L))
}

pairwise_lines <- c(
  "\\begin{tabular}{clcccccc}",
  "\\toprule",
  paste0(
    "& & \\multicolumn{2}{c}{Point estimates} & ",
    "\\multicolumn{2}{c}{Row-i.i.d.} & ",
    "\\multicolumn{2}{c}{Clustered} \\\\"
  ),
  "\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}\\cmidrule(lr){7-8}",
  paste0(
    "& & H-LAO & Reported & H-LAO CI & Difference CI & ",
    "H-LAO CI & Difference CI \\\\"
  ),
  "\\midrule",
  "\\multicolumn{8}{l}{\\textit{Complete sample}} \\\\",
  pairwise_rows(results$complete_design$pairwise_inference),
  "\\addlinespace",
  "\\multicolumn{8}{l}{\\textit{Dominance-consistent sample}} \\\\",
  pairwise_rows(results$dominance_consistent$pairwise_inference),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(
  pairwise_lines,
  file.path(
    tables_dir,
    "CCMM_2026_wp--table-empapp-pairwise-inference.tex"
  )
)

ram <- results$ram_inference
ram_rows <- list()
ram_index <- 0L
for (ranking_group in c("reported-rational", "reported-irrational")) {
  for (sampling in c("iid", "cluster")) {
    selected <- ram$sampling == sampling & ram$ranking_group == ranking_group
    gms <- ram[selected & ram$method == "GMS", , drop = FALSE]
    lf <- ram[selected & ram$method == "LF", , drop = FALSE]
    ram_index <- ram_index + 1L
    ram_rows[[ram_index]] <- data.frame(
      sampling = sampling,
      ranking_group = ranking_group,
      gms_nonrejected = sum(!gms$reject),
      gms_min_p = min(gms$p_value),
      lf_nonrejected = sum(!lf$reject),
      lf_min_p = min(lf$p_value)
    )
  }
}
ram_summary <- do.call(rbind, ram_rows)
ram_lines <- c(
  "\\begin{tabular}{llcccc}",
  "\\toprule",
  paste0(
    "& & \\multicolumn{2}{c}{LF} & ",
    "\\multicolumn{2}{c}{GMS} \\\\"
  ),
  "\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}",
  paste0(
    "Candidate rankings & Sampling & Nonrejected & Min. $p$-value & ",
    "Nonrejected & Min. $p$-value \\\\"
  ),
  "\\midrule",
  vapply(seq_len(nrow(ram_summary)), function(index) {
    sprintf(
      "%s & %s & %d & %.3f & %d & %.3f \\\\",
      if (ram_summary$ranking_group[index] == "reported-rational") {
        "Nine rational"
      } else {
        "Two irrational"
      },
      if (ram_summary$sampling[index] == "iid") "Row-i.i.d." else "Clustered",
      ram_summary$lf_nonrejected[index],
      ram_summary$lf_min_p[index],
      ram_summary$gms_nonrejected[index],
      ram_summary$gms_min_p[index]
    )
  }, character(1L)),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(
  ram_lines,
  file.path(tables_dir, "CCMM_2026_wp--table-empapp-ram-inference.tex")
)

aom <- results$aom_inference
aom_rows <- list()
aom_index <- 0L
for (sample_name in c("complete", "dominance-consistent")) {
  for (sampling in c("iid", "cluster")) {
    gms <- aom[
      aom$sample == sample_name & aom$sampling == sampling &
        aom$method == "GMS",
      , drop = FALSE
    ]
    lf <- aom[
      aom$sample == sample_name & aom$sampling == sampling &
        aom$method == "LF",
      , drop = FALSE
    ]
    aom_index <- aom_index + 1L
    aom_rows[[aom_index]] <- data.frame(
      sample = sample_name,
      sampling = sampling,
      gms_nonrejected = sum(!gms$reject),
      gms_min_p = min(gms$p_value),
      lf_nonrejected = sum(!lf$reject),
      lf_min_p = min(lf$p_value)
    )
  }
}
aom_summary <- do.call(rbind, aom_rows)
aom_lines <- c(
  "\\begin{tabular}{llcccc}",
  "\\toprule",
  "& & \\multicolumn{2}{c}{LF} & \\multicolumn{2}{c}{GMS} \\\\",
  "\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}",
  paste0(
    "Sample & Sampling & Nonrejected & Min. $p$-value & ",
    "Nonrejected & Min. $p$-value \\\\"
  ),
  "\\midrule",
  vapply(seq_len(nrow(aom_summary)), function(index) {
    sprintf(
      "%s & %s & %d & %.3f & %d & %.3f \\\\",
      if (aom_summary$sample[index] == "complete") {
        "Complete"
      } else {
        "Dominance-consistent"
      },
      if (aom_summary$sampling[index] == "iid") "Row-i.i.d." else "Clustered",
      aom_summary$lf_nonrejected[index],
      aom_summary$lf_min_p[index],
      aom_summary$gms_nonrejected[index],
      aom_summary$gms_min_p[index]
    )
  }, character(1L)),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(
  aom_lines,
  file.path(tables_dir, "CCMM_2026_wp--table-empapp-aom-inference.tex")
)

specification <- results$hlao_specification
specification <- specification[
  specification$restriction %in% c(
    "attention-overload", "block-marschak", "omnibus"
  ),
  , drop = FALSE
]
specification_lines <- c(
  "\\begin{tabular}{llccccc}",
  "\\toprule",
  paste0(
    "& & & \\multicolumn{2}{c}{Row-i.i.d.} & ",
    "\\multicolumn{2}{c}{Clustered} \\\\"
  ),
  "\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
  paste0(
    "Sample & Restriction & Max. discrepancy & Lower & $p$-value & ",
    "Lower & $p$-value \\\\"
  ),
  "\\midrule"
)
for (sample_name in c("complete", "dominance-consistent")) {
  for (restriction in c("attention-overload", "block-marschak", "omnibus")) {
    iid <- specification[
      specification$sample == sample_name & specification$sampling == "iid" &
        specification$restriction == restriction,
      , drop = FALSE
    ]
    clustered <- specification[
      specification$sample == sample_name &
        specification$sampling == "cluster" &
        specification$restriction == restriction,
      , drop = FALSE
    ]
    specification_lines <- c(
      specification_lines,
      sprintf(
        "%s & %s & %.3f & %.3f & %.3f & %.3f & %.3f \\\\",
        if (sample_name == "complete") "Complete" else "Dominance-consistent",
        switch(
          restriction,
          `attention-overload` = "Attention overload",
          `block-marschak` = "Block--Marschak",
          "Omnibus"
        ),
        iid$max_violation_estimate,
        iid$max_violation_lower,
        iid$p_value,
        clustered$max_violation_lower,
        clustered$p_value
      )
    )
  }
}
specification_lines <- c(
  specification_lines,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(
  specification_lines,
  file.path(tables_dir, "CCMM_2026_wp--table-empapp-hlao-specification.tex")
)

common <- results$common_menu$intervals
iid_common <- common[common$sampling == "iid", , drop = FALSE]
cluster_common <- common[common$sampling == "cluster", , drop = FALSE]
cluster_common <- cluster_common[
  match(iid_common$event, cluster_common$event),
  , drop = FALSE
]
format_preference_event <- function(event) {
  alternatives <- strsplit(event, " above ", fixed = TRUE)[[1L]]
  sprintf("$%s \\succ %s$", alternatives[1L], alternatives[2L])
}
common_lines <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{95\\% outer confidence bounds} \\\\",
  "\\cmidrule(lr){2-3}",
  "Preference event & Row-i.i.d. & Subject-clustered \\\\",
  "\\midrule",
  vapply(seq_len(nrow(iid_common)), function(index) {
    sprintf(
      "%s & %s & %s \\\\",
      format_preference_event(iid_common$event[index]),
      format_interval(iid_common$lower[index], iid_common$upper[index]),
      format_interval(cluster_common$lower[index], cluster_common$upper[index])
    )
  }, character(1L)),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(
  common_lines,
  file.path(tables_dir, "CCMM_2026_wp--table-empapp-common-menu.tex")
)

search <- results$search$comparisons
search_summary <- function(prefix, label) {
  estimate <- search[[paste0(prefix, "_difference")]]
  data.frame(
    proxy = label,
    mean = mean(estimate),
    negative = sum(estimate < 0),
    positive = sum(estimate > 0),
    iid_negative = sum(search[[paste0(prefix, "_iid_upper")]] < 0),
    iid_positive = sum(search[[paste0(prefix, "_iid_lower")]] > 0),
    cluster_negative = sum(search[[paste0(prefix, "_cluster_upper")]] < 0),
    cluster_positive = sum(search[[paste0(prefix, "_cluster_lower")]] > 0)
  )
}
search_table <- rbind(
  search_summary("inspected", "Displayed"),
  search_summary("inspected_or_chosen", "Displayed or chosen")
)
search_lines <- c(
  "\\begin{tabular}{lrrrcccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Differences} & \\multicolumn{2}{c}{Row-i.i.d.} & \\multicolumn{2}{c}{Clustered} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-6} \\cmidrule(lr){7-8}",
  "Attention proxy & Mean & Negative & Positive & Sig. negative & Sig. positive & Sig. negative & Sig. positive \\\\",
  "\\midrule",
  vapply(seq_len(nrow(search_table)), function(index) {
    sprintf(
      "%s & %.3f & %d & %d & %d & %d & %d & %d \\\\",
      search_table$proxy[index],
      search_table$mean[index],
      search_table$negative[index],
      search_table$positive[index],
      search_table$iid_negative[index],
      search_table$iid_positive[index],
      search_table$cluster_negative[index],
      search_table$cluster_positive[index]
    )
  }, character(1L)),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(
  search_lines,
  file.path(tables_dir, "CCMM_2026_wp--table-empapp-search.tex")
)

cat("Wrote empirical LaTeX tables to", normalizePath(tables_dir), "\n")
