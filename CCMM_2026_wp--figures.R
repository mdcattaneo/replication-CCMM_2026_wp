################################################################################
# Attention Overload
# 2026 paper-figure renderer
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
figures_dir <- file.path(project_root, "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

read_hlao <- function(pilot) {
  suffix <- if (pilot) "--pilot" else ""
  path <- file.path(
    output_dir,
    paste0("CCMM_2026_wp--raw-hlao", suffix, ".rds")
  )
  if (!file.exists(path)) stop("Missing raw simulation file: ", path, call. = FALSE)
  object <- readRDS(path)
  has_results <- is.data.frame(object$results)
  has_files <- length(object$result_files) > 0L
  if (!is.list(object) || (!has_results && !has_files)) {
    stop("Invalid raw simulation object: ", path, call. = FALSE)
  }
  object
}

result_data <- function(object, design) {
  if (is.data.frame(object$results)) {
    return(object$results[object$results$design_id == design, , drop = FALSE])
  }
  files <- object$result_files[names(object$result_files) == design]
  if (!length(files)) stop("No checkpoint for design: ", design, call. = FALSE)
  readRDS(file.path(project_root, files[[1L]]))
}

mean_na <- function(x) {
  if (!length(x) || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

figure_path <- function(filename, pilot) {
  prefix <- if (pilot) "pilot_" else ""
  file.path(figures_dir, paste0(prefix, filename))
}

plot_reach_performance <- function(object, pilot) {
  summaries <- list()
  summary_index <- 0L
  for (design_index in seq_len(nrow(object$design))) {
    design <- object$design[design_index, ]
    data <- result_data(object, design$design_id)
    data <- data[
      data$estimand_type == "pairwise-share" & data$target_valid %in% TRUE,
      ,
      drop = FALSE
    ]
    data$method_group <- ifelse(
      grepl("hoeffding", data$method), "Hoeffding", "Correlated Gaussian"
    )
    for (method in c("Hoeffding", "Correlated Gaussian")) {
      selected <- data[data$method_group == method, , drop = FALSE]
      if (!nrow(selected)) next
      summary_index <- summary_index + 1L
      summaries[[summary_index]] <- data.frame(
        design_id = design$design_id,
        n_per_menu = design$n_per_menu,
        reach_design = design$reach_design,
        method_group = method,
        coverage = mean_na(selected$covered),
        width = mean_na(selected$width)
      )
    }
  }
  designs <- do.call(rbind, summaries)

  path <- figure_path("CCMM_2026_wp--figure-hlao-reach.pdf", pilot)
  grDevices::pdf(path, width = 8.2, height = 6.4, useDingbats = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(2L, 2L), mar = c(4.2, 4.2, 2.2, 1.0), las = 1)
  colors <- c(high = "#0072B2", low = "#D55E00", zero = "#009E73")
  points <- c(high = 16L, low = 17L, zero = 15L)
  sample_sizes <- sort(unique(designs$n_per_menu))

  for (metric in c("coverage", "width")) {
    for (method in c("Hoeffding", "Correlated Gaussian")) {
      selected <- designs[designs$method_group == method, , drop = FALSE]
      y <- selected[[metric]]
      limits <- if (metric == "coverage") c(0.75, 1.01) else c(0, max(y, na.rm = TRUE) * 1.08)
      graphics::plot(
        sample_sizes, rep(NA_real_, length(sample_sizes)),
        type = "n", ylim = limits, xlab = "Observations per menu",
        ylab = if (metric == "coverage") "Coverage probability" else "Average interval width",
        main = method
      )
      if (metric == "coverage") graphics::abline(h = 0.95, lty = 2L, col = "gray45")
      for (reach in names(colors)) {
        values <- vapply(sample_sizes, function(sample_size) {
          mean_na(selected[[metric]][
            selected$n_per_menu == sample_size & selected$reach_design == reach
          ])
        }, numeric(1L))
        graphics::lines(
          sample_sizes, values, type = "b", lwd = 1.6,
          pch = points[[reach]], col = colors[[reach]]
        )
      }
      if (metric == "coverage" && method == "Hoeffding") {
        graphics::legend(
          "bottomright", legend = c("High reach", "Low reach", "Zero reach"),
          col = colors, pch = points, lty = 1L, bty = "n", cex = 0.82
        )
      }
    }
  }
  grDevices::dev.off()
  on.exit(NULL, add = FALSE)
  message("Wrote figure: ", path)
  invisible(path)
}

plot_runtime <- function(object, pilot) {
  configurations <- sprintf("H%02d", 1:10)
  methods <- c("Hoeffding", "Correlated Gaussian")
  runtime <- matrix(
    NA_real_,
    nrow = length(configurations),
    ncol = length(methods),
    dimnames = list(configurations, methods)
  )
  maximum_sample_size <- max(object$design$n_per_menu)
  selected_designs <- object$design[
    object$design$n_per_menu == maximum_sample_size,
    ,
    drop = FALSE
  ]
  for (design_index in seq_len(nrow(selected_designs))) {
    design <- selected_designs[design_index, ]
    config <- sub("^HLAO-(H[0-9]+)-.*$", "\\1", design$design_id)
    data <- result_data(object, design$design_id)
    data <- data[data$estimand_type == "pairwise-share", , drop = FALSE]
    data$method_group <- ifelse(
      grepl("hoeffding", data$method), "Hoeffding", "Correlated Gaussian"
    )
    for (method in methods) {
      rows <- data[data$method_group == method, , drop = FALSE]
      replication_runtime <- aggregate(
        rows$elapsed_seconds,
        list(replication = rows$replication),
        function(x) unique(x)[[1L]]
      )$x
      runtime[config, method] <- mean_na(replication_runtime)
    }
  }

  path <- figure_path("CCMM_2026_wp--figure-hlao-runtime.pdf", pilot)
  grDevices::pdf(path, width = 7.4, height = 4.7, useDingbats = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(4.2, 4.5, 1.2, 0.8), las = 1)
  graphics::barplot(
    t(runtime), beside = TRUE, names.arg = configurations,
    col = c("#0072B2", "#E69F00"), border = NA,
    ylab = "Average seconds per replication", xlab = "Simulation design"
  )
  graphics::legend(
    "topleft", legend = colnames(runtime), fill = c("#0072B2", "#E69F00"),
    bty = "n", horiz = TRUE, cex = 0.9
  )
  grDevices::dev.off()
  on.exit(NULL, add = FALSE)
  message("Wrote figure: ", path)
  invisible(path)
}

main <- function() {
  pilot <- "--pilot" %in% commandArgs(trailingOnly = TRUE)
  object <- read_hlao(pilot)
  plot_reach_performance(object, pilot)
  plot_runtime(object, pilot)
}

if (sys.nframe() == 0L) main()
