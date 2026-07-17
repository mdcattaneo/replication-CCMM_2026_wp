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
    data <- data[
      data$estimand_type == "pairwise-share" &
        !grepl("^studentized", data$method),
      ,
      drop = FALSE
    ]
    if (!(config %in% configurations)) next
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
  plot_runtime(object, pilot)
}

if (sys.nframe() == 0L) main()
