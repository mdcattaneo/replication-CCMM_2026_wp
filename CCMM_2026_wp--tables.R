################################################################################
# Attention Overload
# 2026 paper-table renderer
################################################################################

options(stringsAsFactors = FALSE)

script_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0L) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }
  script <- sub("^--file=", "", file_arg[[1L]])
  dirname(normalizePath(script, winslash = "/", mustWork = TRUE))
}

project_root <- script_root()
output_dir <- file.path(project_root, "output")
tables_dir <- file.path(project_root, "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

read_raw_results <- function() {
  paths <- list.files(
    output_dir,
    pattern = "^CCMM_2026_wp--raw-.*\\.rds$",
    full.names = TRUE
  )
  if (length(paths) == 0L) {
    stop("No raw simulation files found in ", output_dir, call. = FALSE)
  }
  lapply(paths, readRDS)
}

build_tables <- function(raw_results) {
  stop(
    "Paper-table builders are not implemented yet. Finalize the raw output ",
    "schema before adding LaTeX renderers.",
    call. = FALSE
  )
}

main <- function() {
  raw_results <- read_raw_results()
  build_tables(raw_results)
}

if (sys.nframe() == 0L) {
  main()
}
