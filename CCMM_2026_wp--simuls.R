################################################################################
# Attention Overload
# 2026 simulation driver
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
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

master_seed <- 20260713L
production_replications <- 2000L
critical_value_draws <- 2000L

build_aom_design <- function() {
  supports <- data.frame(
    support_id = sprintf("A%02d", 1:7),
    menu_support = c(
      "sizes-2-to-6", "sizes-3-to-6", "sizes-4-to-6", "sizes-5-to-6",
      "sizes-2-3-4-6", "sizes-2-3-6", "sizes-2-6"
    )
  )
  design <- merge(
    supports,
    data.frame(n_per_menu = c(50L, 100L, 200L)),
    all = TRUE
  )
  design$block <- "homogeneous-aom"
  design$universe_size <- 6L
  design$preference_design <- "homogeneous-four-ordering-size-power"
  design$stopping_design <- "logit-attention-alpha-2"
  design$reach_design <- "not-applicable"
  design$dependence_design <- "not-applicable"
  design$violation_design <- "published-baseline"
  design$design_id <- sprintf("AOM-%s-N%03d", design$support_id, design$n_per_menu)
  design$support_id <- NULL
  design
}

build_hlao_design <- function() {
  configurations <- data.frame(
    config_id = sprintf("H%02d", 1:10),
    universe_size = c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 6L, 6L),
    preference_design = c(
      "diffuse", "concentrated", "diffuse", "diffuse", "diffuse",
      "diffuse", "concentrated", "diffuse", "diffuse", "concentrated"
    ),
    stopping_design = c(
      "geometric", "geometric", "rank-dependent", "alternative-dependent",
      "geometric", "rank-dependent", "alternative-dependent",
      "rank-dependent", "geometric", "rank-dependent"
    ),
    reach_design = c(
      "high", "high", "high", "high", "low", "low", "low", "zero",
      "high", "low"
    ),
    menu_support = c(
      "full", "full", "prefix-rich", "prefix-rich", "full", "prefix-rich",
      "prefix-rich", "prefix-rich", "sparse-pairwise", "sparse-pairwise"
    ),
    dependence_design = c(
      rep("independent", 6), "dependent", "independent", "independent",
      "independent"
    ),
    violation_design = "none"
  )
  design <- merge(
    configurations,
    data.frame(n_per_menu = c(50L, 100L, 200L, 500L)),
    all = TRUE
  )
  design$block <- "hlao"
  design$design_id <- sprintf("HLAO-%s-N%03d", design$config_id, design$n_per_menu)
  design$config_id <- NULL
  design
}

build_diagnostic_design <- function() {
  design <- expand.grid(
    violation_design = c("recovered-list-attention", "block-marschak"),
    n_per_menu = c(100L, 200L),
    stringsAsFactors = FALSE
  )
  design$block <- "hlao-diagnostic"
  design$universe_size <- 4L
  design$preference_design <- "diffuse"
  design$stopping_design <- "geometric"
  design$reach_design <- "high"
  design$menu_support <- "prefix-rich"
  design$dependence_design <- "independent"
  design$design_id <- sprintf(
    "DIAG-%s-N%03d",
    ifelse(design$violation_design == "recovered-list-attention", "RLA", "BM"),
    design$n_per_menu
  )
  design
}

build_design <- function() {
  columns <- c(
    "design_id", "block", "universe_size", "n_per_menu",
    "preference_design", "stopping_design", "reach_design", "menu_support",
    "dependence_design", "violation_design"
  )
  design <- rbind(
    build_aom_design()[columns],
    build_hlao_design()[columns],
    build_diagnostic_design()[columns]
  )
  rownames(design) <- NULL
  design
}

run_simulations <- function(design, n_rep, n_crit, seed) {
  stop(
    "Simulation engines are not wired yet. Implement and test the required ",
    "ramchoice 3.0 interfaces before launching Monte Carlo runs.",
    call. = FALSE
  )
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  design <- build_design()
  design_path <- file.path(output_dir, "CCMM_2026_wp--design.csv")

  if ("--design-only" %in% args) {
    utils::write.csv(design, design_path, row.names = FALSE)
    message("Wrote simulation design: ", design_path)
    return(invisible(design))
  }

  if (!requireNamespace("ramchoice", quietly = TRUE)) {
    stop("Install the development version of ramchoice before running simulations.", call. = FALSE)
  }

  raw <- run_simulations(
    design = design,
    n_rep = production_replications,
    n_crit = critical_value_draws,
    seed = master_seed
  )

  saveRDS(
    list(
      design = design,
      results = raw,
      master_seed = master_seed,
      n_rep = production_replications,
      n_crit = critical_value_draws,
      ramchoice_version = as.character(utils::packageVersion("ramchoice")),
      session_info = utils::sessionInfo()
    ),
    file.path(output_dir, "CCMM_2026_wp--raw-all.rds")
  )
}

if (sys.nframe() == 0L) {
  main()
}
