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
pilot_replications <- 25L
pilot_critical_value_draws <- 200L

aom_preferences <- matrix(
  c(
    1, 2, 3, 4, 5, 6,
    2, 3, 4, 5, 6, 1,
    1, 2, 6, 5, 4, 3,
    1, 6, 5, 4, 3, 2
  ),
  ncol = 6,
  byrow = TRUE
)

aom_menu_sizes <- list(
  "sizes-2-to-6" = 6:2,
  "sizes-3-to-6" = 6:3,
  "sizes-4-to-6" = 6:4,
  "sizes-5-to-6" = 6:5,
  "sizes-2-3-4-6" = c(6, 4:2),
  "sizes-2-3-6" = c(6, 3, 2),
  "sizes-2-6" = c(6, 2)
)

build_aom_design <- function() {
  supports <- data.frame(
    support_id = sprintf("A%02d", seq_along(aom_menu_sizes)),
    menu_support = names(aom_menu_sizes)
  )
  design <- merge(
    supports,
    data.frame(n_per_menu = c(50L, 100L, 200L)),
    all = TRUE
  )
  design <- design[order(design$support_id, design$n_per_menu), ]
  design$block <- "homogeneous-aom"
  design$universe_size <- 6L
  design$preference_design <- "homogeneous-four-ordering-size-power"
  design$stopping_design <- "logit-attention-alpha-2"
  design$reach_design <- "not-applicable"
  design$dependence_design <- "not-applicable"
  design$violation_design <- "published-baseline"
  design$design_id <- sprintf("AOM-%s-N%03d", design$support_id, design$n_per_menu)
  design$support_id <- NULL
  rownames(design) <- NULL
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

argument_value <- function(args, prefix, default = NULL) {
  value <- grep(paste0("^", prefix), args, value = TRUE)
  if (length(value) == 0L) {
    return(default)
  }
  if (length(value) > 1L) {
    stop("Argument supplied more than once: ", prefix, call. = FALSE)
  }
  sub(paste0("^", prefix), "", value)
}

positive_integer_argument <- function(args, prefix, default) {
  value <- argument_value(args, prefix, default = as.character(default))
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed) || parsed <= 0L) {
    stop(prefix, " must be a positive integer.", call. = FALSE)
  }
  parsed
}

parse_run_options <- function(args) {
  pilot <- "--pilot" %in% args
  default_replications <- if (pilot) pilot_replications else production_replications
  default_critical_draws <- if (pilot) {
    pilot_critical_value_draws
  } else {
    critical_value_draws
  }
  block <- argument_value(args, "--block=", default = "homogeneous-aom")
  if (!(block %in% c("homogeneous-aom", "hlao", "hlao-diagnostic", "all"))) {
    stop("Unknown simulation block: ", block, call. = FALSE)
  }

  list(
    design_only = "--design-only" %in% args,
    pilot = pilot,
    block = block,
    n_rep = positive_integer_argument(
      args,
      "--replications=",
      default_replications
    ),
    n_crit = positive_integer_argument(
      args,
      "--critical-draws=",
      default_critical_draws
    )
  )
}

build_aom_population <- function(menu_sizes, universe_size = 6L, alpha = 2) {
  pieces <- lapply(menu_sizes, function(menu_size) {
    alternatives <- t(utils::combn(universe_size, menu_size))
    menu <- prob <- matrix(
      0,
      nrow = nrow(alternatives),
      ncol = universe_size
    )
    choice_probability <- ramchoice::logitAtte(menu_size, alpha)$choiceProb
    for (index in seq_len(nrow(alternatives))) {
      menu[index, alternatives[index, ]] <- 1
      prob[index, alternatives[index, ]] <- choice_probability
    }
    list(menu = menu, prob = prob)
  })

  list(
    menu = do.call(rbind, lapply(pieces, `[[`, "menu")),
    prob = do.call(rbind, lapply(pieces, `[[`, "prob"))
  )
}

simulate_aom_sample <- function(n_per_menu, menu_sizes,
                                universe_size = 6L, alpha = 2) {
  pieces <- lapply(menu_sizes, function(menu_size) {
    ramchoice::logitSimu(
      n = n_per_menu,
      uSize = universe_size,
      mSize = menu_size,
      a = alpha
    )
  })
  list(
    menu = do.call(rbind, lapply(pieces, `[[`, "menu")),
    choice = do.call(rbind, lapply(pieces, `[[`, "choice"))
  )
}

replication_seed <- function(seed, design_index, replication) {
  candidate <- as.double(seed) + 100000 * design_index + replication
  as.integer(candidate %% .Machine$integer.max)
}

run_aom_simulations <- function(design, n_rep, n_crit, seed) {
  result <- vector("list", nrow(design) * n_rep)
  result_index <- 0L
  population_cache <- list()

  for (design_index in seq_len(nrow(design))) {
    row <- design[design_index, ]
    menu_sizes <- aom_menu_sizes[[row$menu_support]]
    if (is.null(menu_sizes)) {
      stop("Unknown homogeneous-AOM menu support: ", row$menu_support, call. = FALSE)
    }

    if (is.null(population_cache[[row$menu_support]])) {
      population <- build_aom_population(menu_sizes)
      population_cache[[row$menu_support]] <- ramchoice::aomModel(
        population$menu,
        population$prob,
        pref_list = aom_preferences
      )$results
    }
    population_results <- population_cache[[row$menu_support]]

    for (replication in seq_len(n_rep)) {
      seed_replication <- replication_seed(seed, design_index, replication)
      set.seed(seed_replication)
      sample <- simulate_aom_sample(row$n_per_menu, menu_sizes)
      fit <- ramchoice::aomTest(
        sample$menu,
        sample$choice,
        pref_list = aom_preferences,
        method = "GMS",
        alpha = 0.05,
        nCritSimu = n_crit
      )

      inference <- fit$results
      truth_index <- match(
        inference$preference_id,
        population_results$preference_id
      )
      result_index <- result_index + 1L
      result[[result_index]] <- data.frame(
        design_id = row$design_id,
        block = row$block,
        replication = replication,
        replication_seed = seed_replication,
        universe_size = row$universe_size,
        n_per_menu = row$n_per_menu,
        menu_support = row$menu_support,
        preference_id = inference$preference_id,
        preference = inference$preference,
        is_null = population_results$compatible[truth_index],
        population_n_violated = population_results$n_violated[truth_index],
        population_max_inequality = population_results$max_inequality[truth_index],
        population_max_violation = population_results$max_violation[truth_index],
        method = inference$method,
        alpha = inference$alpha,
        n_critical_draws = n_crit,
        statistic = inference$statistic,
        critical_value = inference$critical_value,
        p_value = inference$p_value,
        reject = inference$reject,
        n_inequalities = inference$n_inequalities,
        n_positive_sample_inequalities = inference$n_positive_sample_inequalities,
        max_sample_inequality = inference$max_sample_inequality,
        elapsed_seconds = fit$elapsed,
        stringsAsFactors = FALSE
      )
    }

    message(
      "Completed ", row$design_id, " (", design_index, "/", nrow(design), ")"
    )
  }

  do.call(rbind, result[seq_len(result_index)])
}

run_simulations <- function(design, block, n_rep, n_crit, seed) {
  if (block != "homogeneous-aom") {
    stop(
      "Only the homogeneous-aom engine is implemented. The H-LAO blocks remain design-only.",
      call. = FALSE
    )
  }
  selected <- design[design$block == block, , drop = FALSE]
  run_aom_simulations(selected, n_rep = n_rep, n_crit = n_crit, seed = seed)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  options <- parse_run_options(args)
  design <- build_design()
  design_path <- file.path(output_dir, "CCMM_2026_wp--design.csv")

  if (options$design_only) {
    utils::write.csv(design, design_path, row.names = FALSE)
    message("Wrote simulation design: ", design_path)
    return(invisible(design))
  }

  if (!requireNamespace("ramchoice", quietly = TRUE)) {
    stop("Install the development version of ramchoice before running simulations.", call. = FALSE)
  }

  started_at <- Sys.time()
  started_elapsed <- proc.time()[["elapsed"]]
  raw <- run_simulations(
    design = design,
    block = options$block,
    n_rep = options$n_rep,
    n_crit = options$n_crit,
    seed = master_seed
  )
  ended_at <- Sys.time()

  suffix <- if (options$pilot) "--pilot" else ""
  output_path <- file.path(
    output_dir,
    paste0("CCMM_2026_wp--raw-", options$block, suffix, ".rds")
  )
  selected_design <- design[design$block == options$block, , drop = FALSE]
  saveRDS(
    list(
      design = selected_design,
      results = raw,
      master_seed = master_seed,
      n_rep = options$n_rep,
      n_crit = options$n_crit,
      pilot = options$pilot,
      block = options$block,
      started_at = started_at,
      ended_at = ended_at,
      elapsed_seconds = unname(proc.time()[["elapsed"]] - started_elapsed),
      ramchoice_version = as.character(utils::packageVersion("ramchoice")),
      ramchoice_git_sha = Sys.getenv("RAMCHOICE_GIT_SHA", unset = NA_character_),
      session_info = utils::sessionInfo()
    ),
    output_path
  )
  message("Wrote raw simulation results: ", output_path)
  invisible(raw)
}

if (sys.nframe() == 0L) {
  main()
}
