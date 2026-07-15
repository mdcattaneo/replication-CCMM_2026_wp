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
simulation_schema_version <- "2026-07-15-v4"
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
  design$path_independence_design <- "not-applicable"
  design$violation_design <- "published-baseline"
  design$violation_severity <- "not-applicable"
  design$design_id <- sprintf("AOM-%s-N%03d", design$support_id, design$n_per_menu)
  design$support_id <- NULL
  rownames(design) <- NULL
  design
}

build_hlao_design <- function() {
  configurations <- data.frame(
    config_id = sprintf("H%02d", 1:11),
    universe_size = c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 6L, 6L, 4L),
    preference_design = c(
      "diffuse", "concentrated", "diffuse", "diffuse", "diffuse",
      "diffuse", "concentrated", "diffuse", "diffuse", "concentrated",
      "late-concentrated"
    ),
    stopping_design = c(
      "geometric", "geometric", "rank-dependent", "alternative-dependent",
      "geometric", "rank-dependent", "alternative-dependent",
      "rank-dependent", "geometric", "rank-dependent", "menu-dependent"
    ),
    reach_design = c(
      "high", "high", "high", "high", "low", "low", "low", "zero",
      "high", "low", "moderate"
    ),
    menu_support = c(
      "full", "full", "prefix-rich", "prefix-rich", "full", "prefix-rich",
      "prefix-rich", "prefix-rich", "sparse-pairwise", "sparse-pairwise",
      "full"
    ),
    dependence_design = c(
      rep("independent", 6), "dependent", "independent", "independent",
      "independent", "independent"
    ),
    path_independence_design = c(rep("satisfied", 10), "violated"),
    violation_design = "none",
    violation_severity = "not-applicable"
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
  configurations <- data.frame(
    diagnostic_id = c("NULL", "RLA-L", "RLA-F", "BM-L", "BM-F"),
    violation_design = c(
      "none", "recovered-list-attention", "recovered-list-attention",
      "block-marschak", "block-marschak"
    ),
    violation_severity = c("null", "local", "fixed", "local", "fixed"),
    stringsAsFactors = FALSE
  )
  design <- merge(
    configurations,
    data.frame(n_per_menu = c(100L, 200L, 500L)),
    all = TRUE
  )
  design$block <- "hlao-diagnostic"
  design$universe_size <- 4L
  design$preference_design <- "bm-boundary"
  design$stopping_design <- "geometric"
  design$reach_design <- "high"
  design$menu_support <- "full"
  design$dependence_design <- "independent"
  design$path_independence_design <- "satisfied"
  design$design_id <- sprintf(
    "DIAG-%s-N%03d",
    design$diagnostic_id,
    design$n_per_menu
  )
  design$diagnostic_id <- NULL
  design
}

build_design <- function() {
  columns <- c(
    "design_id", "block", "universe_size", "n_per_menu",
    "preference_design", "stopping_design", "reach_design", "menu_support",
    "dependence_design", "path_independence_design", "violation_design",
    "violation_severity"
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

optional_positive_integer_argument <- function(args, prefix) {
  value <- argument_value(args, prefix, default = NULL)
  if (is.null(value)) {
    return(NULL)
  }
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

  array_task <- optional_positive_integer_argument(args, "--array-task=")
  design_id <- argument_value(args, "--design-id=", default = NULL)
  checkpoint_only <- "--checkpoint-only" %in% args
  assemble_only <- "--assemble-only" %in% args
  design_only <- "--design-only" %in% args

  if (!is.null(array_task) && !is.null(design_id)) {
    stop("Use either --array-task or --design-id, not both.", call. = FALSE)
  }
  if (checkpoint_only && assemble_only) {
    stop("--checkpoint-only and --assemble-only are mutually exclusive.", call. = FALSE)
  }
  if (checkpoint_only && is.null(array_task) && is.null(design_id)) {
    stop(
      "--checkpoint-only requires --array-task or --design-id.",
      call. = FALSE
    )
  }
  if ((!is.null(array_task) || !is.null(design_id)) && !checkpoint_only) {
    stop(
      "--array-task and --design-id may be used only with --checkpoint-only.",
      call. = FALSE
    )
  }
  if (assemble_only && block == "all") {
    stop("Assemble one simulation block at a time.", call. = FALSE)
  }
  if (design_only && (checkpoint_only || assemble_only)) {
    stop("--design-only cannot be combined with a run mode.", call. = FALSE)
  }

  list(
    design_only = design_only,
    pilot = pilot,
    block = block,
    array_task = array_task,
    design_id = design_id,
    checkpoint_only = checkpoint_only,
    assemble_only = assemble_only,
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

design_seed_index <- function(row, fallback) {
  if ("simulation_index" %in% names(row)) {
    return(as.integer(row$simulation_index[[1L]]))
  }
  as.integer(fallback)
}

run_aom_simulations <- function(design, n_rep, n_crit, seed) {
  result <- vector("list", nrow(design) * n_rep)
  result_index <- 0L
  population_cache <- list()

  for (design_index in seq_len(nrow(design))) {
    row <- design[design_index, ]
    seed_index <- design_seed_index(row, design_index)
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
      seed_replication <- replication_seed(seed, seed_index, replication)
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

hlao_menu_key <- function(items) {
  paste(items, collapse = "-")
}

hlao_suffix_close <- function(menus) {
  result <- menus
  for (items in menus) {
    for (position in seq_along(items)) {
      suffix <- items[position:length(items)]
      result[[hlao_menu_key(suffix)]] <- suffix
    }
  }
  code <- vapply(result, function(items) sum(2^(items - 1L)), numeric(1L))
  result[order(code)]
}

hlao_menu_support <- function(universe_size, support) {
  if (support == "full") {
    menus <- list()
    for (size in seq_len(universe_size)) {
      combinations <- utils::combn(universe_size, size)
      for (index in seq_len(ncol(combinations))) {
        items <- combinations[, index]
        menus[[hlao_menu_key(items)]] <- items
      }
    }
    return(hlao_suffix_close(menus))
  }
  if (support == "prefix-rich") {
    if (universe_size != 4L) {
      stop("The prefix-rich design is calibrated for four alternatives.", call. = FALSE)
    }
    menus <- list(
      c(1L), c(2L), c(3L), c(4L), c(1L, 2L), c(1L, 3L),
      c(2L, 4L), c(1L, 2L, 4L), c(1L, 3L, 4L)
    )
    names(menus) <- vapply(menus, hlao_menu_key, character(1L))
    return(hlao_suffix_close(menus))
  }
  if (support == "sparse-pairwise") {
    menus <- lapply(seq_len(universe_size), function(item) item)
    pairs <- utils::combn(universe_size, 2L, simplify = FALSE)
    menus <- c(menus, pairs)
    names(menus) <- vapply(menus, hlao_menu_key, character(1L))
    return(hlao_suffix_close(menus))
  }
  stop("Unknown H-LAO menu support: ", support, call. = FALSE)
}

hlao_bm_boundary_event <- function(rankings) {
  if (ncol(rankings) != 4L) {
    stop("The BM-boundary preference design requires four alternatives.", call. = FALSE)
  }
  apply(rankings, 1L, function(ranking) {
    position <- match(seq_len(4L), ranking)
    max(position[c(1L, 4L)]) < position[2L] &&
      position[2L] < position[3L]
  })
}

hlao_preference_distribution <- function(universe_size, design) {
  rankings <- ramchoice::hlaoRankings(seq_len(universe_size))
  diffuse_weights <- ((seq_len(nrow(rankings)) * 37L) %% 101L) + 1L
  diffuse <- diffuse_weights / sum(diffuse_weights)
  if (design == "diffuse") {
    tau <- diffuse
  } else if (design == "concentrated") {
    tau <- 0.25 * diffuse
    baseline <- which(apply(rankings, 1L, function(ranking) {
      identical(as.integer(ranking), seq_len(universe_size))
    }))
    tau[baseline] <- tau[baseline] + 0.75
  } else if (design == "late-concentrated") {
    tau <- 0.05 * diffuse
    late_best <- which(apply(rankings, 1L, function(ranking) {
      identical(as.integer(ranking), rev(seq_len(universe_size)))
    }))
    tau[late_best] <- tau[late_best] + 0.95
  } else if (design == "degenerate") {
    tau <- numeric(nrow(rankings))
    baseline <- which(apply(rankings, 1L, function(ranking) {
      identical(as.integer(ranking), seq_len(universe_size))
    }))
    tau[baseline] <- 1
  } else if (design == "cyclic") {
    if (universe_size != 4L) {
      stop("The cyclic preference design requires four alternatives.", call. = FALSE)
    }
    support <- rbind(
      c(1L, 2L, 3L, 4L),
      c(2L, 3L, 4L, 1L),
      c(3L, 4L, 1L, 2L),
      c(4L, 1L, 2L, 3L)
    )
    tau <- numeric(nrow(rankings))
    for (ranking in seq_len(nrow(support))) {
      selected <- which(apply(rankings, 1L, function(candidate) {
        identical(as.integer(candidate), support[ranking, ])
      }))
      tau[selected] <- 1 / nrow(support)
    }
  } else if (design == "bm-boundary") {
    excluded <- hlao_bm_boundary_event(rankings)
    tau <- as.numeric(!excluded)
    tau <- tau / sum(tau)
  } else {
    stop("Unknown H-LAO preference design: ", design, call. = FALSE)
  }
  list(rankings = rankings, tau = tau / sum(tau))
}

hlao_continuation <- function(items, stopping_design, reach_design,
                              universe_size) {
  remaining <- length(items) - seq_along(items)
  if (stopping_design == "geometric") {
    probability <- switch(
      reach_design,
      high = 0.90,
      moderate = 0.75,
      low = 0.50,
      stop("Unknown geometric reach design: ", reach_design, call. = FALSE)
    )
    return(rep(probability, length(items)))
  }
  if (stopping_design == "rank-dependent") {
    if (reach_design == "high") {
      return(pmax(0.65, 0.95 - 0.06 * remaining))
    }
    if (reach_design == "low") {
      return(pmax(0.12, 0.65 - 0.13 * remaining))
    }
    if (reach_design == "zero") {
      return(ifelse(remaining >= 1L, 0, 0.80))
    }
  }
  if (stopping_design == "alternative-dependent") {
    probabilities <- if (reach_design == "high") {
      seq(0.94, 0.76, length.out = universe_size)
    } else {
      seq(0.68, 0.28, length.out = universe_size)
    }
    return(probabilities[items])
  }
  stop(
    "Unsupported H-LAO stopping/reach combination: ",
    stopping_design, "/", reach_design,
    call. = FALSE
  )
}

hlao_prefix_masses <- function(continuation) {
  reach <- cumprod(continuation)
  masses <- c(
    1 - reach[1L],
    if (length(reach) > 1L) {
      reach[-length(reach)] - reach[-1L]
    } else {
      numeric(0L)
    },
    reach[length(reach)]
  )
  list(reach = reach, masses = masses)
}

hlao_menu_dependent_attention <- function(items, universe_size) {
  alternative_reach <- seq(0.92, 0.62, length.out = universe_size)
  menu_scale <- max(0.55, 1 - 0.10 * (length(items) - 1L))
  reach <- alternative_reach[items] * menu_scale
  masses <- c(
    1 - reach[1L],
    if (length(reach) > 1L) {
      reach[-length(reach)] - reach[-1L]
    } else {
      numeric(0L)
    },
    reach[length(reach)]
  )
  list(reach = reach, masses = masses)
}

hlao_couple_margins <- function(row_margins, column_margins, row_order) {
  rows <- row_margins[row_order]
  columns <- column_margins
  coupling <- matrix(0, nrow = length(rows), ncol = length(columns))
  row_index <- column_index <- 1L
  tolerance <- 1e-14
  while (row_index <= length(rows) && column_index <= length(columns)) {
    amount <- min(rows[row_index], columns[column_index])
    coupling[row_index, column_index] <-
      coupling[row_index, column_index] + amount
    rows[row_index] <- rows[row_index] - amount
    columns[column_index] <- columns[column_index] - amount
    if (rows[row_index] <= tolerance) row_index <- row_index + 1L
    if (columns[column_index] <= tolerance) column_index <- column_index + 1L
  }
  result <- matrix(0, nrow = length(row_margins), ncol = length(column_margins))
  result[row_order, ] <- coupling
  result
}

hlao_best_in_prefix <- function(rankings, prefix) {
  apply(rankings, 1L, function(ranking) ranking[match(TRUE, ranking %in% prefix)])
}

build_hlao_population <- function(design_row) {
  universe_size <- as.integer(design_row$universe_size)
  menus <- hlao_menu_support(universe_size, design_row$menu_support)
  preferences <- hlao_preference_distribution(
    universe_size,
    design_row$preference_design
  )
  rankings <- preferences$rankings
  tau <- preferences$tau
  menu <- prob <- matrix(0, nrow = length(menus), ncol = universe_size)
  outside_prob <- numeric(length(menus))
  reach <- masses <- vector("list", length(menus))

  for (menu_index in seq_along(menus)) {
    items <- menus[[menu_index]]
    menu[menu_index, items] <- 1
    attention <- if (design_row$stopping_design == "menu-dependent") {
      hlao_menu_dependent_attention(items, universe_size)
    } else {
      hlao_prefix_masses(hlao_continuation(
        items,
        design_row$stopping_design,
        design_row$reach_design,
        universe_size
      ))
    }
    reach[[menu_index]] <- attention$reach
    masses[[menu_index]] <- attention$masses
    outside_prob[menu_index] <- attention$masses[1L]

    if (design_row$dependence_design == "independent") {
      coupling <- outer(tau, attention$masses)
    } else {
      last_item <- items[length(items)]
      preference_position <- apply(rankings, 1L, function(ranking) {
        match(last_item, ranking)
      })
      coupling <- hlao_couple_margins(
        tau,
        attention$masses,
        order(-preference_position)
      )
    }
    for (prefix_size in seq_along(items)) {
      winners <- hlao_best_in_prefix(
        rankings,
        items[seq_len(prefix_size)]
      )
      for (alternative in items) {
        selected <- winners == alternative
        prob[menu_index, alternative] <- prob[menu_index, alternative] +
          sum(coupling[selected, prefix_size + 1L])
      }
    }
  }

  event <- apply(rankings, 1L, function(ranking) {
    match(universe_size, ranking) < match(1L, ranking)
  })
  list(
    menu = menu,
    prob = prob,
    outside_prob = outside_prob,
    menus = menus,
    reach = reach,
    masses = masses,
    rankings = rankings,
    tau = tau,
    event = event,
    event_name = paste0(universe_size, " above 1"),
    event_truth = sum(tau[event])
  )
}

hlao_full_rule <- function(population) {
  full <- matrix(
    0,
    nrow = nrow(population$menu),
    ncol = ncol(population$menu)
  )
  for (menu_index in seq_along(population$menus)) {
    items <- population$menus[[menu_index]]
    winners <- hlao_best_in_prefix(population$rankings, items)
    for (alternative in items) {
      full[menu_index, alternative] <- sum(
        population$tau[winners == alternative]
      )
    }
  }
  full
}

hlao_prob_from_full <- function(population, full) {
  prob <- matrix(
    0,
    nrow = nrow(population$menu),
    ncol = ncol(population$menu)
  )
  keys <- vapply(population$menus, hlao_menu_key, character(1L))
  for (menu_index in seq_along(population$menus)) {
    items <- population$menus[[menu_index]]
    for (prefix_size in seq_along(items)) {
      prefix_index <- match(
        hlao_menu_key(items[seq_len(prefix_size)]),
        keys
      )
      prob[menu_index, ] <- prob[menu_index, ] +
        population$masses[[menu_index]][prefix_size + 1L] *
        full[prefix_index, ]
    }
  }
  prob
}

hlao_diagnostic_magnitude <- function(design_row) {
  if (design_row$violation_severity == "null") {
    return(0)
  }
  if (design_row$violation_severity == "local") {
    coefficient <- if (design_row$violation_design == "block-marschak") 3 else 0.75
    return(coefficient / sqrt(design_row$n_per_menu))
  }
  if (design_row$violation_severity == "fixed") {
    return(if (design_row$violation_design == "block-marschak") 0.30 else 0.08)
  }
  stop(
    "Unknown diagnostic violation severity: ",
    design_row$violation_severity,
    call. = FALSE
  )
}

build_hlao_diagnostic_population <- function(design_row) {
  population <- build_hlao_population(design_row)
  magnitude <- hlao_diagnostic_magnitude(design_row)
  keys <- vapply(population$menus, hlao_menu_key, character(1L))

  if (design_row$violation_design == "recovered-list-attention") {
    target <- match("1-2", keys)
    population$outside_prob[target] <-
      population$outside_prob[target] - magnitude
    conditional_inside <- population$prob[target, ] /
      sum(population$prob[target, ])
    population$prob[target, ] <-
      (1 - population$outside_prob[target]) * conditional_inside
  } else if (design_row$violation_design == "block-marschak") {
    excluded <- hlao_bm_boundary_event(population$rankings)
    signed_tau <- population$tau
    signed_tau[excluded] <- -magnitude / sum(excluded)
    signed_tau[!excluded] <- signed_tau[!excluded] +
      magnitude / sum(!excluded)
    signed_population <- population
    signed_population$tau <- signed_tau
    full <- hlao_full_rule(signed_population)
    population$prob <- hlao_prob_from_full(population, full)
  } else if (design_row$violation_design != "none") {
    stop(
      "Unknown H-LAO diagnostic violation: ",
      design_row$violation_design,
      call. = FALSE
    )
  }

  population$analysis <- ramchoice::hlaoModel(
    population$menu,
    population$prob,
    outside_prob = population$outside_prob,
    dependence = "both"
  )
  population$violation_magnitude <- magnitude
  population$attention_violation <-
    population$analysis$attention_diagnostics$max_attention_overload_violation
  population$bm_violation <- if (is.null(population$analysis$block_marschak)) {
    NA_real_
  } else {
    max(0, -population$analysis$block_marschak$minimum)
  }
  population
}

simulate_hlao_sample <- function(population, n_per_menu) {
  universe_size <- ncol(population$menu)
  menu <- choice <- matrix(
    0,
    nrow = nrow(population$menu) * n_per_menu,
    ncol = universe_size
  )
  for (menu_index in seq_len(nrow(population$menu))) {
    rows <- (menu_index - 1L) * n_per_menu + seq_len(n_per_menu)
    menu[rows, ] <- population$menu[rep(menu_index, n_per_menu), , drop = FALSE]
    probabilities <- c(
      population$outside_prob[menu_index],
      population$prob[menu_index, ]
    )
    draws <- max.col(
      t(stats::rmultinom(n_per_menu, 1L, probabilities)),
      ties.method = "first"
    )
    inside <- which(draws > 1L)
    if (length(inside)) {
      choice[cbind(rows[inside], draws[inside] - 1L)] <- 1
    }
  }
  list(menu = menu, choice = choice)
}

hlao_pairwise_truth <- function(rankings, tau, earlier, later) {
  selected <- apply(rankings, 1L, function(ranking) {
    match(later, ranking) < match(earlier, ranking)
  })
  sum(tau[selected])
}

hlao_result_row <- function(design_row, replication, replication_seed,
                            estimand_type, estimand_id, truth = NA_real_,
                            estimate = NA_real_, lower = NA_real_,
                            upper = NA_real_, identified = NA,
                            target_valid = TRUE, population_lower = NA_real_,
                            population_upper = NA_real_, method = NA_character_,
                            alpha = NA_real_, menu_id = NA_integer_,
                            alternative = NA_integer_, earlier = NA_integer_,
                            later = NA_integer_, elapsed_seconds = NA_real_,
                            population_independent_compatible = NA,
                            population_robust_compatible = NA,
                            population_nopi_compatible = NA,
                            population_independent_lower = NA_real_,
                            population_independent_upper = NA_real_,
                            population_robust_lower = NA_real_,
                            population_robust_upper = NA_real_,
                            population_nopi_lower = NA_real_,
                            population_nopi_upper = NA_real_,
                            covered_override = NULL, width_override = NULL,
                            n_components = NA_integer_) {
  covered <- if (!is.null(covered_override)) {
    covered_override
  } else if (target_valid && is.finite(truth) && is.finite(lower) &&
             is.finite(upper)) {
    lower <= truth && truth <= upper
  } else {
    NA
  }
  data.frame(
    design_id = design_row$design_id,
    block = design_row$block,
    replication = replication,
    replication_seed = replication_seed,
    universe_size = design_row$universe_size,
    n_per_menu = design_row$n_per_menu,
    menu_support = design_row$menu_support,
    preference_design = design_row$preference_design,
    stopping_design = design_row$stopping_design,
    reach_design = design_row$reach_design,
    dependence_design = design_row$dependence_design,
    path_independence_design = design_row$path_independence_design,
    estimand_type = estimand_type,
    estimand_id = estimand_id,
    menu_id = menu_id,
    alternative = alternative,
    earlier = earlier,
    later = later,
    truth = truth,
    estimate = estimate,
    error = if (is.finite(truth) && is.finite(estimate)) estimate - truth else NA_real_,
    squared_error = if (is.finite(truth) && is.finite(estimate)) {
      (estimate - truth)^2
    } else {
      NA_real_
    },
    lower = lower,
    upper = upper,
    covered = covered,
    width = if (!is.null(width_override)) {
      width_override
    } else if (is.finite(lower) && is.finite(upper)) {
      upper - lower
    } else {
      NA_real_
    },
    n_components = n_components,
    identified = identified,
    target_valid = target_valid,
    population_lower = population_lower,
    population_upper = population_upper,
    population_independent_compatible = population_independent_compatible,
    population_robust_compatible = population_robust_compatible,
    population_nopi_compatible = population_nopi_compatible,
    population_independent_lower = population_independent_lower,
    population_independent_upper = population_independent_upper,
    population_robust_lower = population_robust_lower,
    population_robust_upper = population_robust_upper,
    population_nopi_lower = population_nopi_lower,
    population_nopi_upper = population_nopi_upper,
    method = method,
    alpha = alpha,
    elapsed_seconds = elapsed_seconds,
    stringsAsFactors = FALSE
  )
}

run_hlao_simulations <- function(design, n_rep, n_crit, seed) {
  result <- list()
  result_index <- 0L
  population_cache <- list()

  for (design_index in seq_len(nrow(design))) {
    row <- design[design_index, ]
    seed_index <- design_seed_index(row, design_index)
    config_id <- sub("-N[0-9]+$", "", row$design_id)
    if (is.null(population_cache[[config_id]])) {
      population <- build_hlao_population(row)
      event_argument <- if (row$universe_size == 4L) {
        setNames(list(population$event), population$event_name)
      } else {
        NULL
      }
      population$analysis <- ramchoice::hlaoModel(
        population$menu,
        population$prob,
        outside_prob = population$outside_prob,
        events = event_argument,
        dependence = if (row$universe_size == 4L) "all" else "independent"
      )
      population_cache[[config_id]] <- population
    }
    population <- population_cache[[config_id]]
    compatibility <- population$analysis$compatibility
    independent_compatible <- compatibility$compatible[
      match("independent", compatibility$mode)
    ]
    robust_compatible <- compatibility$compatible[
      match("robust", compatibility$mode)
    ]
    nopi_compatible <- compatibility$compatible[
      match("noPI", compatibility$mode)
    ]
    if (!length(robust_compatible)) robust_compatible <- NA
    if (!length(nopi_compatible)) nopi_compatible <- NA
    independent_bounds <- population$analysis$bounds[
      population$analysis$bounds$mode == "independent", , drop = FALSE
    ]
    robust_bounds <- population$analysis$bounds[
      population$analysis$bounds$mode == "robust", , drop = FALSE
    ]
    nopi_bounds <- population$analysis$bounds[
      population$analysis$bounds$mode == "noPI", , drop = FALSE
    ]

    for (replication in seq_len(n_rep)) {
      seed_replication <- replication_seed(
        seed,
        1000L + seed_index,
        replication
      )
      set.seed(seed_replication)
      sample <- simulate_hlao_sample(population, row$n_per_menu)
      event_argument <- if (row$universe_size == 4L) {
        setNames(list(population$event), population$event_name)
      } else {
        NULL
      }
      fit_hoeffding <- ramchoice::hlaoTest(
        sample$menu,
        sample$choice,
        events = event_argument,
        alpha = 0.05,
        band_method = "hoeffding"
      )
      set.seed(replication_seed(
        seed,
        2000L + seed_index,
        replication
      ))
      fit_gaussian <- ramchoice::hlaoTest(
        sample$menu,
        sample$choice,
        events = event_argument,
        alpha = 0.05,
        band_method = "gaussian",
        n_band_draws = n_crit
      )
      nopi_fits <- list()
      config_label <- sub("^HLAO-", "", config_id)
      if (!is.null(event_argument) &&
          config_label %in% c("H01", "H05", "H07", "H11")) {
        fit_nopi_hoeffding <- ramchoice::hlaoNoPITest(
          sample$menu,
          sample$choice,
          events = event_argument,
          alpha = 0.05,
          band_method = "hoeffding"
        )
        set.seed(replication_seed(
          seed,
          2500L + seed_index,
          replication
        ))
        fit_nopi_gaussian <- ramchoice::hlaoNoPITest(
          sample$menu,
          sample$choice,
          events = event_argument,
          alpha = 0.05,
          band_method = "gaussian",
          n_band_draws = n_crit
        )
        nopi_fits <- list(
          hoeffding = fit_nopi_hoeffding,
          gaussian = fit_nopi_gaussian
        )
      }
      fits <- list(hoeffding = fit_hoeffding, gaussian = fit_gaussian)
      fit <- fit_hoeffding

      for (attention_index in if (
        row$path_independence_design == "satisfied"
      ) seq_len(nrow(fit$attention)) else integer(0L)) {
        estimate_row <- fit$attention[attention_index, ]
        truth_row <- population$analysis$attention[
          population$analysis$attention$menu_id == estimate_row$menu_id &
            population$analysis$attention$alternative == estimate_row$alternative,
          ,
          drop = FALSE
        ]
        result_index <- result_index + 1L
        result[[result_index]] <- hlao_result_row(
          row, replication, seed_replication,
          estimand_type = "reach",
          estimand_id = paste0("reach-M", estimate_row$menu_id,
                               "-A", estimate_row$alternative),
          truth = truth_row$reach,
          estimate = estimate_row$reach,
          identified = TRUE,
          menu_id = estimate_row$menu_id,
          alternative = estimate_row$alternative,
          method = "plug-in",
          elapsed_seconds = fit$elapsed,
          population_independent_compatible = independent_compatible,
          population_robust_compatible = robust_compatible,
          population_nopi_compatible = nopi_compatible
        )
      }

      if (row$path_independence_design == "satisfied" &&
          !is.null(fit$full_attention) &&
          !is.null(population$analysis$full_attention)) {
        for (menu_index in seq_len(nrow(population$menu))) {
          for (alternative in which(population$menu[menu_index, ] == 1L)) {
            result_index <- result_index + 1L
            result[[result_index]] <- hlao_result_row(
              row, replication, seed_replication,
              estimand_type = "full-attention",
              estimand_id = paste0("f-M", menu_index, "-A", alternative),
              truth = population$analysis$full_attention$matrix[
                menu_index, alternative
              ],
              estimate = fit$full_attention$matrix[menu_index, alternative],
              identified = TRUE,
              menu_id = menu_index,
              alternative = alternative,
              method = "recursive-plug-in",
              elapsed_seconds = fit$elapsed,
              population_independent_compatible = independent_compatible,
              population_robust_compatible = robust_compatible,
              population_nopi_compatible = nopi_compatible
            )
          }
        }
      }

      for (fit in fits) {
        for (pair_index in seq_len(nrow(fit$pairwise))) {
          pair <- fit$pairwise[pair_index, ]
          truth <- hlao_pairwise_truth(
            population$rankings,
            population$tau,
            pair$earlier,
            pair$later
          )
          population_pair <- population$analysis$pairwise[
            population$analysis$pairwise$earlier == pair$earlier &
              population$analysis$pairwise$later == pair$later,
            ,
            drop = FALSE
          ]
          target_valid <- row$dependence_design == "independent" &&
            row$path_independence_design == "satisfied"
          result_index <- result_index + 1L
          result[[result_index]] <- hlao_result_row(
            row, replication, seed_replication,
            estimand_type = "pairwise-share",
            estimand_id = paste0(pair$later, "-above-", pair$earlier),
            truth = truth,
            estimate = pair$estimate,
            lower = pair$lower,
            upper = pair$upper,
            identified = population_pair$identified,
            target_valid = target_valid,
            earlier = pair$earlier,
            later = pair$later,
            method = pair$method,
            alpha = pair$alpha,
            elapsed_seconds = fit$elapsed,
            population_independent_compatible = independent_compatible,
            population_robust_compatible = robust_compatible,
            population_nopi_compatible = nopi_compatible
          )
        }

        if (nrow(fit$event_intervals)) {
          interval <- fit$event_intervals[1L, ]
          result_index <- result_index + 1L
          result[[result_index]] <- hlao_result_row(
            row, replication, seed_replication,
            estimand_type = "preference-event",
            estimand_id = population$event_name,
            truth = population$event_truth,
            lower = interval$lower,
            upper = interval$upper,
            identified = if (nrow(robust_bounds)) {
              abs(robust_bounds$upper - robust_bounds$lower) < 1e-10
            } else {
              NA
            },
            target_valid = row$path_independence_design == "satisfied",
            population_lower = if (nrow(robust_bounds)) robust_bounds$lower else NA_real_,
            population_upper = if (nrow(robust_bounds)) robust_bounds$upper else NA_real_,
            method = interval$method,
            alpha = interval$alpha,
            elapsed_seconds = fit$elapsed,
            population_independent_compatible = independent_compatible,
            population_robust_compatible = robust_compatible,
            population_nopi_compatible = nopi_compatible,
            population_independent_lower = if (nrow(independent_bounds)) {
              independent_bounds$lower
            } else {
              NA_real_
            },
            population_independent_upper = if (nrow(independent_bounds)) {
              independent_bounds$upper
            } else {
              NA_real_
            },
            population_robust_lower = if (nrow(robust_bounds)) {
              robust_bounds$lower
            } else {
              NA_real_
            },
            population_robust_upper = if (nrow(robust_bounds)) {
              robust_bounds$upper
            } else {
              NA_real_
            },
            population_nopi_lower = if (nrow(nopi_bounds)) {
              nopi_bounds$lower
            } else {
              NA_real_
            },
            population_nopi_upper = if (nrow(nopi_bounds)) {
              nopi_bounds$upper
            } else {
              NA_real_
            }
          )
        }
      }

      for (pair_index in seq_len(nrow(fit_hoeffding$pairwise_studentized))) {
        pair <- fit_hoeffding$pairwise_studentized[pair_index, ]
        truth <- hlao_pairwise_truth(
          population$rankings,
          population$tau,
          pair$earlier,
          pair$later
        )
        components <- fit_hoeffding$pairwise_studentized_components[
          fit_hoeffding$pairwise_studentized_components$menu_id == pair$menu_id,
          ,
          drop = FALSE
        ]
        target_valid <- row$dependence_design == "independent" &&
          row$path_independence_design == "satisfied"
        covered <- if (target_valid) {
          any(components$lower <= truth & components$upper >= truth)
        } else {
          NA
        }
        population_pair <- population$analysis$pairwise[
          population$analysis$pairwise$earlier == pair$earlier &
            population$analysis$pairwise$later == pair$later,
          ,
          drop = FALSE
        ]
        result_index <- result_index + 1L
        result[[result_index]] <- hlao_result_row(
          row, replication, seed_replication,
          estimand_type = "pairwise-share",
          estimand_id = paste0(pair$later, "-above-", pair$earlier),
          truth = truth,
          estimate = pair$estimate,
          lower = pair$lower,
          upper = pair$upper,
          identified = population_pair$identified,
          target_valid = target_valid,
          earlier = pair$earlier,
          later = pair$later,
          method = pair$method,
          alpha = pair$alpha,
          elapsed_seconds = fit_hoeffding$elapsed,
          population_independent_compatible = independent_compatible,
          population_robust_compatible = robust_compatible,
          population_nopi_compatible = nopi_compatible,
          covered_override = covered,
          width_override = pair$width,
          n_components = pair$n_components
        )
      }

      for (nopi_fit in nopi_fits) {
        if (!nrow(nopi_fit$intervals)) next
        interval <- nopi_fit$intervals[1L, ]
        result_index <- result_index + 1L
        result[[result_index]] <- hlao_result_row(
          row, replication, seed_replication,
          estimand_type = "preference-event-nopi",
          estimand_id = population$event_name,
          truth = population$event_truth,
          lower = interval$lower,
          upper = interval$upper,
          identified = if (nrow(nopi_bounds)) {
            abs(nopi_bounds$upper - nopi_bounds$lower) < 1e-10
          } else {
            NA
          },
          target_valid = TRUE,
          population_lower = if (nrow(nopi_bounds)) {
            nopi_bounds$lower
          } else {
            NA_real_
          },
          population_upper = if (nrow(nopi_bounds)) {
            nopi_bounds$upper
          } else {
            NA_real_
          },
          method = interval$method,
          alpha = interval$alpha,
          elapsed_seconds = nopi_fit$elapsed,
          population_independent_compatible = independent_compatible,
          population_robust_compatible = robust_compatible,
          population_nopi_compatible = nopi_compatible,
          population_independent_lower = if (nrow(independent_bounds)) {
            independent_bounds$lower
          } else {
            NA_real_
          },
          population_independent_upper = if (nrow(independent_bounds)) {
            independent_bounds$upper
          } else {
            NA_real_
          },
          population_robust_lower = if (nrow(robust_bounds)) {
            robust_bounds$lower
          } else {
            NA_real_
          },
          population_robust_upper = if (nrow(robust_bounds)) {
            robust_bounds$upper
          } else {
            NA_real_
          },
          population_nopi_lower = if (nrow(nopi_bounds)) {
            nopi_bounds$lower
          } else {
            NA_real_
          },
          population_nopi_upper = if (nrow(nopi_bounds)) {
            nopi_bounds$upper
          } else {
            NA_real_
          }
        )
      }
    }

    message(
      "Completed ", row$design_id, " (", design_index, "/", nrow(design), ")"
    )
  }
  do.call(rbind, result[seq_len(result_index)])
}

run_hlao_diagnostic_simulations <- function(design, n_rep, n_crit, seed) {
  result <- list()
  result_index <- 0L

  for (design_index in seq_len(nrow(design))) {
    row <- design[design_index, ]
    seed_index <- design_seed_index(row, design_index)
    population <- build_hlao_diagnostic_population(row)
    compatibility <- population$analysis$compatibility
    independent_compatible <- compatibility$compatible[
      match("independent", compatibility$mode)
    ]
    robust_compatible <- compatibility$compatible[
      match("robust", compatibility$mode)
    ]

    for (replication in seq_len(n_rep)) {
      seed_replication <- replication_seed(
        seed,
        3000L + seed_index,
        replication
      )
      set.seed(seed_replication)
      sample <- simulate_hlao_sample(population, row$n_per_menu)
      fit_hoeffding <- ramchoice::hlaoTest(
        sample$menu,
        sample$choice,
        alpha = 0.05,
        band_method = "hoeffding"
      )
      set.seed(replication_seed(
        seed,
        4000L + seed_index,
        replication
      ))
      fit_delta <- ramchoice::hlaoTest(
        sample$menu,
        sample$choice,
        alpha = 0.05,
        band_method = "gaussian",
        diagnostic_method = "delta",
        n_band_draws = n_crit
      )

      diagnostic_fits <- list(
        list(
          specification = fit_hoeffding$specification,
          fit = fit_hoeffding,
          diagnostic_method = "outer"
        ),
        list(
          specification = fit_delta$outer_specification,
          fit = fit_delta,
          diagnostic_method = "outer"
        ),
        list(
          specification = fit_delta$specification,
          fit = fit_delta,
          diagnostic_method = "delta"
        )
      )

      for (diagnostic_fit in diagnostic_fits) {
        fit <- diagnostic_fit$fit
        specification <- diagnostic_fit$specification
        for (diagnostic_index in seq_len(nrow(specification))) {
          diagnostic <- specification[diagnostic_index, ]
          population_violation <- switch(
            diagnostic$restriction,
            `attention-overload` = population$attention_violation,
            `full-attention-probability` = 0,
            `block-marschak` = population$bm_violation,
            omnibus = max(
              population$attention_violation,
              population$bm_violation,
              na.rm = TRUE
            )
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
            preference_design = row$preference_design,
            stopping_design = row$stopping_design,
            reach_design = row$reach_design,
            dependence_design = row$dependence_design,
            violation_design = row$violation_design,
            violation_severity = row$violation_severity,
            violation_magnitude = population$violation_magnitude,
            restriction = diagnostic$restriction,
            is_null = population_violation <= 1e-12,
            population_violation = population_violation,
            population_independent_compatible = independent_compatible,
            population_robust_compatible = robust_compatible,
            available = diagnostic$available,
            n_restrictions = diagnostic$n_restrictions,
            max_violation_estimate = diagnostic$max_violation_estimate,
            max_violation_lower = diagnostic$max_violation_lower,
            n_rejected = diagnostic$n_rejected,
            reject = diagnostic$reject,
            method = diagnostic$method,
            probability_band_method = fit$options$band_method,
            diagnostic_method = diagnostic_fit$diagnostic_method,
            diagnostic_critical_value = if (
              diagnostic_fit$diagnostic_method == "delta"
            ) fit$options$diagnostic_critical_value else NA_real_,
            alpha = diagnostic$alpha,
            n_critical_draws = if (grepl("gaussian", diagnostic$method)) {
              n_crit
            } else {
              NA_integer_
            },
            elapsed_seconds = fit$elapsed,
            stringsAsFactors = FALSE
          )
        }
      }
    }

    message(
      "Completed ", row$design_id, " (", design_index, "/", nrow(design), ")"
    )
  }
  do.call(rbind, result[seq_len(result_index)])
}

atomic_save_rds <- function(object, path) {
  temporary <- paste0(path, ".tmp-", Sys.getpid())
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(object, temporary)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically save checkpoint: ", path, call. = FALSE)
  }
  invisible(path)
}

checkpoint_token <- function(value) {
  gsub("[^A-Za-z0-9._-]", "_", value)
}

checkpoint_directory <- function(block, pilot, n_rep, n_crit) {
  ramchoice_sha <- Sys.getenv("RAMCHOICE_GIT_SHA", unset = "unknown")
  ramchoice_token <- substr(checkpoint_token(ramchoice_sha), 1L, 12L)
  run_type <- if (pilot) "pilot" else "production"
  file.path(
    output_dir,
    "checkpoints",
    paste(
      checkpoint_token(block),
      run_type,
      checkpoint_token(simulation_schema_version),
      paste0("R", n_rep),
      paste0("C", n_crit),
      paste0("rc-", ramchoice_token),
      sep = "--"
    )
  )
}

run_design_chunk <- function(row, block, n_rep, n_crit, seed) {
  if (block == "homogeneous-aom") {
    return(run_aom_simulations(row, n_rep, n_crit, seed))
  }
  if (block == "hlao") {
    return(run_hlao_simulations(row, n_rep, n_crit, seed))
  }
  if (block == "hlao-diagnostic") {
    return(run_hlao_diagnostic_simulations(row, n_rep, n_crit, seed))
  }
  stop("Unknown simulation block: ", block, call. = FALSE)
}

run_simulations <- function(design, block, n_rep, n_crit, seed, pilot,
                            design_id = NULL, require_checkpoints = FALSE) {
  if (block == "all") {
    stop(
      "Run each simulation block separately because their raw schemas differ.",
      call. = FALSE
    )
  }
  block_design <- design[design$block == block, , drop = FALSE]
  if (!nrow(block_design)) {
    stop("Unknown or empty simulation block: ", block, call. = FALSE)
  }
  block_design$simulation_index <- seq_len(nrow(block_design))
  selected <- block_design
  if (!is.null(design_id)) {
    selected <- selected[selected$design_id == design_id, , drop = FALSE]
    if (nrow(selected) != 1L) {
      stop("Unknown or non-unique design ID: ", design_id, call. = FALSE)
    }
  }
  checkpoint_dir <- checkpoint_directory(block, pilot, n_rep, n_crit)
  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  chunks <- if (pilot) vector("list", nrow(selected)) else NULL
  checkpoint_paths <- character(nrow(selected))
  names(checkpoint_paths) <- selected$design_id

  for (index in seq_len(nrow(selected))) {
    row <- selected[index, , drop = FALSE]
    checkpoint_path <- file.path(
      checkpoint_dir,
      sprintf(
        "%03d--%s.rds",
        row$simulation_index[[1L]],
        checkpoint_token(row$design_id)
      )
    )
    if (file.exists(checkpoint_path)) {
      chunk <- readRDS(checkpoint_path)
      message("Resumed checkpoint: ", row$design_id)
    } else if (require_checkpoints) {
      stop("Required checkpoint is missing: ", checkpoint_path, call. = FALSE)
    } else {
      chunk <- run_design_chunk(
        row, block, n_rep = n_rep, n_crit = n_crit, seed = seed
      )
      atomic_save_rds(chunk, checkpoint_path)
      message("Saved checkpoint: ", row$design_id)
    }
    if (!is.data.frame(chunk)) {
      stop("Invalid checkpoint for design: ", row$design_id, call. = FALSE)
    }
    checkpoint_paths[[index]] <- substring(
      normalizePath(checkpoint_path, winslash = "/", mustWork = TRUE),
      nchar(normalizePath(project_root, winslash = "/", mustWork = TRUE)) + 2L
    )
    if (pilot) chunks[[index]] <- chunk
    rm(chunk)
  }

  list(
    results = if (pilot) do.call(rbind, chunks) else NULL,
    result_files = checkpoint_paths
  )
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  options <- parse_run_options(args)
  design <- build_design()
  design$array_task <- seq_len(nrow(design))
  design_path <- file.path(output_dir, "CCMM_2026_wp--design.csv")

  if (options$design_only) {
    utils::write.csv(design, design_path, row.names = FALSE)
    message("Wrote simulation design: ", design_path)
    return(invisible(design))
  }

  if (!is.null(options$array_task)) {
    if (options$array_task > nrow(design)) {
      stop(
        "--array-task exceeds the ", nrow(design), "-row design.",
        call. = FALSE
      )
    }
    task_design <- design[options$array_task, , drop = FALSE]
    options$block <- task_design$block[[1L]]
    options$design_id <- task_design$design_id[[1L]]
    message(
      "Array task ", options$array_task, " maps to ", options$design_id,
      " in block ", options$block, "."
    )
  } else if (!is.null(options$design_id)) {
    task_design <- design[design$design_id == options$design_id, , drop = FALSE]
    if (nrow(task_design) != 1L) {
      stop("Unknown or non-unique design ID: ", options$design_id, call. = FALSE)
    }
    options$block <- task_design$block[[1L]]
  }

  if (!requireNamespace("ramchoice", quietly = TRUE)) {
    stop("Install the development version of ramchoice before running simulations.", call. = FALSE)
  }

  started_at <- Sys.time()
  started_elapsed <- proc.time()[["elapsed"]]
  simulation <- run_simulations(
    design = design,
    block = options$block,
    n_rep = options$n_rep,
    n_crit = options$n_crit,
    seed = master_seed,
    pilot = options$pilot,
    design_id = options$design_id,
    require_checkpoints = options$assemble_only
  )
  ended_at <- Sys.time()

  if (options$checkpoint_only) {
    message("Checkpoint task completed without writing a shared manifest.")
    return(invisible(simulation))
  }

  suffix <- if (options$pilot) "--pilot" else ""
  output_path <- file.path(
    output_dir,
    paste0("CCMM_2026_wp--raw-", options$block, suffix, ".rds")
  )
  selected_design <- design[design$block == options$block, , drop = FALSE]
  atomic_save_rds(
    list(
      design = selected_design,
      results = simulation$results,
      result_files = simulation$result_files,
      master_seed = master_seed,
      n_rep = options$n_rep,
      n_crit = options$n_crit,
      pilot = options$pilot,
      block = options$block,
      assembled_only = options$assemble_only,
      simulation_schema_version = simulation_schema_version,
      started_at = started_at,
      ended_at = ended_at,
      elapsed_seconds = unname(proc.time()[["elapsed"]] - started_elapsed),
      ramchoice_version = as.character(utils::packageVersion("ramchoice")),
      ramchoice_git_sha = Sys.getenv("RAMCHOICE_GIT_SHA", unset = NA_character_),
      replication_git_sha = Sys.getenv(
        "REPLICATION_GIT_SHA",
        unset = NA_character_
      ),
      session_info = utils::sessionInfo()
    ),
    output_path
  )
  message("Wrote raw simulation results: ", output_path)
  invisible(simulation)
}

if (sys.nframe() == 0L) {
  main()
}
