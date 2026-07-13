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
  } else {
    stop("Unknown H-LAO preference design: ", design, call. = FALSE)
  }
  list(rankings = rankings, tau = tau / sum(tau))
}

hlao_continuation <- function(items, stopping_design, reach_design,
                              universe_size) {
  remaining <- length(items) - seq_along(items)
  if (stopping_design == "geometric") {
    probability <- if (reach_design == "high") 0.90 else 0.50
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
    attention <- hlao_prefix_masses(hlao_continuation(
      items,
      design_row$stopping_design,
      design_row$reach_design,
      universe_size
    ))
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
                            population_robust_compatible = NA) {
  covered <- if (target_valid && is.finite(truth) && is.finite(lower) &&
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
    width = if (is.finite(lower) && is.finite(upper)) upper - lower else NA_real_,
    identified = identified,
    target_valid = target_valid,
    population_lower = population_lower,
    population_upper = population_upper,
    population_independent_compatible = population_independent_compatible,
    population_robust_compatible = population_robust_compatible,
    method = method,
    alpha = alpha,
    elapsed_seconds = elapsed_seconds,
    stringsAsFactors = FALSE
  )
}

run_hlao_simulations <- function(design, n_rep, seed) {
  result <- list()
  result_index <- 0L
  population_cache <- list()

  for (design_index in seq_len(nrow(design))) {
    row <- design[design_index, ]
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
        dependence = if (row$universe_size == 4L) "both" else "independent"
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
    if (!length(robust_compatible)) robust_compatible <- NA
    robust_bounds <- population$analysis$bounds[
      population$analysis$bounds$mode == "robust", , drop = FALSE
    ]

    for (replication in seq_len(n_rep)) {
      seed_replication <- replication_seed(
        seed,
        1000L + design_index,
        replication
      )
      set.seed(seed_replication)
      sample <- simulate_hlao_sample(population, row$n_per_menu)
      event_argument <- if (row$universe_size == 4L) {
        setNames(list(population$event), population$event_name)
      } else {
        NULL
      }
      fit <- ramchoice::hlaoTest(
        sample$menu,
        sample$choice,
        events = event_argument,
        alpha = 0.05
      )

      for (attention_index in seq_len(nrow(fit$attention))) {
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
          population_robust_compatible = robust_compatible
        )
      }

      if (!is.null(fit$full_attention) &&
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
              population_robust_compatible = robust_compatible
            )
          }
        }
      }

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
        target_valid <- row$dependence_design == "independent"
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
          population_robust_compatible = robust_compatible
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
          target_valid = TRUE,
          population_lower = if (nrow(robust_bounds)) robust_bounds$lower else NA_real_,
          population_upper = if (nrow(robust_bounds)) robust_bounds$upper else NA_real_,
          method = interval$method,
          alpha = interval$alpha,
          elapsed_seconds = fit$elapsed,
          population_independent_compatible = independent_compatible,
          population_robust_compatible = robust_compatible
        )
      }
    }

    message(
      "Completed ", row$design_id, " (", design_index, "/", nrow(design), ")"
    )
  }
  do.call(rbind, result[seq_len(result_index)])
}

run_simulations <- function(design, block, n_rep, n_crit, seed) {
  if (block == "homogeneous-aom") {
    selected <- design[design$block == block, , drop = FALSE]
    return(run_aom_simulations(
      selected,
      n_rep = n_rep,
      n_crit = n_crit,
      seed = seed
    ))
  }
  if (block == "hlao") {
    selected <- design[design$block == block, , drop = FALSE]
    return(run_hlao_simulations(selected, n_rep = n_rep, seed = seed))
  }
  if (block %in% c("hlao-diagnostic", "all")) {
    stop(
      "The hlao-diagnostic block is not implemented yet; run a specific completed block.",
      call. = FALSE
    )
  }
  stop("Unknown simulation block: ", block, call. = FALSE)
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
