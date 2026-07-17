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
data_path <- option_value(args, "data", file.path(root, "data", "data_clean.xlsx"))
output_dir <- option_value(args, "output-dir", file.path(root, "output"))
alpha <- as.numeric(option_value(args, "alpha", "0.05"))
n_draws <- as.integer(option_value(args, "draws", "4999"))
seed <- as.integer(option_value(args, "seed", "20260716"))

if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
  stop("--alpha must be strictly between zero and one.", call. = FALSE)
}
if (is.na(n_draws) || n_draws < 100L) {
  stop("--draws must be an integer of at least 100.", call. = FALSE)
}
if (is.na(seed)) {
  stop("--seed must be an integer.", call. = FALSE)
}

if (!file.exists(data_path)) {
  stop(
    "The restricted Wang--Zhu workbook was not found. Supply its local path ",
    "with --data=C:/path/to/data_clean.xlsx.",
    call. = FALSE
  )
}
for (package in c("readxl", "ramchoice")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Install the R package '", package, "' before running this script.",
         call. = FALSE)
  }
}

data <- readxl::read_excel(data_path, .name_repair = "minimal")
modes <- c("A", "B", "C", "D")
required <- c(
  "id", "task", "menu", "decision", "nb_click", "pre",
  paste0(modes, "_mode"),
  as.vector(outer(c("time_", "price_"), paste0(modes, "_checked"), paste0))
)
missing_columns <- setdiff(required, names(data))
if (length(missing_columns)) {
  stop("Missing required column(s): ", paste(missing_columns, collapse = ", "),
       call. = FALSE)
}

n <- nrow(data)
ids <- as.character(data$id)
tasks <- as.character(data$task)
decision <- as.character(data$decision)
menu <- vapply(
  modes,
  function(mode) as.integer(as.character(data[[paste0(mode, "_mode")]]) == mode),
  integer(n)
)
choice <- vapply(
  modes,
  function(mode) as.integer(decision == mode),
  integer(n)
)
outside <- as.integer(rowSums(choice) == 0L)
menu_key <- apply(menu, 1L, paste0, collapse = "")
menu_label <- apply(menu, 1L, function(row) {
  label <- paste0(modes[as.logical(row)], collapse = "")
  if (nzchar(label)) label else "Empty"
})

duplicate_id_task <- sum(duplicated(paste(ids, tasks, sep = "|")))
unavailable_choices <- sum(rowSums(choice * (1L - menu)) > 0L)
click_total <- rowSums(vapply(
  as.vector(outer(c("time_", "price_"), paste0(modes, "_checked"), paste0)),
  function(name) as.numeric(data[[name]]),
  numeric(n)
))
click_mismatch <- sum(click_total != as.numeric(data$nb_click), na.rm = TRUE)
if (duplicate_id_task || unavailable_choices || click_mismatch) {
  stop("Data-integrity checks failed; no output was written.", call. = FALSE)
}

support_count <- tapply(menu_key, ids, function(value) length(unique(value)))
complete_subjects <- names(support_count[support_count == 16L])
raw_reports <- unique(data.frame(id = ids, pre = as.character(data$pre)))
dominance_consistent_report <- vapply(raw_reports$pre, function(value) {
  all(vapply(modes, grepl, logical(1L), x = value, fixed = TRUE))
}, logical(1L))
dominance_consistent_ids <- raw_reports$id[dominance_consistent_report]
complete_rows <- ids %in% complete_subjects & rowSums(menu) > 0L
dominance_consistent_rows <-
  ids %in% dominance_consistent_ids & rowSums(menu) > 0L
common_rows <- ids %in% dominance_consistent_ids & rowSums(menu) >= 2L

population_fit <- function(rows, dependence) {
  keys <- unique(menu_key[rows])
  population_menu <- matrix(0, nrow = length(keys), ncol = length(modes))
  population_prob <- matrix(0, nrow = length(keys), ncol = length(modes))
  population_outside <- numeric(length(keys))
  for (index in seq_along(keys)) {
    selected <- rows & menu_key == keys[[index]]
    population_menu[index, ] <- menu[which(selected)[1L], ]
    population_prob[index, ] <- colMeans(choice[selected, , drop = FALSE])
    population_outside[index] <- mean(outside[selected])
  }
  colnames(population_menu) <- colnames(population_prob) <- modes
  ramchoice::hlaoModel(
    menu = population_menu,
    prob = population_prob,
    outside_prob = population_outside,
    list_order = seq_along(modes),
    dependence = dependence,
    agreement = dependence != "noPI"
  )
}

complete_fit <- population_fit(complete_rows, "all")
dominance_consistent_fit <- population_fit(dominance_consistent_rows, "all")
common_fit <- population_fit(common_rows, "noPI")

rankings <- ramchoice::hlaoRankings(seq_along(modes))
pairwise_events <- list()
for (earlier in seq_len(length(modes) - 1L)) {
  for (later in (earlier + 1L):length(modes)) {
    event_name <- paste0(modes[later], " above ", modes[earlier])
    pairwise_events[[event_name]] <- apply(rankings, 1L, function(ranking) {
      match(later, ranking) < match(earlier, ranking)
    })
  }
}

run_hlao_test <- function(rows, clustered, draw_seed) {
  set.seed(draw_seed)
  ramchoice::hlaoTest(
    menu = menu[rows, , drop = FALSE],
    choice = choice[rows, , drop = FALSE],
    outside = outside[rows],
    list_order = seq_along(modes),
    events = pairwise_events,
    alpha = alpha,
    band_method = "gaussian",
    diagnostic_method = "delta",
    n_band_draws = n_draws,
    cluster = if (clustered) ids[rows] else NULL
  )
}

complete_hlao_iid <- run_hlao_test(complete_rows, FALSE, seed + 1L)
complete_hlao_cluster <- run_hlao_test(complete_rows, TRUE, seed + 2L)
dominance_consistent_hlao_iid <-
  run_hlao_test(dominance_consistent_rows, FALSE, seed + 3L)
dominance_consistent_hlao_cluster <-
  run_hlao_test(dominance_consistent_rows, TRUE, seed + 4L)

set.seed(seed + 5L)
common_hlao_iid <- ramchoice::hlaoNoPITest(
  menu = menu[common_rows, , drop = FALSE],
  choice = choice[common_rows, , drop = FALSE],
  outside = outside[common_rows],
  list_order = seq_along(modes),
  events = pairwise_events,
  alpha = alpha,
  band_method = "gaussian",
  n_band_draws = n_draws
)
set.seed(seed + 6L)
common_hlao_cluster <- ramchoice::hlaoNoPITest(
  menu = menu[common_rows, , drop = FALSE],
  choice = choice[common_rows, , drop = FALSE],
  outside = outside[common_rows],
  list_order = seq_along(modes),
  events = pairwise_events,
  alpha = alpha,
  band_method = "gaussian",
  n_band_draws = n_draws,
  cluster = ids[common_rows]
)

aom_menu <- cbind(menu, Default = 1L)
aom_choice <- cbind(choice, Default = outside)
aom_preferences <- cbind(rankings, Default = length(modes) + 1L)
run_aom_test <- function(rows, clustered, draw_seed) {
  set.seed(draw_seed)
  ramchoice::aomTest(
    menu = aom_menu[rows, , drop = FALSE],
    choice = aom_choice[rows, , drop = FALSE],
    pref_list = aom_preferences,
    method = "ALL",
    alpha = alpha,
    nCritSimu = n_draws,
    cluster = if (clustered) ids[rows] else NULL
  )
}

complete_aom_iid <- run_aom_test(ids %in% complete_subjects, FALSE, seed + 7L)
complete_aom_cluster <- run_aom_test(ids %in% complete_subjects, TRUE, seed + 8L)
dominance_consistent_aom_iid <- run_aom_test(
  ids %in% dominance_consistent_ids, FALSE, seed + 17L
)
dominance_consistent_aom_cluster <- run_aom_test(
  ids %in% dominance_consistent_ids, TRUE, seed + 18L
)
ram_rational_strings <- unique(raw_reports$pre[dominance_consistent_report])
ram_rational_preferences <- t(vapply(ram_rational_strings, function(value) {
  match(strsplit(value, "", fixed = TRUE)[[1L]], modes)
}, integer(length(modes))))
ram_rational_preferences <- cbind(
  ram_rational_preferences,
  Default = length(modes) + 1L
)
ram_irrational_preferences <- rbind(
  c(2L, 3L, 4L, 5L, 1L),
  c(5L, 1L, 4L, 3L, 2L)
)
ram_preferences <- rbind(
  ram_rational_preferences,
  ram_irrational_preferences
)
ram_ranking_group <- c(
  rep("reported-rational", nrow(ram_rational_preferences)),
  rep("reported-irrational", nrow(ram_irrational_preferences))
)
run_ram_test <- function(rows, clustered, draw_seed) {
  set.seed(draw_seed)
  ramchoice::ramTest(
    menu = aom_menu[rows, , drop = FALSE],
    choice = aom_choice[rows, , drop = FALSE],
    pref_list = ram_preferences,
    method = "ALL",
    alpha = alpha,
    nCritSimu = n_draws,
    attBinary = 2 / 3,
    cluster = if (clustered) ids[rows] else NULL
  )
}
ram_iid <- run_ram_test(ids %in% dominance_consistent_ids, FALSE, seed + 15L)
ram_cluster <- run_ram_test(ids %in% dominance_consistent_ids, TRUE, seed + 16L)

reported <- raw_reports[dominance_consistent_report, , drop = FALSE]

pairwise_validation <- function(fit, report_ids) {
  pairwise <- fit$pairwise
  if (!nrow(pairwise)) return(data.frame())
  reports <- reported[reported$id %in% report_ids, , drop = FALSE]
  pairwise$earlier_mode <- modes[pairwise$earlier]
  pairwise$later_mode <- modes[pairwise$later]
  pairwise$reported_share <- mapply(function(earlier, later) {
    mean(
      regexpr(later, reports$pre, fixed = TRUE) <
        regexpr(earlier, reports$pre, fixed = TRUE)
    )
  }, pairwise$earlier_mode, pairwise$later_mode)
  pairwise$absolute_difference <- abs(
    pairwise$share_later_preferred - pairwise$reported_share
  )
  pairwise$reported_subjects <- nrow(reports)
  pairwise[c(
    "earlier_mode", "later_mode", "share_later_preferred",
    "reported_share", "absolute_difference", "reported_subjects"
  )]
}

complete_validation <- pairwise_validation(complete_fit, complete_subjects)
dominance_consistent_validation <- pairwise_validation(
  dominance_consistent_fit,
  dominance_consistent_ids
)

simultaneous_difference_band <- function(scores, estimate, draw_seed) {
  standard_error <- sqrt(colSums(scores^2))
  set.seed(draw_seed)
  weights <- matrix(
    stats::rnorm(n_draws * nrow(scores)),
    nrow = n_draws,
    ncol = nrow(scores)
  )
  draws <- weights %*% scores
  active <- standard_error > sqrt(.Machine$double.eps)
  standardized <- matrix(0, nrow = n_draws, ncol = ncol(scores))
  standardized[, active] <- sweep(
    draws[, active, drop = FALSE],
    2L,
    standard_error[active],
    "/"
  )
  critical_value <- as.numeric(stats::quantile(
    apply(abs(standardized), 1L, max),
    1 - alpha,
    names = FALSE,
    type = 8
  ))
  lower <- estimate - critical_value * standard_error
  upper <- estimate + critical_value * standard_error
  list(
    standard_error = standard_error,
    critical_value = critical_value,
    lower = lower,
    upper = upper,
    reject = lower > 0 | upper < 0
  )
}

pairwise_inference <- function(
  iid_fit, cluster_fit, analysis_rows, report_ids, draw_seed
) {
  iid <- iid_fit$pairwise_studentized
  clustered <- cluster_fit$pairwise_studentized
  result <- data.frame(
    earlier = iid$earlier,
    later = iid$later,
    earlier_mode = modes[iid$earlier],
    later_mode = modes[iid$later],
    estimate = iid$estimate,
    iid_lower = iid$lower,
    iid_upper = iid$upper,
    cluster_lower = clustered$lower,
    cluster_upper = clustered$upper,
    stringsAsFactors = FALSE
  )
  iid_projection <- iid_fit$pairwise
  cluster_projection <- cluster_fit$pairwise
  result$iid_projection_lower <- iid_projection$lower
  result$iid_projection_upper <- iid_projection$upper
  result$cluster_projection_lower <- cluster_projection$lower
  result$cluster_projection_upper <- cluster_projection$upper

  reports <- reported[reported$id %in% report_ids, , drop = FALSE]
  result$reported_share <- mapply(function(earlier, later) {
    mean(
      regexpr(later, reports$pre, fixed = TRUE) <
        regexpr(earlier, reports$pre, fixed = TRUE)
    )
  }, result$earlier_mode, result$later_mode)
  result$difference <- result$estimate - result$reported_share
  result$reported_subjects <- nrow(reports)

  iid_summary <- iid_fit$summary
  analysis_menu <- menu[analysis_rows, , drop = FALSE]
  analysis_choice <- choice[analysis_rows, , drop = FALSE]
  analysis_outside <- outside[analysis_rows]
  n_choice <- nrow(analysis_menu)
  n_report <- nrow(reports)
  if (n_choice < 2L || n_report < 2L) {
    stop("Pairwise difference inference requires at least two observations.")
  }
  iid_difference_scores <- matrix(
    0,
    nrow = n_choice + n_report,
    ncol = nrow(result)
  )
  for (index in seq_len(nrow(result))) {
    menu_id <- iid$menu_id[index]
    later <- result$later[index]
    singleton_id <- which(
      rowSums(iid_summary$menu) == 1L & iid_summary$menu[, later] == 1L
    )
    binary_rows <- rowSums(
      analysis_menu != matrix(
        iid_summary$menu[menu_id, ],
        nrow = n_choice,
        ncol = ncol(analysis_menu),
        byrow = TRUE
      )
    ) == 0L
    singleton_rows <- rowSums(
      analysis_menu != matrix(
        iid_summary$menu[singleton_id, ],
        nrow = n_choice,
        ncol = ncol(analysis_menu),
        byrow = TRUE
      )
    ) == 0L
    y <- iid_summary$prob[menu_id, later]
    e_ab <- iid_summary$outside_prob[menu_id]
    e_b <- iid_summary$outside_prob[singleton_id]
    reach <- (1 - e_ab) * (1 - e_b)
    gradient <- c(
      1 / reach,
      y * (1 - e_b) / reach^2,
      y * (1 - e_ab) / reach^2
    )
    model_score <- numeric(n_choice)
    model_score[binary_rows] <-
      (analysis_choice[binary_rows, later] - y) /
        sum(binary_rows) * gradient[1L] +
      (analysis_outside[binary_rows] - e_ab) /
        sum(binary_rows) * gradient[2L]
    model_score[singleton_rows] <- model_score[singleton_rows] +
      (analysis_outside[singleton_rows] - e_b) /
        sum(singleton_rows) * gradient[3L]
    report_indicator <- vapply(
      reports$pre,
      function(preference) {
        regexpr(result$later_mode[index], preference, fixed = TRUE) <
          regexpr(result$earlier_mode[index], preference, fixed = TRUE)
      },
      logical(1L)
    )
    report_score <- (report_indicator - result$reported_share[index]) / n_report
    iid_difference_scores[, index] <- c(
      model_score * sqrt(n_choice / (n_choice - 1L)),
      -report_score * sqrt(n_report / (n_report - 1L))
    )
  }

  summary <- cluster_fit$summary
  n_cluster <- summary$n_cluster
  cluster_difference_scores <- matrix(
    0,
    nrow = n_cluster,
    ncol = nrow(result)
  )
  for (index in seq_len(nrow(result))) {
    menu_id <- clustered$menu_id[index]
    later <- result$later[index]
    singleton_id <- which(
      rowSums(summary$menu) == 1L & summary$menu[, later] == 1L
    )
    y <- summary$prob[menu_id, later]
    e_ab <- summary$outside_prob[menu_id]
    e_b <- summary$outside_prob[singleton_id]
    reach <- (1 - e_ab) * (1 - e_b)
    cells <- c(
      summary$inside_cell[menu_id, later],
      summary$outside_cell[menu_id],
      summary$outside_cell[singleton_id]
    )
    gradient <- c(
      1 / reach,
      y * (1 - e_b) / reach^2,
      y * (1 - e_ab) / reach^2
    )
    model_score <- as.numeric(
      summary$cluster_scores[, cells, drop = FALSE] %*% gradient
    )
    report_indicator <- numeric(n_cluster)
    report_match <- match(summary$cluster_labels, reports$id)
    observed_report <- !is.na(report_match)
    report_indicator[observed_report] <- vapply(
      reports$pre[report_match[observed_report]],
      function(preference) {
        regexpr(result$later_mode[index], preference, fixed = TRUE) <
          regexpr(result$earlier_mode[index], preference, fixed = TRUE)
      },
      logical(1L)
    )
    report_score <- numeric(n_cluster)
    report_score[observed_report] <- (
      report_indicator[observed_report] - result$reported_share[index]
    ) / nrow(reports)
    cluster_difference_scores[, index] <- model_score - report_score
  }
  cluster_difference_scores <- cluster_difference_scores * sqrt(
    n_cluster / (n_cluster - 1L)
  )
  iid_band <- simultaneous_difference_band(
    iid_difference_scores,
    result$difference,
    draw_seed + 100L
  )
  cluster_band <- simultaneous_difference_band(
    cluster_difference_scores,
    result$difference,
    draw_seed
  )
  for (field in c("standard_error", "lower", "upper", "reject")) {
    result[[paste0("iid_difference_", field)]] <- iid_band[[field]]
    result[[paste0("cluster_difference_", field)]] <- cluster_band[[field]]
  }
  result$iid_difference_critical_value <- iid_band$critical_value
  result$cluster_difference_critical_value <- cluster_band$critical_value

  # Preserve the previous field names as clustered-inference aliases.
  result$difference_se <- cluster_band$standard_error
  result$difference_lower <- cluster_band$lower
  result$difference_upper <- cluster_band$upper
  result$difference_reject <- cluster_band$reject
  result
}

complete_pairwise_inference <- pairwise_inference(
  complete_hlao_iid,
  complete_hlao_cluster,
  complete_rows,
  complete_subjects,
  seed + 11L
)
dominance_consistent_pairwise_inference <- pairwise_inference(
  dominance_consistent_hlao_iid,
  dominance_consistent_hlao_cluster,
  dominance_consistent_rows,
  dominance_consistent_ids,
  seed + 12L
)

descriptive_rows <- lapply(sort(unique(rowSums(menu))), function(size) {
  selected <- rowSums(menu) == size
  data.frame(
    inside_menu_size = size,
    observations = sum(selected),
    mean_displays = mean(as.numeric(data$nb_click[selected])),
    no_display_share = mean(as.numeric(data$nb_click[selected]) == 0),
    default_choice_share = mean(outside[selected] == 1L)
  )
})
descriptives <- do.call(rbind, descriptive_rows)

search_rows <- list()
search_index <- 0L
for (label in unique(menu_label)) {
  label_rows <- ids %in% dominance_consistent_ids & menu_label == label
  for (mode in modes) {
    mode_index <- match(mode, modes)
    selected <- label_rows & menu[, mode_index] == 1L
    if (!any(selected)) next
    inspected <-
      as.numeric(data[[paste0("time_", mode, "_checked")]]) > 0 |
      as.numeric(data[[paste0("price_", mode, "_checked")]]) > 0
    search_index <- search_index + 1L
    search_rows[[search_index]] <- data.frame(
      menu = label,
      mode = mode,
      observations = sum(selected),
      inspected_rate = mean(inspected[selected]),
      inspected_or_chosen_rate = mean((inspected | decision == mode)[selected])
    )
  }
}
search_rates <- do.call(rbind, search_rows)

is_strict_subset <- function(smaller, larger) {
  small <- strsplit(smaller, "", fixed = TRUE)[[1L]]
  large <- strsplit(larger, "", fixed = TRUE)[[1L]]
  length(small) < length(large) && all(small %in% large)
}
comparison_rows <- list()
comparison_index <- 0L
for (mode in modes) {
  relevant <- search_rates[search_rates$mode == mode, , drop = FALSE]
  for (small_index in seq_len(nrow(relevant))) {
    for (large_index in seq_len(nrow(relevant))) {
      if (!is_strict_subset(
        relevant$menu[[small_index]], relevant$menu[[large_index]]
      )) next
      comparison_index <- comparison_index + 1L
      comparison_rows[[comparison_index]] <- data.frame(
        mode = mode,
        smaller_menu = relevant$menu[[small_index]],
        larger_menu = relevant$menu[[large_index]],
        inspected_difference =
          relevant$inspected_rate[[large_index]] -
          relevant$inspected_rate[[small_index]],
        inspected_or_chosen_difference =
          relevant$inspected_or_chosen_rate[[large_index]] -
          relevant$inspected_or_chosen_rate[[small_index]]
      )
    }
  }
}
search_comparisons <- do.call(rbind, comparison_rows)

search_rate_score <- function(label, mode, proxy) {
  mode_index <- match(mode, modes)
  selected <- ids %in% dominance_consistent_ids & menu_label == label &
    menu[, mode_index] == 1L
  inspected <-
    as.numeric(data[[paste0("time_", mode, "_checked")]]) > 0 |
    as.numeric(data[[paste0("price_", mode, "_checked")]]) > 0
  outcome <- if (proxy == "inspected") {
    inspected
  } else {
    inspected | decision == mode
  }
  estimate <- mean(outcome[selected])
  score <- numeric(n)
  score[selected] <- (outcome[selected] - estimate) / sum(selected)
  list(estimate = estimate, score = score)
}

search_score_matrix <- function(proxy) {
  scores <- matrix(0, nrow = n, ncol = nrow(search_comparisons))
  for (index in seq_len(nrow(search_comparisons))) {
    small <- search_rate_score(
      search_comparisons$smaller_menu[index],
      search_comparisons$mode[index],
      proxy
    )
    large <- search_rate_score(
      search_comparisons$larger_menu[index],
      search_comparisons$mode[index],
      proxy
    )
    scores[, index] <- large$score - small$score
  }
  scores
}

simultaneous_score_intervals <- function(estimates, row_scores, row_ids, draw_seed) {
  cluster_index <- match(row_ids, unique(row_ids))
  cluster_scores <- rowsum(row_scores, cluster_index, reorder = FALSE)
  score_sets <- list(iid = row_scores, cluster = cluster_scores)
  result <- list()
  set.seed(draw_seed)
  for (sampling in names(score_sets)) {
    scores <- score_sets[[sampling]]
    n_unit <- nrow(scores)
    correction <- n_unit / (n_unit - 1L)
    standard_error <- sqrt(correction * colSums(scores^2))
    weights <- matrix(
      stats::rnorm(n_draws * n_unit),
      nrow = n_draws,
      ncol = n_unit
    )
    draws <- weights %*% scores * sqrt(correction)
    active <- standard_error > sqrt(.Machine$double.eps)
    standardized <- matrix(0, nrow = n_draws, ncol = length(estimates))
    standardized[, active] <- sweep(
      draws[, active, drop = FALSE],
      2L,
      standard_error[active],
      "/"
    )
    critical_value <- as.numeric(stats::quantile(
      apply(abs(standardized), 1L, max),
      1 - alpha,
      names = FALSE,
      type = 8
    ))
    result[[sampling]] <- data.frame(
      standard_error = standard_error,
      lower = estimates - critical_value * standard_error,
      upper = estimates + critical_value * standard_error,
      critical_value = critical_value
    )
  }
  result
}

inspected_scores <-
  search_score_matrix("inspected")[dominance_consistent_rows, , drop = FALSE]
inspected_or_chosen_scores <-
  search_score_matrix("inspected_or_chosen")[
    dominance_consistent_rows, , drop = FALSE
  ]
inspected_intervals <- simultaneous_score_intervals(
  search_comparisons$inspected_difference,
  inspected_scores,
  ids[dominance_consistent_rows],
  seed + 13L
)
inspected_or_chosen_intervals <- simultaneous_score_intervals(
  search_comparisons$inspected_or_chosen_difference,
  inspected_or_chosen_scores,
  ids[dominance_consistent_rows],
  seed + 14L
)
for (sampling in c("iid", "cluster")) {
  for (field in c("standard_error", "lower", "upper")) {
    search_comparisons[[paste("inspected", sampling, field, sep = "_")]] <-
      inspected_intervals[[sampling]][[field]]
    search_comparisons[[paste(
      "inspected_or_chosen", sampling, field, sep = "_"
    )]] <- inspected_or_chosen_intervals[[sampling]][[field]]
  }
}

collect_aom <- function(fit, sample_name, sampling) {
  result <- fit$results
  result$sample <- sample_name
  result$sampling <- sampling
  result
}
aom_inference <- rbind(
  collect_aom(complete_aom_iid, "complete", "iid"),
  collect_aom(complete_aom_cluster, "complete", "cluster"),
  collect_aom(
    dominance_consistent_aom_iid, "dominance-consistent", "iid"
  ),
  collect_aom(
    dominance_consistent_aom_cluster, "dominance-consistent", "cluster"
  )
)
aom_ranking_key <- apply(aom_preferences, 1L, paste, collapse = "-")
reported_rational_key <- apply(
  ram_rational_preferences, 1L, paste, collapse = "-"
)
aom_inference$reported_rational <-
  aom_ranking_key[aom_inference$preference_id] %in% reported_rational_key

collect_ram <- function(fit, sampling) {
  result <- fit$results
  result$ranking_group <- ram_ranking_group[result$preference_id]
  result$sample <- "rational-report"
  result$sampling <- sampling
  result
}
ram_inference <- rbind(
  collect_ram(ram_iid, "iid"),
  collect_ram(ram_cluster, "cluster")
)

collect_specification <- function(fit, sample_name, sampling) {
  result <- fit$specification
  result$sample <- sample_name
  result$sampling <- sampling
  result$critical_value <- fit$options$diagnostic_critical_value
  result
}
hlao_specification <- rbind(
  collect_specification(complete_hlao_iid, "complete", "iid"),
  collect_specification(complete_hlao_cluster, "complete", "cluster"),
  collect_specification(
    dominance_consistent_hlao_iid, "dominance-consistent", "iid"
  ),
  collect_specification(
    dominance_consistent_hlao_cluster, "dominance-consistent", "cluster"
  )
)

collect_common_intervals <- function(fit, sampling) {
  result <- fit$intervals
  result$sampling <- sampling
  result
}
common_menu_intervals <- rbind(
  collect_common_intervals(common_hlao_iid, "iid"),
  collect_common_intervals(common_hlao_cluster, "cluster")
)

results <- list(
  metadata = list(
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    ramchoice_version = as.character(utils::packageVersion("ramchoice")),
    replication_git_sha = Sys.getenv("REPLICATION_GIT_SHA", unset = NA_character_),
    data_basename = basename(data_path),
    alpha = alpha,
    n_draws = n_draws,
    seed = seed,
    inference_status = paste(
      "Complete and dominance-consistent samples: row-i.i.d. benchmarks",
      "and subject-clustered",
      "covariance/multiplier inference."
    )
  ),
  integrity = list(
    duplicate_id_task = duplicate_id_task,
    unavailable_choices = unavailable_choices,
    click_count_mismatch = click_mismatch
  ),
  design = list(
    observations = n,
    subjects = length(unique(ids)),
    tasks = length(unique(tasks)),
    observed_menus = length(unique(menu_key)),
    complete_design_subjects = length(complete_subjects),
    dominance_consistent_subjects = length(dominance_consistent_ids),
    rational_reports = nrow(reported)
  ),
  descriptives = descriptives,
  complete_design = list(
    fit = complete_fit,
    iid_inference = list(aom = complete_aom_iid, hlao = complete_hlao_iid),
    cluster_inference = list(
      aom = complete_aom_cluster,
      hlao = complete_hlao_cluster
    ),
    pairwise_validation = complete_validation,
    pairwise_inference = complete_pairwise_inference
  ),
  dominance_consistent = list(
    subjects = dominance_consistent_ids,
    fit = dominance_consistent_fit,
    iid_inference = list(
      aom = dominance_consistent_aom_iid,
      hlao = dominance_consistent_hlao_iid
    ),
    cluster_inference = list(
      aom = dominance_consistent_aom_cluster,
      hlao = dominance_consistent_hlao_cluster
    ),
    pairwise_validation = dominance_consistent_validation,
    pairwise_inference = dominance_consistent_pairwise_inference
  ),
  common_menu = list(
    subjects = dominance_consistent_ids,
    fit = common_fit,
    iid_inference = common_hlao_iid,
    cluster_inference = common_hlao_cluster,
    intervals = common_menu_intervals
  ),
  ram_inference = ram_inference,
  aom_inference = aom_inference,
  hlao_specification = hlao_specification,
  search = list(
    subjects = dominance_consistent_ids,
    rates = search_rates,
    comparisons = search_comparisons,
    inspected_critical_values = vapply(
      inspected_intervals,
      function(value) value$critical_value[1L],
      numeric(1L)
    ),
    inspected_or_chosen_critical_values = vapply(
      inspected_or_chosen_intervals,
      function(value) value$critical_value[1L],
      numeric(1L)
    )
  ),
  session_info = utils::sessionInfo()
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(results, file.path(output_dir, "CCMM_2026_wp--empapp-results.rds"))
utils::write.csv(
  descriptives,
  file.path(output_dir, "CCMM_2026_wp--empapp-descriptives.csv"),
  row.names = FALSE
)
utils::write.csv(
  complete_validation,
  file.path(output_dir, "CCMM_2026_wp--empapp-pairwise-complete.csv"),
  row.names = FALSE
)
utils::write.csv(
  dominance_consistent_validation,
  file.path(
    output_dir,
    "CCMM_2026_wp--empapp-pairwise-dominance-consistent.csv"
  ),
  row.names = FALSE
)
utils::write.csv(
  search_comparisons,
  file.path(output_dir, "CCMM_2026_wp--empapp-search-comparisons.csv"),
  row.names = FALSE
)
utils::write.csv(
  complete_pairwise_inference,
  file.path(output_dir, "CCMM_2026_wp--empapp-pairwise-inference-complete.csv"),
  row.names = FALSE
)
utils::write.csv(
  dominance_consistent_pairwise_inference,
  file.path(
    output_dir,
    "CCMM_2026_wp--empapp-pairwise-inference-dominance-consistent.csv"
  ),
  row.names = FALSE
)
utils::write.csv(
  ram_inference,
  file.path(output_dir, "CCMM_2026_wp--empapp-ram-inference.csv"),
  row.names = FALSE
)
utils::write.csv(
  aom_inference,
  file.path(output_dir, "CCMM_2026_wp--empapp-aom-inference.csv"),
  row.names = FALSE
)
utils::write.csv(
  hlao_specification,
  file.path(output_dir, "CCMM_2026_wp--empapp-hlao-specification.csv"),
  row.names = FALSE
)
utils::write.csv(
  common_menu_intervals,
  file.path(output_dir, "CCMM_2026_wp--empapp-common-menu-intervals.csv"),
  row.names = FALSE
)

cat("Wrote aggregate empirical results to", normalizePath(output_dir), "\n")
cat("Complete-design subjects:", length(complete_subjects), "\n")
cat("Mean complete-design pairwise gap:",
    sprintf("%.3f", mean(complete_validation$absolute_difference)), "\n")
cat("Complete-design AOM nonrejections (GMS, iid/cluster):",
    sum(aom_inference$sample == "complete" & aom_inference$sampling == "iid" &
          aom_inference$method == "GMS" & !aom_inference$reject), "/",
    sum(aom_inference$sample == "complete" & aom_inference$sampling == "cluster" &
          aom_inference$method == "GMS" & !aom_inference$reject), "\n")
cat("Inspected-or-chosen directional violations:",
    sum(search_comparisons$inspected_or_chosen_difference > 0), "of",
    nrow(search_comparisons), "\n")
