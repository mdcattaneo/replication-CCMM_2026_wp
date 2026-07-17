#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

script_path <- function() {
  argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(argument)) return(normalizePath(".", winslash = "/"))
  normalizePath(sub("^--file=", "", argument[[1L]]), winslash = "/")
}

script_dir <- dirname(script_path())
output_dir <- file.path(script_dir, "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

parse_options <- function(arguments) {
  result <- list(
    pilot = FALSE,
    repetitions = 3L,
    aom_max = 7L,
    hlao_max = 7L
  )
  for (argument in arguments) {
    if (identical(argument, "--pilot")) {
      result$pilot <- TRUE
    } else if (grepl("^--repetitions=", argument)) {
      result$repetitions <- as.integer(sub("^--repetitions=", "", argument))
    } else if (grepl("^--aom-max=", argument)) {
      result$aom_max <- as.integer(sub("^--aom-max=", "", argument))
    } else if (grepl("^--hlao-max=", argument)) {
      result$hlao_max <- as.integer(sub("^--hlao-max=", "", argument))
    } else {
      stop("Unknown option: ", argument, call. = FALSE)
    }
  }
  if (result$pilot) {
    result$repetitions <- 1L
    result$aom_max <- min(result$aom_max, 4L)
    result$hlao_max <- min(result$hlao_max, 4L)
  }
  values <- unlist(result[c("repetitions", "aom_max", "hlao_max")])
  if (anyNA(values) || result$repetitions < 1L ||
      result$aom_max < 3L || result$hlao_max < 3L) {
    stop("Benchmark dimensions and repetitions must be positive integers.", call. = FALSE)
  }
  result
}

options_run <- parse_options(commandArgs(trailingOnly = TRUE))

suppressPackageStartupMessages(library(ramchoice))

safe_git_commit <- function(path) {
  if (!dir.exists(file.path(path, ".git"))) return(NA_character_)
  result <- tryCatch(
    system2("git", c("-C", shQuote(path), "rev-parse", "HEAD"), stdout = TRUE),
    error = function(error) character(0L)
  )
  if (length(result)) result[[1L]] else NA_character_
}

complete_menus <- function(universe_size) {
  grids <- rep(list(0:1), universe_size)
  menu <- as.matrix(expand.grid(grids, KEEP.OUT.ATTRS = FALSE))
  storage.mode(menu) <- "integer"
  menu[rowSums(menu) > 0L, , drop = FALSE]
}

aom_population <- function(universe_size) {
  menu <- complete_menus(universe_size)
  prob <- matrix(0, nrow = nrow(menu), ncol = universe_size)
  for (menu_index in seq_len(nrow(menu))) {
    prob[menu_index, min(which(menu[menu_index, ] == 1L))] <- 1
  }
  list(menu = menu, prob = prob)
}

aom_inequality_count <- function(universe_size) {
  sum(vapply(3:universe_size, function(large_size) {
    choose(universe_size, large_size) *
      sum(vapply(2:(large_size - 1L), function(small_size) {
        choose(large_size, small_size) * (small_size - 1L)
      }, numeric(1L)))
  }, numeric(1L)))
}

time_aom <- function(universe_size, pairwise, repetition) {
  population <- aom_population(universe_size)
  gc()
  started <- proc.time()[["elapsed"]]
  fit <- aomIdentify(population$menu, population$prob, pairwise = pairwise)
  total_elapsed <- unname(proc.time()[["elapsed"]] - started)
  diagnostic <- fit$diagnostics[1L, ]
  data.frame(
    model = "AOM",
    universe_size = universe_size,
    repetition = repetition,
    query = if (pairwise) "all-pairs" else "feasibility",
    n_menus = nrow(population$menu),
    n_choice_equations = NA_integer_,
    n_inequalities = aom_inequality_count(universe_size),
    n_possible_rankings = factorial(universe_size),
    n_columns = NA_integer_,
    n_active_columns = NA_integer_,
    n_binary_variables = diagnostic$n_binary_variables,
    n_constraints = diagnostic$n_constraints,
    n_solves = diagnostic$n_milp_solves,
    n_iterations = NA_integer_,
    construction_seconds = max(0, total_elapsed - diagnostic$elapsed),
    solve_seconds = diagnostic$elapsed,
    elapsed_seconds = total_elapsed,
    compatible = fit$compatible,
    certified = diagnostic$all_solver_statuses_resolved,
    lower_bound = NA_real_,
    upper_bound = NA_real_,
    endpoint_match = NA,
    stringsAsFactors = FALSE
  )
}

hlao_population <- function(universe_size) {
  menu <- prob <- matrix(0, nrow = universe_size, ncol = universe_size)
  outside <- rep(0.2, universe_size)
  for (menu_index in seq_len(universe_size)) {
    items <- menu_index:universe_size
    menu[menu_index, items] <- 1
    prob[menu_index, items[[1L]]] <- 0.8
  }
  list(menu = menu, prob = prob, outside = outside)
}

time_hlao <- function(universe_size, algorithm, repetition) {
  population <- hlao_population(universe_size)
  event <- hlaoEvent(
    1L,
    2:universe_size,
    name = "alternative 1 top ranked"
  )
  gc()
  fit <- hlaoModel(
    population$menu,
    population$prob,
    outside_prob = population$outside,
    events = event,
    dependence = "independent",
    algorithm = algorithm,
    max_rankings = max(1, factorial(universe_size)),
    max_iterations = 5000L
  )
  computation <- fit$computation[1L, ]
  compatibility <- fit$compatibility[1L, ]
  is_generated <- identical(computation$algorithm, "column-generation")
  data.frame(
    model = "H-LAO",
    universe_size = universe_size,
    repetition = repetition,
    query = if (is_generated) "column-generation" else "enumeration",
    n_menus = nrow(population$menu),
    n_choice_equations = sum(rowSums(population$menu)),
    n_inequalities = NA_real_,
    n_possible_rankings = factorial(universe_size),
    n_columns = if (is_generated) NA_integer_ else computation$n_columns,
    n_active_columns = if (is_generated) computation$n_columns else NA_integer_,
    n_binary_variables = if (is_generated) computation$pricing_binary_variables else NA_integer_,
    n_constraints = if (is_generated) computation$pricing_constraints else compatibility$n_constraints,
    n_solves = NA_integer_,
    n_iterations = if (is_generated) computation$iterations else NA_integer_,
    construction_seconds = NA_real_,
    solve_seconds = NA_real_,
    elapsed_seconds = fit$elapsed,
    compatible = compatibility$compatible,
    certified = if (is_generated) computation$certified else {
      compatibility$lp_status == 0L
    },
    lower_bound = fit$bounds$lower[[1L]],
    upper_bound = fit$bounds$upper[[1L]],
    endpoint_match = NA,
    stringsAsFactors = FALSE
  )
}

results <- list()
result_index <- 0L
for (repetition in seq_len(options_run$repetitions)) {
  for (universe_size in 3:options_run$aom_max) {
    for (pairwise in c(FALSE, TRUE)) {
      result_index <- result_index + 1L
      results[[result_index]] <- time_aom(universe_size, pairwise, repetition)
      message(
        "Completed AOM p=", universe_size,
        ", query=", results[[result_index]]$query,
        ", repetition=", repetition
      )
    }
  }
  for (universe_size in 3:options_run$hlao_max) {
    for (algorithm in c("enumerate", "column_generation")) {
      result_index <- result_index + 1L
      results[[result_index]] <- time_hlao(universe_size, algorithm, repetition)
      message(
        "Completed H-LAO p=", universe_size,
        ", algorithm=", results[[result_index]]$query,
        ", repetition=", repetition
      )
    }
  }
}

results <- do.call(rbind, results)
row.names(results) <- NULL

hlao_groups <- unique(results[
  results$model == "H-LAO",
  c("universe_size", "repetition"),
  drop = FALSE
])
for (group_index in seq_len(nrow(hlao_groups))) {
  selected <- results$model == "H-LAO" &
    results$universe_size == hlao_groups$universe_size[group_index] &
    results$repetition == hlao_groups$repetition[group_index]
  current <- results[selected, , drop = FALSE]
  enumerated <- current[current$query == "enumeration", , drop = FALSE]
  generated <- current[current$query == "column-generation", , drop = FALSE]
  difference <- max(abs(c(
    enumerated$lower_bound - generated$lower_bound,
    enumerated$upper_bound - generated$upper_bound
  )))
  matched <- is.finite(difference) && difference <= 1e-8
  results$endpoint_match[selected] <- matched
  if (!matched) {
    stop(
      "H-LAO enumeration and column generation disagree for p=",
      hlao_groups$universe_size[group_index],
      ", repetition=", hlao_groups$repetition[group_index],
      ".",
      call. = FALSE
    )
  }
}

replication_root <- normalizePath(file.path(script_dir, ".."), winslash = "/")
ramchoice_root <- Sys.getenv("RAMCHOICE_REPO", unset = NA_character_)
object <- list(
  schema = "2026-07-17-computation-v1",
  pilot = options_run$pilot,
  options = options_run,
  results = results,
  metadata = list(
    started_from = script_dir,
    completed = format(Sys.time(), tz = "UTC", usetz = TRUE),
    replication_commit = safe_git_commit(replication_root),
    ramchoice_commit = if (!is.na(ramchoice_root)) {
      safe_git_commit(ramchoice_root)
    } else {
      NA_character_
    },
    ramchoice_version = as.character(packageVersion("ramchoice")),
    lpSolve_version = as.character(packageVersion("lpSolve")),
    R_version = R.version.string,
    platform = R.version$platform,
    node = unname(Sys.info()[["nodename"]]),
    system = unname(Sys.info()[["sysname"]]),
    release = unname(Sys.info()[["release"]]),
    machine = unname(Sys.info()[["machine"]]),
    tolerance = sqrt(.Machine$double.eps)
  ),
  session_info = utils::sessionInfo()
)

prefix <- if (options_run$pilot) "pilot_" else ""
rds_path <- file.path(
  output_dir,
  paste0(prefix, "CCMM_2026_wp--computation.rds")
)
csv_path <- file.path(
  output_dir,
  paste0(prefix, "CCMM_2026_wp--computation.csv")
)
saveRDS(object, rds_path)
write.csv(results, csv_path, row.names = FALSE)
message("Wrote computational benchmark: ", rds_path)
