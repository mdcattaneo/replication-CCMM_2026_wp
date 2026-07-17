# Reproduce the incomplete-menu H-LAO example in the Supplemental Appendix.

if (!requireNamespace("lpSolve", quietly = TRUE)) {
  stop("Install the CRAN package 'lpSolve' before running this script.")
}

permutations <- function(x) {
  if (length(x) == 1L) return(matrix(x, nrow = 1L))
  do.call(rbind, lapply(seq_along(x), function(i) {
    tail <- permutations(x[-i])
    cbind(x[i], tail)
  }))
}

menu_key <- function(S) paste(S, collapse = "")

suffix_close <- function(menus) {
  out <- menus
  for (S in menus) {
    for (j in seq_along(S)) out[[menu_key(S[j:length(S)])]] <- S[j:length(S)]
  }
  out[order(vapply(out, function(S) sum(2^(S - 1L)), numeric(1L)))]
}

prefix_masses <- function(S, continuation) {
  reach <- cumprod(continuation[S])
  c(1 - reach[1L], reach[-length(reach)] - reach[-1L], reach[length(reach)])
}

best_in <- function(ranking, S) ranking[match(TRUE, ranking %in% S)]

generate_choice <- function(menus, rankings, tau, continuation) {
  masses <- lapply(menus, prefix_masses, continuation = continuation)
  names(masses) <- names(menus)
  pi <- list()
  for (key in names(menus)) {
    S <- menus[[key]]
    m <- masses[[key]]
    out <- setNames(numeric(length(S) + 1L), c("o", letters[S]))
    out["o"] <- m[1L]
    for (h in seq_len(nrow(rankings))) {
      for (j in seq_along(S)) {
        winner <- best_in(rankings[h, ], S[seq_len(j)])
        out[letters[winner]] <- out[letters[winner]] + tau[h] * m[j + 1L]
      }
    }
    pi[[key]] <- out
  }
  list(pi = pi, masses = masses)
}

independent_system <- function(menus, rankings, masses, pi) {
  rows <- list()
  rhs <- numeric()
  labels <- character()
  for (key in names(menus)) {
    S <- menus[[key]]
    for (a in S) {
      row <- numeric(nrow(rankings))
      for (h in seq_len(nrow(rankings))) {
        for (j in seq_along(S)) {
          if (best_in(rankings[h, ], S[seq_len(j)]) == a) {
            row[h] <- row[h] + masses[[key]][j + 1L]
          }
        }
      }
      rows[[length(rows) + 1L]] <- row
      rhs <- c(rhs, pi[[key]][letters[a]])
      labels <- c(labels, paste0(key, ":", letters[a]))
    }
  }
  list(A = do.call(rbind, rows), b = rhs, labels = labels)
}

lp_event_bounds <- function(A, b, event) {
  const.mat <- rbind(rep(1, length(event)), A)
  const.rhs <- c(1, b)
  const.dir <- rep("=", length(const.rhs))
  lo <- lpSolve::lp("min", event, const.mat, const.dir, const.rhs)
  hi <- lpSolve::lp("max", event, const.mat, const.dir, const.rhs)
  stopifnot(lo$status == 0L, hi$status == 0L)
  c(lower = lo$objval, upper = hi$objval)
}

column_generation_one_side <- function(A, b, objective, tolerance = 1e-8) {
  B <- rbind(rep(1, ncol(A)), A)
  rhs <- c(1, b)
  nrows <- nrow(B)
  selected <- 1L
  iterations <- 0L
  artificial_penalty <- 1e4

  repeat {
    iterations <- iterations + 1L
    master_matrix <- cbind(B[, selected, drop = FALSE], diag(nrows))
    master_objective <- c(
      objective[selected], rep(artificial_penalty, nrows)
    )
    master <- lpSolve::lp(
      "min", master_objective, master_matrix, rep("=", nrows), rhs,
      compute.sens = 1
    )
    stopifnot(master$status == 0L)
    dual <- master$duals[seq_len(nrows)]
    reduced_cost <- objective - drop(crossprod(dual, B))
    reduced_cost[selected] <- Inf
    entering <- which.min(reduced_cost)
    if (reduced_cost[entering] >= -tolerance) break
    selected <- c(selected, entering)
  }

  artificial <- master$solution[length(selected) + seq_len(nrows)]
  if (sum(artificial) > 1e-7) {
    stop("Column generation ended before finding a feasible ranking mixture.")
  }
  list(
    objective = master$objval,
    columns = selected,
    iterations = iterations
  )
}

column_generation_bounds <- function(A, b, event) {
  lo <- column_generation_one_side(A, b, event)
  hi <- column_generation_one_side(A, b, -event)
  list(
    bounds = c(lower = lo$objective, upper = -hi$objective),
    lower_columns = length(lo$columns),
    upper_columns = length(hi$columns),
    lower_iterations = lo$iterations,
    upper_iterations = hi$iterations
  )
}

dependence_robust_system <- function(menus, rankings, masses, pi) {
  nr <- nrow(rankings)
  widths <- vapply(menus, function(S) nr * (length(S) + 1L), integer(1L))
  starts <- nr + c(0L, head(cumsum(widths), -1L)) + 1L
  nvar <- nr + sum(widths)
  rows <- list()
  rhs <- numeric()

  add_constraint <- function(row, value) {
    rows[[length(rows) + 1L]] <<- row
    rhs <<- c(rhs, value)
  }

  row <- numeric(nvar)
  row[seq_len(nr)] <- 1
  add_constraint(row, 1)

  for (k in seq_along(menus)) {
    S <- menus[[k]]
    key <- names(menus)[k]
    nj <- length(S) + 1L
    q_index <- function(h, j) starts[k] + (h - 1L) * nj + j

    for (j in 0:(nj - 1L)) {
      row <- numeric(nvar)
      row[vapply(seq_len(nr), q_index, integer(1L), j = j)] <- 1
      add_constraint(row, masses[[key]][j + 1L])
    }
    for (h in seq_len(nr)) {
      row <- numeric(nvar)
      row[h] <- -1
      row[vapply(0:(nj - 1L), function(j) q_index(h, j), integer(1L))] <- 1
      add_constraint(row, 0)
    }
    for (a in S) {
      row <- numeric(nvar)
      for (h in seq_len(nr)) {
        for (j in seq_along(S)) {
          if (best_in(rankings[h, ], S[seq_len(j)]) == a) {
            row[q_index(h, j)] <- 1
          }
        }
      }
      add_constraint(row, pi[[key]][letters[a]])
    }
  }
  list(A = do.call(rbind, rows), b = rhs, nr = nr, nvar = nvar)
}

dependence_robust_bounds <- function(system, event) {
  objective <- numeric(system$nvar)
  objective[seq_len(system$nr)] <- event
  direction <- rep("=", length(system$b))
  lo <- lpSolve::lp("min", objective, system$A, direction, system$b)
  hi <- lpSolve::lp("max", objective, system$A, direction, system$b)
  stopifnot(lo$status == 0L, hi$status == 0L)
  c(lower = lo$objval, upper = hi$objval)
}

# Alternatives are a, b, c, d and the observed list has this order.
rankings <- permutations(1:4)
seed_menus <- list(c(1), c(2), c(3), c(4), c(1, 2), c(1, 3),
                   c(2, 4), c(1, 2, 4), c(1, 3, 4))
names(seed_menus) <- vapply(seed_menus, menu_key, character(1L))
menus <- suffix_close(seed_menus)
names(menus) <- vapply(menus, menu_key, character(1L))

# Item-dependent continuation generates valid attention overload. The zero for
# c creates observed menus in which c and every later item have zero reach.
continuation <- c(0.86, 0.73, 0, 0.64)

# A fixed, full-support preference distribution keeps the example deterministic.
raw_tau <- c(31, 17, 23, 41, 29, 13, 37, 19, 43, 11, 47, 7,
             5, 53, 3, 59, 2, 61, 67, 71, 73, 79, 83, 89)
tau <- raw_tau / sum(raw_tau)

generated <- generate_choice(menus, rankings, tau, continuation)
system <- independent_system(menus, rankings, generated$masses, generated$pi)
event <- apply(rankings, 1L, function(r) match(4L, r) < match(1L, r))
independent <- lp_event_bounds(system$A, system$b, event)
generated_columns <- column_generation_bounds(system$A, system$b, event)

robust_system <- dependence_robust_system(
  menus, rankings, generated$masses, generated$pi
)
robust <- dependence_robust_bounds(robust_system, event)
truth <- sum(tau[event])

stopifnot(
  max(abs(system$A %*% tau - system$b)) < 1e-8,
  max(abs(generated_columns$bounds - independent)) < 1e-7,
  independent["lower"] <= truth + 1e-8,
  independent["upper"] >= truth - 1e-8,
  robust["lower"] <= independent["lower"] + 1e-8,
  robust["upper"] >= independent["upper"] - 1e-8
)

choice_table <- do.call(rbind, lapply(names(menus), function(key) {
  S <- menus[[key]]
  p <- generated$pi[[key]]
  data.frame(
    menu = paste0("{", paste(letters[S], collapse = ","), "}"),
    o = unname(p["o"]),
    a = if ("a" %in% names(p)) unname(p["a"]) else NA,
    b = if ("b" %in% names(p)) unname(p["b"]) else NA,
    c = if ("c" %in% names(p)) unname(p["c"]) else NA,
    d = if ("d" %in% names(p)) unname(p["d"]) else NA
  )
}))

cat("Observed menus:", length(menus), "of", 2^4 - 1, "nonempty menus\n")
cat("Rankings:", nrow(rankings), "\n")
cat("True Pr(d > a):", sprintf("%.6f", truth), "\n")
cat("Independent H-LAO bounds:",
    paste(sprintf("%.6f", independent), collapse = ", "), "\n")
cat("Columns used for lower/upper bounds:",
    generated_columns$lower_columns, "/", generated_columns$upper_columns,
    "of", nrow(rankings), "\n")
cat("Dependence-robust bounds:",
    paste(sprintf("%.6f", robust), collapse = ", "), "\n\n")
print(choice_table, row.names = FALSE, digits = 6)
