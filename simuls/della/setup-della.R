args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2L) {
  stop(
    "Usage: Rscript setup-della.R <ramchoice-package-path> <user-library>",
    call. = FALSE
  )
}

package_path <- normalizePath(args[[1L]], mustWork = TRUE)
user_library <- path.expand(args[[2L]])
dir.create(user_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(user_library, .libPaths()))

cran <- "https://cloud.r-project.org"
dependencies <- c("lpSolve", "MASS")
missing <- dependencies[
  !vapply(dependencies, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1L))
]
if (length(missing)) {
  install.packages(missing, repos = cran, lib = user_library)
}

install.packages(
  package_path,
  repos = NULL,
  type = "source",
  lib = user_library
)

if (!requireNamespace("ramchoice", quietly = TRUE)) {
  stop("ramchoice was not installed successfully.", call. = FALSE)
}

message(
  "Installed ramchoice ", as.character(utils::packageVersion("ramchoice")),
  " in ", user_library
)
