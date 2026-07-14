#!/bin/bash

set -euo pipefail

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
BASE_DIR=$(dirname "$REPO_ROOT")
RAMCHOICE_REPO=${RAMCHOICE_REPO:-"$BASE_DIR/ramchoice"}
RAMCHOICE_REF=${RAMCHOICE_REF:-b49da223774719a98e60a069864e3aee8726a21b}
RAMCHOICE_REMOTE=${RAMCHOICE_REMOTE:-git@github.com:mdcattaneo/ramchoice.git}

module purge
module load R/4.5.1

if [[ ! -d "$RAMCHOICE_REPO/.git" ]]; then
  git clone "$RAMCHOICE_REMOTE" "$RAMCHOICE_REPO"
fi

git -C "$RAMCHOICE_REPO" fetch --tags origin
git -C "$RAMCHOICE_REPO" checkout --detach "$RAMCHOICE_REF"

R_MINOR=$(Rscript --vanilla -e 'cat(paste(R.version$major, strsplit(R.version$minor, "\\.")[[1L]][1L], sep = "."))')
export R_LIBS_USER=${R_LIBS_USER:-"$HOME/R/x86_64-pc-linux-gnu-library/$R_MINOR"}
mkdir -p "$R_LIBS_USER" "$REPO_ROOT/output/della-logs"

Rscript --vanilla \
  "$REPO_ROOT/della/setup-della.R" \
  "$RAMCHOICE_REPO/R/ramchoice" \
  "$R_LIBS_USER"

Rscript --vanilla -e 'library(ramchoice); cat("ramchoice", as.character(packageVersion("ramchoice")), "is ready.\n")'

printf 'Della setup complete.\n'
printf 'Replication repository: %s\n' "$REPO_ROOT"
printf 'ramchoice repository:   %s\n' "$RAMCHOICE_REPO"
printf 'Persistent R library:   %s\n' "$R_LIBS_USER"
