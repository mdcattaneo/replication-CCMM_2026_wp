#!/bin/bash

set -euo pipefail

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$REPO_ROOT"
RAMCHOICE_REPO=${RAMCHOICE_REPO:-"$(dirname "$REPO_ROOT")/ramchoice"}
export RAMCHOICE_REPO

if ! git diff --quiet || ! git diff --cached --quiet; then
  printf 'The replication repository has tracked, uncommitted changes. Commit them first.\n' >&2
  exit 1
fi
if ! git -C "$RAMCHOICE_REPO" diff --quiet || \
   ! git -C "$RAMCHOICE_REPO" diff --cached --quiet; then
  printf 'The ramchoice repository has tracked, uncommitted changes. Commit them first.\n' >&2
  exit 1
fi

module purge
module load R/4.5.1
R_MINOR=$(Rscript --vanilla -e 'cat(paste(R.version$major, strsplit(R.version$minor, "\\.")[[1L]][1L], sep = "."))')
export R_LIBS_USER=${R_LIBS_USER:-"$HOME/R/x86_64-pc-linux-gnu-library/$R_MINOR"}
Rscript --vanilla -e 'library(ramchoice); cat("Using ramchoice", as.character(packageVersion("ramchoice")), "\n")'

mkdir -p output/della-logs tables figures
Rscript --vanilla CCMM_2026_wp--simuls.R --design-only
task_spec=$(Rscript --vanilla -e 'd <- read.csv("output/CCMM_2026_wp--design.csv"); cat(paste(d$array_task[d$block == "hlao"], collapse = ","))')
task_count=$(Rscript --vanilla -e 'd <- read.csv("output/CCMM_2026_wp--design.csv"); cat(sum(d$block == "hlao"))')

array_submission=$(sbatch \
  --parsable \
  --array="${task_spec}%44" \
  della/CCMM_2026_wp--simulations.slurm)
array_job=${array_submission%%;*}

assembly_submission=$(sbatch \
  --parsable \
  --dependency="afterok:$array_job" \
  --export=ALL,CCMM_BLOCKS=hlao \
  della/CCMM_2026_wp--assemble.slurm)
assembly_job=${assembly_submission%%;*}

{
  printf 'array_job=%s\n' "$array_job"
  printf 'assembly_job=%s\n' "$assembly_job"
  printf 'task_count=%s\n' "$task_count"
  printf 'blocks=hlao\n'
  printf 'replication_commit=%s\n' "$(git rev-parse HEAD)"
  printf 'ramchoice_commit=%s\n' "$(git -C "$RAMCHOICE_REPO" rev-parse HEAD)"
} > output/della-hlao-extension-jobs.txt

printf 'Submitted %s H-LAO tasks as array job %s.\n' "$task_count" "$array_job"
printf 'Assembly job %s will update the H-LAO manifest and render all artifacts.\n' "$assembly_job"
printf 'Monitor with: squeue -j %s,%s\n' "$array_job" "$assembly_job"
