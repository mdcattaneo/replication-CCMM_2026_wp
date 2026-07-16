#!/bin/bash
#SBATCH --job-name=ccmm-assemble
#SBATCH --time=04:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --output=output/della-logs/%x-%j.out
#SBATCH --error=output/della-logs/%x-%j.err

set -euo pipefail

module purge
module load R/4.5.1

cd "$SLURM_SUBMIT_DIR"
GIT_ROOT=$(git rev-parse --show-toplevel)
RAMCHOICE_REPO=${RAMCHOICE_REPO:-"$(dirname "$GIT_ROOT")/ramchoice"}
R_MINOR=$(Rscript --vanilla -e 'cat(paste(R.version$major, strsplit(R.version$minor, "\\.")[[1L]][1L], sep = "."))')
export R_LIBS_USER=${R_LIBS_USER:-"$HOME/R/x86_64-pc-linux-gnu-library/$R_MINOR"}
export RAMCHOICE_GIT_SHA=$(git -C "$RAMCHOICE_REPO" rev-parse HEAD)
export REPLICATION_GIT_SHA=$(git -C "$GIT_ROOT" rev-parse HEAD)

IFS=',' read -r -a blocks <<< "${CCMM_BLOCKS:-homogeneous-aom,hlao,hlao-diagnostic}"
for block in "${blocks[@]}"; do
  Rscript --vanilla CCMM_2026_wp--simuls.R \
    "--block=$block" \
    --assemble-only
done

Rscript --vanilla CCMM_2026_wp--tables.R
Rscript --vanilla CCMM_2026_wp--figures.R

{
  printf 'Completed: %s\n' "$(date --iso-8601=seconds)"
  printf 'Slurm job: %s\n' "$SLURM_JOB_ID"
  printf 'Replication commit: %s\n' "$REPLICATION_GIT_SHA"
  printf 'ramchoice commit: %s\n' "$RAMCHOICE_GIT_SHA"
} > output/della-production-complete.txt
