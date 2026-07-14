#!/bin/bash
#SBATCH --job-name=ccmm-sim
#SBATCH --time=23:59:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --output=output/della-logs/%x-%A_%a.out
#SBATCH --error=output/della-logs/%x-%A_%a.err

set -euo pipefail

module purge
module load R/4.5.1

cd "$SLURM_SUBMIT_DIR"
RAMCHOICE_REPO=${RAMCHOICE_REPO:-"$(dirname "$SLURM_SUBMIT_DIR")/ramchoice"}
R_MINOR=$(Rscript --vanilla -e 'cat(paste(R.version$major, strsplit(R.version$minor, "\\.")[[1L]][1L], sep = "."))')
export R_LIBS_USER=${R_LIBS_USER:-"$HOME/R/x86_64-pc-linux-gnu-library/$R_MINOR"}
export RAMCHOICE_GIT_SHA=$(git -C "$RAMCHOICE_REPO" rev-parse HEAD)
export REPLICATION_GIT_SHA=$(git rev-parse HEAD)

args=("--array-task=$SLURM_ARRAY_TASK_ID" "--checkpoint-only")
if [[ ${CCMM_PILOT:-0} == 1 ]]; then
  args+=("--pilot")
fi
if [[ -n ${CCMM_REPLICATIONS:-} ]]; then
  args+=("--replications=$CCMM_REPLICATIONS")
fi
if [[ -n ${CCMM_CRITICAL_DRAWS:-} ]]; then
  args+=("--critical-draws=$CCMM_CRITICAL_DRAWS")
fi

Rscript --vanilla CCMM_2026_wp--simuls.R "${args[@]}"
