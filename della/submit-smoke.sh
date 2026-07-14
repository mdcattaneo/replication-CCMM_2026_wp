#!/bin/bash

set -euo pipefail

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$REPO_ROOT"
mkdir -p output/della-logs

submission=$(sbatch \
  --parsable \
  --array=1,22,62 \
  --export=ALL,CCMM_PILOT=1,CCMM_REPLICATIONS=1,CCMM_CRITICAL_DRAWS=100 \
  della/CCMM_2026_wp--simulations.slurm)
job_id=${submission%%;*}

printf '%s\n' "$job_id" > output/della-smoke-job.txt
printf 'Submitted smoke-test array job %s (tasks 1, 22, and 62).\n' "$job_id"
printf 'Monitor with: squeue -j %s\n' "$job_id"
printf 'After completion: sacct -j %s --format=JobID,State,Elapsed,MaxRSS,ExitCode\n' "$job_id"
