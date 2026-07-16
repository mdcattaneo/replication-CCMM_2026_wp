# Della Simulation Workflow

This directory runs the simulation workflow under `simuls/` on Princeton's
Della cluster. Array tasks write one checkpoint per design. A dependent
assembly job creates manifests and renders the final tables and figures only
after every requested task succeeds.

## Login and Repositories

```bash
ssh -o MACs=hmac-sha2-512 cattaneo@della.princeton.edu
export WORK=/scratch/gpfs/CATTANEO/cattaneo
mkdir -p "$WORK"
cd "$WORK"
```

The replication repository is public. Clone it with HTTPS if it is absent:

```bash
git clone https://github.com/mdcattaneo/replication-CCMM_2026_wp.git
```

The development `ramchoice` repository is a sibling of the replication
repository. Configure SSH access to GitHub before the first setup run because
the package repository may require authentication.

## One-Time Setup

```bash
cd "$WORK/replication-CCMM_2026_wp"
git pull --ff-only
bash simuls/della/setup-della.sh
```

The setup script loads `R/4.5.1`, installs required packages and the checked
`ramchoice` development commit into the persistent versioned user library under
`$HOME/R/`, and creates `simuls/output/della-logs/`. It does not use a
repository-local or temporary R library.

Run setup again whenever the required `ramchoice` commit changes:

```bash
git pull --ff-only
bash simuls/della/setup-della.sh
```

## Smoke Test

```bash
bash simuls/della/submit-smoke.sh
```

The smoke array runs one replication for four representative designs. Monitor
the reported job identifier with `squeue` and inspect
`simuls/output/della-logs/`. Every task should complete with exit code zero and
write a checkpoint without creating a shared manifest.

## Production Run

```bash
bash simuls/della/submit-production.sh
```

The submission script creates the design table, submits one array task per
design, and submits a dependent assembly job. It records both job identifiers
in `simuls/output/della-production-jobs.txt`.

Monitor the jobs with the command printed by the submission script, or use:

```bash
squeue -u "$USER"
sacct -j ARRAY_JOB,ASSEMBLY_JOB --format=JobID,State,Elapsed,MaxRSS,ExitCode
```

Successful assembly creates:

- raw manifests and checkpoints under `simuls/output/`;
- paper-ready tables under `simuls/tables/`;
- paper-ready figures under `simuls/figures/`; and
- `simuls/output/della-production-complete.txt`.

Generated files remain untracked.

## H-LAO Extension Only

When completed homogeneous-AOM and diagnostic outputs are already present, run
only the expanded H-LAO block with:

```bash
bash simuls/della/submit-hlao-extension.sh
```

The dependent assembly job updates the H-LAO manifest and rerenders all tables
and figures without rerunning the other blocks.

## Retrieval

After assembly succeeds, retrieve `simuls/output/`, `simuls/tables/`, and
`simuls/figures/` to the corresponding local folders. Keep the checkpoint
directory with each lightweight manifest. Verify all design identifiers and
commit hashes before copying paper-ready artifacts into the paper repository.

Do not pull a reorganized repository or change the checked package commit while
an existing Slurm array or assembly job is still running in that worktree.
