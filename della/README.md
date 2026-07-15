# Della simulation workflow

The production simulation is split into 80 independent design checkpoints.
A Slurm array computes one checkpoint per task. A dependent assembly job runs
only after every task succeeds; it creates the three raw manifests and renders
the final tables and figures. Array tasks never write a shared manifest.

## 1. Connect and choose scratch storage

From a local terminal, connect to Della and complete Princeton authentication:

```bash
ssh -o MACs=hmac-sha2-512 cattaneo@della.princeton.edu
```

The verified sponsor scratch directory for this project is `CATTANEO` (path
names are case-sensitive). Create a personal project directory there:

```bash
export CCMM_BASE=/scratch/gpfs/CATTANEO/$USER
mkdir -p "$CCMM_BASE" && cd "$CCMM_BASE"
```

If access changes, use `checkquota`, `sshare -U -u "$USER"`, and the writable
directories under `/scratch/gpfs/` to verify the sponsor path before continuing.

## 2. Clone and set up persistent R packages

The replication repository is public, but `ramchoice` is private. Configure a
GitHub SSH key on Della before running setup. First inspect existing keys and
create an Ed25519 key only if one is not already present:

```bash
ls -la ~/.ssh
test -f ~/.ssh/id_ed25519 || \
  ssh-keygen -t ed25519 -C "cattaneo@princeton.edu"
cat ~/.ssh/id_ed25519.pub
```

Add the displayed public key as an authentication key at
<https://github.com/settings/ssh/new>, signing into GitHub through Google in a
local browser. Then verify the connection from Della:

```bash
ssh -T git@github.com
```

Accept GitHub's host key if prompted. A successful test identifies the GitHub
account and notes that GitHub does not provide shell access.

Now clone and set up the project:

```bash
git clone https://github.com/mdcattaneo/replication-CCMM_2026_wp.git && \
  cd replication-CCMM_2026_wp && \
  bash della/setup-della.sh
```

The setup script loads `R/4.5.1`, clones the tested `ramchoice` commit over SSH
into a
sibling directory, installs dependencies and `ramchoice` in the persistent R
library under your home directory, and creates the Slurm log directory. It does
not use a temporary or repository-local R library.

For a later update of an existing checkout:

```bash
cd "$CCMM_BASE/replication-CCMM_2026_wp"
git pull --ff-only
bash della/setup-della.sh
```

## 3. Run the smoke test

```bash
bash della/submit-smoke.sh
```

This submits one-replication tasks for the first homogeneous-AOM and H-LAO
designs, the no-SPI H11 design, and the first diagnostic design. The command
prints the job ID. Monitor it with the printed `squeue` and `sacct` commands,
and inspect `output/della-logs/`. All four array tasks should finish with state
`COMPLETED` and exit code `0:0` before production.

## 4. Submit production

```bash
bash della/submit-production.sh
```

The wrapper derives the array size from the design table, caps concurrent tasks
at 60, and submits the assembly job with an `afterok` dependency. Reissuing the
workflow resumes matching per-design checkpoints. Production uses 2,000 Monte
Carlo replications and 2,000 critical-value draws unless the simulation driver
is changed explicitly.

Monitor the job IDs printed by the wrapper:

```bash
squeue --me
sacct -j <array-job-id>,<assembly-job-id> \
  --format=JobID,State,Elapsed,MaxRSS,ExitCode
```

Successful assembly creates `output/della-production-complete.txt`, the raw
manifests and checkpoints under `output/`, and the rendered files under
`tables/` and `figures/`. These generated outputs remain untracked.

### Incremental H-LAO extension

When the homogeneous-AOM and diagnostic production outputs already exist, run
only the expanded H-LAO block with:

```bash
bash della/submit-hlao-extension.sh
```

This submits the 44 H-LAO tasks, including H11, and then assembles the H-LAO
manifest and rerenders all tables and figures. It does not rerun the 21
homogeneous-AOM or 15 diagnostic tasks.

## 5. Retrieve results

The Princeton Open OnDemand file browser is the simplest way to download the
three generated directories. Alternatively, run `scp` from the local computer,
using the same explicit MAC option if the Windows SSH client needs it. Do not
move or delete the Della copy until the manifests, checkpoint directories,
tables, and figures have all been retrieved and checked locally.
