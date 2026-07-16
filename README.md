# Replication: Cattaneo, Cheung, Ma, and Masatlioglu (2026)

Replication and simulation files for *Attention Overload*. This repository is
being updated for the paper's 2026 revision.

## R workflow

The current simulation pipeline has three entry points:

- `CCMM_2026_wp--simuls.R` runs the Monte Carlo experiments and writes raw
  results to `output/`.
- `CCMM_2026_wp--tables.R` reads raw results and writes paper-ready LaTeX
  tables to `tables/`.
- `CCMM_2026_wp--figures.R` reads raw results and writes paper-ready figures to
  `figures/`.

Run them from the repository root in this order:

```text
Rscript CCMM_2026_wp--simuls.R
Rscript CCMM_2026_wp--tables.R
Rscript CCMM_2026_wp--figures.R
```

## Production run

The production design uses 2,000 Monte Carlo replications and 2,000 simulated
critical-value draws. On Windows, launch the complete run with:

```text
0000_run-production.cmd
```

The launcher runs the homogeneous AOM, H-LAO estimation/inference, and H-LAO
diagnostic blocks concurrently. It records the `ramchoice` and replication Git
commits, writes separate logs under `output/production-logs/`, waits for all
three blocks, and then renders the production tables and figures. It does not
copy, commit, or push generated files.

Each design is checkpointed separately under `output/checkpoints/`. Reissuing
the same command with the same design schema, replication count, critical-draw
count, and `ramchoice` commit resumes completed designs instead of rerunning
them. Thus an interrupted production run can be restarted with the same
one-line command.

For Princeton's Della cluster, the tracked [`della/`](della/) workflow submits
the 80 designs as independent Slurm array tasks and renders results in a
dependent assembly job. Start with [`della/README.md`](della/README.md); it
contains the one-time setup, three-design smoke test, production submission,
monitoring, and retrieval commands.

For production runs, the block-level raw `.rds` file is a lightweight manifest
that records metadata and the relative paths of its per-design raw
checkpoints. The table and figure scripts read one design at a time, avoiding a
single multi-million-row object in memory. Keep the manifest and checkpoint
directory together when archiving raw results.

The homogeneous-AOM simulation engine reproduces the seven menu-support
designs, three menu-specific sample sizes, and four preference hypotheses
reported in the supplemental appendix. Each AOM replication records the
all-inequalities least-favorable (`LF`) procedure corresponding directly to
Theorem 9 and its GMS refinement from the same Gaussian draws; the table
renderer reports the two procedures separately. The H-LAO engine implements
eleven heterogeneous-preference configurations at four sample sizes. H11
preserves prefix consideration and attention overload while violating
Sequential Path Independence. The separate diagnostic block implements
calibrated null, local, and fixed
alternatives for attention-overload and Block--Marschak restrictions.

To inspect the complete planned Monte Carlo design without running simulations,
use:

```text
Rscript CCMM_2026_wp--simuls.R --design-only
```

The generated design table includes the stable `array_task` index used by the
Della job array. Cluster tasks may also select a design explicitly with
`--design-id`, but both selectors are restricted to `--checkpoint-only` mode.
The `--assemble-only` mode fails if any expected checkpoint is absent.

To run the 25-replication homogeneous-AOM pilot with 200 critical-value draws,
use:

```text
Rscript CCMM_2026_wp--simuls.R --pilot --block=homogeneous-aom
```

To run the 25-replication H-LAO pilot, use:

```text
Rscript CCMM_2026_wp--simuls.R --pilot --block=hlao
```

To run the 25-replication H-LAO specification-diagnostic pilot, use:

```text
Rscript CCMM_2026_wp--simuls.R --pilot --block=hlao-diagnostic
```

H-LAO output records plug-in reach and full-attention estimates, projection
and studentized undivided-moment pairwise confidence sets,
dependence-robust and no-SPI event intervals, population identified bounds,
compatibility diagnostics, and runtime. Every sample is analyzed with both
finite-sample Hoeffding bands and covariance-aware correlated-Gaussian bands;
`--critical-draws` controls the Gaussian simulation. No-SPI event projections
are evaluated in the targeted H01, H05, H07, and H11 configurations.
General-event LPs are used for the four-alternative designs; the
six-alternative sparse designs use the ranking-free pairwise procedure.

The diagnostic block studies simultaneous attention-overload and
Block--Marschak specification checks under a valid null and local or fixed
violations. It compares finite-sample Hoeffding outer diagnostics,
hybrid Gaussian/exact-binomial outer diagnostics, and direct delta-Gaussian
diagnostics. The direct method is used in this regular complete-menu,
positive-reach design; the outer methods remain available when regularity is
weak.

For a quick smoke run, the replication and critical-draw counts can be
overridden explicitly:

```text
Rscript CCMM_2026_wp--simuls.R --pilot --block=hlao --replications=1 --critical-draws=100
```

Pilot output is written to
`output/CCMM_2026_wp--raw-homogeneous-aom--pilot.rds`; production output omits
the `--pilot` suffix. Raw output is local by default.

Pilot tables and figures can be regenerated with:

```text
Rscript CCMM_2026_wp--tables.R --pilot
Rscript CCMM_2026_wp--figures.R --pilot
```

Their filenames begin with `pilot_`; the paper-repository copy helper excludes
these scratch artifacts. Production renderers create four LaTeX tables and two
PDF figures without the prefix.

## Software

The replication uses the development version of the R package
[`ramchoice`](https://github.com/mdcattaneo/ramchoice). The package is being
expanded to implement the homogeneous- and heterogeneous-preference methods in
the paper while preserving compatibility with the earlier JPE application.

## Legacy code

The `AOM/` and `HAOM/` directories contain earlier simulation code and results.
They are retained as numerical and design baselines while the 2026 pipeline is
developed.

## Reference

Cattaneo, Matias D., Paul H.Y. Cheung, Xinwei Ma, and Yusufcan Masatlioglu.
2026. "Attention Overload." Manuscript under revision.
