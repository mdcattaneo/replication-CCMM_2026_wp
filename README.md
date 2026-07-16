# Replication: Cattaneo, Cheung, Ma, and Masatlioglu (2026)

Replication, simulation, and empirical-application code for *Attention
Overload*. The repository is organized into two independent workflows:

- [`simuls/`](simuls/) contains Monte Carlo experiments, computational
  benchmarks, rendering scripts, Della orchestration, and archived legacy
  simulation baselines.
- [`empapp/`](empapp/) contains code for the travel-mode empirical application.
  The restricted subject-level data are not included.

## Software

Both workflows use the development version of the R package
[`ramchoice`](https://github.com/mdcattaneo/ramchoice). Install the package and
all required dependencies in a persistent user R library before running this
replication. The analysis scripts call public package interfaces only.

## Simulations

The current simulation pipeline has three entry points:

```text
Rscript simuls/CCMM_2026_wp--simuls.R
Rscript simuls/CCMM_2026_wp--tables.R
Rscript simuls/CCMM_2026_wp--figures.R
```

The simulation driver writes raw results to `simuls/output/`. The two rendering
scripts read only those saved results and write paper-ready artifacts to
`simuls/tables/` and `simuls/figures/`. Generated artifacts and raw output are
local by default.

To inspect the complete Monte Carlo design without running it:

```text
Rscript simuls/CCMM_2026_wp--simuls.R --design-only
```

For local Windows production runs, use:

```text
simuls\0000_run-production.cmd
```

The launcher runs the homogeneous AOM, H-LAO estimation and inference, and
H-LAO diagnostic blocks concurrently. Designs are checkpointed separately, so
an interrupted run can resume without overwriting completed production output.

For Princeton's Della cluster, start with
[`simuls/della/README.md`](simuls/della/README.md). The tracked Slurm workflow
uses one array task per design and a dependent assembly job that creates the
raw manifests, tables, and figures only after all requested tasks succeed.

The folders [`simuls/legacy/AOM/`](simuls/legacy/AOM/) and
[`simuls/legacy/HAOM/`](simuls/legacy/HAOM/) contain the earlier simulation
code and outputs. They are retained as numerical and design baselines for the
2026 pipeline.

## Empirical Application

The empirical workflow analyzes the travel-mode experiment of Wang and Zhu.
It treats travel modes A--D as inside alternatives and the dominated Default as
the outside option. The code constructs the complete-design and common-menu
samples, estimates H-LAO preference objects, compares them with ex post
rankings, and summarizes observed information acquisition.

The subject-level workbook was shared under a temporary restriction and is not
part of this repository. Until the data providers authorize redistribution,
place an approved local copy outside Git and pass its path explicitly:

```text
Rscript empapp/CCMM_2026_wp--empapp.R --data="C:/path/to/data_clean.xlsx"
Rscript empapp/CCMM_2026_wp--empapp-tables.R
```

The analysis script writes aggregate-only results to `empapp/output/`; the
renderer writes LaTeX tables to `empapp/tables/`. Both directories and
`empapp/data/` are ignored by Git. The workflow reports row-i.i.d. benchmarks
and subject-clustered inference side by side. The clustered analysis estimates
the joint covariance of all menu--outcome frequencies, uses cluster multiplier
calibration, and propagates the resulting simultaneous region through the AOM
and H-LAO procedures.

## Paper Artifacts

Paper-ready simulation tables and figures are copied from `simuls/tables/` and
`simuls/figures/` into the paper repository. Aggregate empirical tables are
copied from `empapp/tables/`; the restricted workbook and subject-level output
never leave their approved local location.

## Reference

Cattaneo, Matias D., Paul H.Y. Cheung, Xinwei Ma, and Yusufcan Masatlioglu.
2026. "Attention Overload." Manuscript under revision.
