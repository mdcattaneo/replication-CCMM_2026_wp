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

The homogeneous-AOM simulation engine reproduces the seven menu-support
designs, three menu-specific sample sizes, and four preference hypotheses
reported in the supplemental appendix. The H-LAO engine implements the ten
planned heterogeneous-preference configurations at four sample sizes. The
separate diagnostic block remains design-only.

To inspect the complete planned Monte Carlo design without running simulations,
use:

```text
Rscript CCMM_2026_wp--simuls.R --design-only
```

To run the 25-replication homogeneous-AOM pilot with 200 critical-value draws,
use:

```text
Rscript CCMM_2026_wp--simuls.R --pilot --block=homogeneous-aom
```

To run the 25-replication H-LAO pilot, use:

```text
Rscript CCMM_2026_wp--simuls.R --pilot --block=hlao
```

H-LAO output records plug-in reach and full-attention estimates, weak-reach
pairwise intervals, dependence-robust event intervals, population identified
bounds, compatibility diagnostics, and runtime. Every sample is analyzed with
both finite-sample Hoeffding bands and covariance-aware correlated-Gaussian
bands; `--critical-draws` controls the Gaussian simulation. General-event LPs
are used for the four-alternative designs; the six-alternative sparse designs
use the ranking-free pairwise procedure.

For a quick smoke run, the replication and critical-draw counts can be
overridden explicitly:

```text
Rscript CCMM_2026_wp--simuls.R --pilot --block=hlao --replications=1 --critical-draws=100
```

Pilot output is written to
`output/CCMM_2026_wp--raw-homogeneous-aom--pilot.rds`; production output omits
the `--pilot` suffix. Raw output is local by default.

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
