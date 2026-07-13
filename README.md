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

The new pipeline is under development. To inspect its planned Monte Carlo
design without running simulations, use:

```text
Rscript CCMM_2026_wp--simuls.R --design-only
```

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
