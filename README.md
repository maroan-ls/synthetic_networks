## README

This is a repository for supplementary code material for the Master Thesis "Synthetic Networks in Finance and Economy: Constructing financial networks". 

# Replication Guide

## Requirements
- R ≥ 4.3
- RStudio (recommended)
- Internet access to install packages

## Setup (one time)
1. **Open the project** by double-clicking the `*.Rproj` file at the repo root.
2. **Restore packages** EASY run:
   ```r
   source("R/replicate.R")
   ```
3. **Alternatively** directly with `{renv}` (this does the same as above but gives more control:
   ```r
   install.packages("renv")
   renv::restore()
   ```

## Quick Start (lightweight, uses cached results)
Run the replication driver. This **does not** recompute the slow ERGM fit; it loads cached artifacts and runs only lightweight steps.

```r
source("R/replicate.R")
```

Outputs (tables/plots) should be created by the analysis scripts and any caches should be read from:

- `data/derived/graphs/` – main graphs + baseline/null graphs  
- `data/derived/models/` – ERGM fit + simulations  
- `data/derived/metrics/` – (optional) heavy, reused analysis artifacts

## Notes on Caches
- Caches are `.rds` files written by the “producer” scripts (e.g., `init_maingraph.R`, `compute_GSIBW2_fit.R`).
- If caches are included in the repo (or via Release/LFS), no heavy compute is needed.
- If a cache is missing, lightweight scripts should error with a message (what’s missing and where it is expected).

## (Optional) Refreshing Caches Intentionally
Only do this if you **want** to overwrite existing caches. The ERGM refit can take a long time.

```r
# Allow cache files to be overwritten by producers
options(update_cache = TRUE)

# Refit ERGM (very slow); will write data/derived/models/ergm_fit.rds
options(recompute_fit = TRUE, update_cache = TRUE)
source("path/to/fit_ergm.R")

# Re-run simulations after a new fit (writes data/derived/models/ergm_sims.rds)
source("path/to/simulate_from_fit.R")
```

## Re-running Analyses
Analyses and figures can be rebuilt (reading caches) with:

```r
# Loads caches and runs lightweight analysis
source("R/replicate.R")
```
The `.ensure` implementation should be able to run without a dedicated library() to any package.  

## Reproducability Example
How this should look in Detail:

1. After running `replicate.R`, you should see variables in your Global Environment.
2. If successful, you should be able to run or replicate any analysis script.
3. You can use the real graph models `g`, `g_l_s` (Glcc) and `g_wire` (GSIBW2) for further empiric work. 
4. You can also do this with the baselines, or the fitted ERGM `fit_wire2`, or their simulations.

**Warning**: Running the ERGM fitting producing script again just to reproduce it takes considerable time to run,
it is therefore not recommended to do so.
Plots/Visualizations also take considerable time. 


## Reproducibility Details
- Package versions are pinned by `renv.lock`. Use `renv::restore()` to recreate the environment.
- Paths are project-rooted via `here::here()`, so no manual `setwd()` is needed.
- Heavy stage outputs are cached; downstream scripts load them on demand using small helpers in `R/ensure.R`.
- This also ensures, that even as the raw data is not there, the graph is stored for subsequent analysis

## Troubleshooting
- **Package install issues:** run `renv::restore()` again; if needed, update R to the latest minor release.
- **Missing cache error:** ensure the `.rds` files exist under `data/derived/**`. If not shipped in the repo, generate them again with the steps above (not recommended, as ERGM fit takes roughly a day)


This is also published on Zenodo to create a DOI for the Thesis:


[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15106167.svg)](https://doi.org/10.5281/zenodo.15106167)

