# scripts/replicate.R
source(here::here("R","setup.R"))
ensure_main_graphs(); ensure_baselines(); ensure_ergm_fit(); ensure_ergm_sims()

# Now run only lightweight steps that read caches and produce tables/plots:
# source(here::here("analysis","make_tables.R"))
# source(here::here("visualization","plots.R"))