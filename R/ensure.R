suppressPackageStartupMessages({ library(here) })

.ensure <- function(path, wanted, envir = parent.frame()){
  if (all(vapply(wanted, exists, logical(1), envir = envir, inherits = TRUE))) return(invisible(TRUE))
  if (!file.exists(path)) stop("Missing cache: ", path, call. = FALSE)
  obj  <- readRDS(path)
  keep <- intersect(names(obj), wanted)
  if (!length(keep)) stop("Cache found but none of the requested objects are inside: ", path, call. = FALSE)
  list2env(obj[keep], envir = envir)
  invisible(TRUE)
}

ensure_main_graphs   <- function(vars = c("g", "g_l_s","g_wire", "g_sib", "net_l_s", "net_wire","net_sib")) {
  .ensure(here::here("data/derived/graphs","main_graphs.rds"), vars)
}
ensure_baselines     <- function(vars = c("g_er","g_er_sub","swg","swg_sub","g_pa", "g_pa_sub")) {
  .ensure(here::here("data/derived/graphs","baseline_graphs.rds"), vars)
}
ensure_ergm_fit      <- function(vars = "fit_wire2") {
  .ensure(here::here("data/derived/models","ergm_fit.rds"), vars)
}
ensure_ergm_sims     <- function(vars = "sims", "obs") {
  .ensure(here::here("data/derived/models","ergm_sims.rds"), vars)
}


cache_write <- function(obj, path, opt = "update_cache") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path) && !isTRUE(getOption(opt))) {
    message("Cache exists, not overwriting: ", path,
            " (set options(", opt, "=TRUE) to overwrite)")
    return(invisible(FALSE))
  }
  saveRDS(obj, path)
  message("Wrote cache: ", path)
  invisible(TRUE)
}