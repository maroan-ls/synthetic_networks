# --- requirements
library(igraph)
library(intergraph)

# Convert statnet::network -> igraph weighted+directed
.as_igraph <- function(net, w_attr = "w_scale") {
  g <- if (inherits(net, "igraph")) net else intergraph::asIgraph(net)
  w <- as.numeric(edge_attr(g, w_attr))
  if (is.null(w)) stop(sprintf("Edge attribute '%s' not found.", w_attr))
  list(g = g, w = w)
}

# Ranks: high value -> small rank number (1 = highest)
.rank_desc <- function(x) rank(-x, ties.method = "average")

# Strength (weighted degree)
.strength <- function(g, w, mode = c("out","in")) {
  strength(g, mode = match.arg(mode), weights = w)
}

# PageRank (weighted, directed)
.pr_centrality <- function(g, w) page_rank(g, directed = TRUE, weights = w)$vector

# Top-k identity stability for banks that are top-k in the observed network
# Top-k identity stability for banks that are top-k in the observed network
.topk_stability <- function(obs_rank, sim_rank_mat, k = 5) {
  # obs_rank: numeric ranks (1 = highest) length n
  # sim_rank_mat: n x nsim ranks for each sim (same convention)
  obs_topk <- head(order(obs_rank), k)          # indices of observed top-k
  # For each simulation, compute its top-k set correctly:
  sim_topk_sets <- apply(sim_rank_mat, 2, function(r) head(order(r), k))
  if (!is.matrix(sim_topk_sets)) sim_topk_sets <- matrix(sim_topk_sets, nrow = k)
  # For each observed top-k bank, fraction of sims where it remains top-k
  keep_frac <- sapply(obs_topk, function(i) {
    mean(apply(sim_topk_sets, 2, function(S) i %in% S))
  })
  c(mean = mean(keep_frac), median = median(keep_frac),
    min = min(keep_frac), max = max(keep_frac))
}

# ---- Main summary: returns a data.frame you can kable() ----
centrality_summary <- function(obs, sims, w_attr = "w_scale", ks = c(5,10)) {
  if (inherits(sims, "network")) sims <- list(sims)
  P <- .as_igraph(obs, w_attr); g0 <- P$g; w0 <- P$w
  
  # Observed centralities and ranks (1 = most central)
  out_obs <- .strength(g0, w0, "out"); r_out_obs <- .rank_desc(out_obs)
  in_obs  <- .strength(g0, w0, "in");  r_in_obs  <- .rank_desc(in_obs)
  pr_obs  <- .pr_centrality(g0, w0);   r_pr_obs  <- .rank_desc(pr_obs)
  
  # Sim centralities (n x nsim)
  get_mat <- function(fun) do.call(cbind, lapply(sims, function(net) {
    P <- .as_igraph(net, w_attr); fun(P$g, P$w)
  }))
  out_mat <- get_mat(function(g,w) .strength(g,w,"out"))
  in_mat  <- get_mat(function(g,w) .strength(g,w,"in"))
  pr_mat  <- get_mat(function(g,w) .pr_centrality(g,w))
  
  # Sim ranks (n x nsim)
  r_out_mat <- apply(out_mat, 2, .rank_desc)
  r_in_mat  <- apply(in_mat,  2, .rank_desc)
  r_pr_mat  <- apply(pr_mat,  2, .rank_desc)
  
  # Median sim ranks (per node)
  r_out_med <- apply(r_out_mat, 1, median)
  r_in_med  <- apply(r_in_mat,  1, median)
  r_pr_med  <- apply(r_pr_mat,  1, median)
  
  # Spearman: observed ranks vs sim-median ranks
  rho_out <- cor(r_out_obs, r_out_med, method = "spearman")
  rho_in  <- cor(r_in_obs,  r_in_med,  method = "spearman")
  rho_pr  <- cor(r_pr_obs,  r_pr_med,  method = "spearman")
  
  # Top-k stability (mean across observed top-k banks)
  tk <- function(r_obs, r_mat, k) .topk_stability(r_obs, r_mat, k)["mean"]
  data.frame(
    metric = c("Out-strength (rank)", "In-strength (rank)", "PageRank (rank)"),
    spearman_rho   = round(c(rho_out, rho_in, rho_pr), 3),
    top5_stability = round(c(tk(r_out_obs, r_out_mat, ks[1]),
                             tk(r_in_obs,  r_in_mat,  ks[1]),
                             tk(r_pr_obs,  r_pr_mat,  ks[1])), 3),
    top10_stability= round(c(tk(r_out_obs, r_out_mat, ks[2]),
                             tk(r_in_obs,  r_in_mat,  ks[2]),
                             tk(r_pr_obs,  r_pr_mat,  ks[2])), 3),
    check.names = FALSE
  )
}


tab_centrality <- centrality_summary(obs, sims, w_attr = "w_scale", ks = c(5,10))
