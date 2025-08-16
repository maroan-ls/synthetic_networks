source(here::here("R","ensure.R"))
ensure_ergm_sims()
ensure_baselines()

# Requires: igraph, intergraph
library(igraph)
library(intergraph)

# Convert to igraph and make a directed, binary view (drop w<=0 if present)
.as_igraph_binary_directed <- function(net, w_attr = "w_scale") {
  g <- if (inherits(net, "igraph")) net else intergraph::asIgraph(net)
  # Drop zero-or-negative weights if attribute exists
  if (!is.null(w_attr) && w_attr %in% edge_attr_names(g)) {
    w <- as.numeric(edge_attr(g, w_attr))
    g <- delete_edges(g, E(g)[is.na(w) | w <= 0])
  }
  # Ensure directed
  if (!is_directed(g)) g <- as.directed(g, mode = "mutual")
  g
}

# Safe diameter on directed graphs: maximum of finite shortest paths
.dir_diameter <- function(g) {
  D <- diameter(g, directed = TRUE, weights = NA)
  D <- D[is.finite(D)]
  if (!length(D)) return(NA_real_)
  max(D)
}

# Mean shortest path on directed graphs, finite pairs only (exclude i=j = 0)
.dir_mean_path <- function(g) {
  D <- mean_distance(g, directed = TRUE, weights = NA)
  D <- D[is.finite(D) & D > 0]
  if (!length(D)) return(NA_real_)
  mean(D)
}

compute_graph_metrics_2 <- function(net, w_attr = "w_scale") {
  g  <- .as_igraph_binary_directed(net, w_attr)
  gu <- as.undirected(g, mode = "collapse")  # for clustering & assortativity
  
  n <- vcount(g); m <- ecount(g)
  
  # total degree (in+out)
  kvals <- degree(g, mode = "all")
  kmax  <- if (n > 0) max(kvals) else NA_real_
  kbar  <- if (n > 0) mean(kvals) else NA_real_
  
  c(
    nodes             = n,
    edges             = m,
    density           = edge_density(g, loops = FALSE),
    diameter          = .dir_diameter(g),
    mean_path         = .dir_mean_path(g),
    transitivity      = transitivity(g, type = "average", weights = NA),
    kmax              = kmax,
    kmax_over_kbar    = if (is.na(kmax) || is.na(kbar) || kbar == 0) NA_real_ else kmax / kbar,
    assortativity     = assortativity_degree(g, directed = TRUE),
    avg_betweenness   = mean(igraph::betweenness(g, directed = TRUE, normalized = FALSE))
  )
}

summarize_ergm_sims <- function(obs, sims, w_attr = "w_scale") {
  obs_vec  <- compute_graph_metrics_2(obs,  w_attr = w_attr)
  # if 'sims' is a single network, wrap it
  if (inherits(sims, "igraph") || inherits(sims, "network")) sims <- list(sims)
  sim_mat <- t(vapply(sims, compute_graph_metrics_2, numeric(length(obs_vec)), w_attr = w_attr))
  colnames(sim_mat) <- names(obs_vec)
  
  # mean/sd for numeric metrics (keep nodes/edges as integers)
  sim_mean <- colMeans(sim_mat, na.rm = TRUE)
  sim_sd   <- apply(sim_mat, 2, sd, na.rm = TRUE)
  
  # Z for comparable scalars (skip discrete counts like nodes/edges)
  z_ok <- !(names(obs_vec) %in% c("nodes","edges"))
  z <- (obs_vec - sim_mean) / sim_sd
  z[!z_ok | !is.finite(z)] <- NA_real_
  
  list(obs = obs_vec, sim_mean = sim_mean, sim_sd = sim_sd, z = z)
}

build_scorecard <- function(obs, sims, er_g, ws_g, ba_g, w_attr = "w_scale") {
  S <- summarize_ergm_sims(obs, sims, w_attr = w_attr)
  
  er  <- compute_graph_metrics_2(er_g, w_attr = NULL)  # unweighted baselines
  ws  <- compute_graph_metrics_2(ws_g, w_attr = NULL)
  ba  <- compute_graph_metrics_2(ba_g, w_attr = NULL)
  
  metrics <- c("nodes","edges","density","diameter","mean_path",
               "transitivity","kmax","kmax_over_kbar","assortativity","avg_betweenness")
  
  df <- data.frame(
    metric   = metrics,
    observed = as.numeric(S$obs[metrics]),
    ERGM_mean = as.numeric(S$sim_mean[metrics]),
    ERGM_sd   = as.numeric(S$sim_sd[metrics]),
    ERGM_Z    = as.numeric(S$z[metrics]),
    ER_value  = as.numeric(er[metrics]),
    WS_value  = as.numeric(ws[metrics]),
    BA_value  = as.numeric(ba[metrics]),
    ER_delta  = as.numeric(er[metrics] - S$obs[metrics]),
    WS_delta  = as.numeric(ws[metrics] - S$obs[metrics]),
    BA_delta  = as.numeric(ba[metrics] - S$obs[metrics]),
    check.names = FALSE
  )
  
  # Pretty rounding: keep integers for counts; 3 decimals for rates; 2 for path lengths; 2 for Z
  int_rows <- df$metric %in% c("nodes","edges","kmax")
  z_rows   <- !is.na(df$ERGM_Z)
  
  df$observed[ int_rows] <- round(df$observed[ int_rows])
  df$ERGM_mean[int_rows] <- round(df$ERGM_mean[int_rows])
  df$ERGM_sd[  int_rows] <- round(df$ERGM_sd[  int_rows])
  
  round3 <- function(x) { ifelse(is.na(x), NA, round(x, 3)) }
  round2 <- function(x) { ifelse(is.na(x), NA, round(x, 2)) }
  
  df$observed[!int_rows]  <- round3(df$observed[!int_rows])
  df$ERGM_mean[!int_rows] <- round3(df$ERGM_mean[!int_rows])
  df$ERGM_sd[!int_rows]   <- round3(df$ERGM_sd[!int_rows])
  df$ERGM_Z[ z_rows]      <- round2(df$ERGM_Z[ z_rows])
  
  df$ER_value[!int_rows]  <- round3(df$ER_value[!int_rows])
  df$WS_value[!int_rows]  <- round3(df$WS_value[!int_rows])
  df$BA_value[!int_rows]  <- round3(df$BA_value[!int_rows])
  
  df$ER_delta <- round3(df$ER_delta)
  df$WS_delta <- round3(df$WS_delta)
  df$BA_delta <- round3(df$BA_delta)
  
  df
}


scorecard <- build_scorecard(obs, sims, g_er_sub, swg_sub, g_pa_sub, w_attr = "w_scale")




