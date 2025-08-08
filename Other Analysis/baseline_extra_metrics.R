library(igraph)
library(ineq)          # Gini

graphs <- list(
  Glcc    = g_l_s,
  ER      = g_er,
  SW      = swg,
  BA      = g_pa,
  GSIBW2  = g_wire,
  ER_sub  = g_er_sub,
  SW_sub  = swg_sub,
  BA_sub  = g_pa_sub
)

er_ref <- c(
  Glcc   = "ER",
  ER     = "ER",
  SW     = "ER",
  BA     = "ER",
  GSIBW2 = "ER_sub",
  ER_sub = "ER_sub",
  SW_sub = "ER_sub",
  BA_sub = "ER_sub"
)

# ------------------------------------------------------------
#  Helper functions
# ------------------------------------------------------------
# ------------------------------------------------------------------
#  One function = one graph   → returns a *named* numeric vector
# ------------------------------------------------------------------
compute_graph_metrics_xtra <- function(g,
                                       directed   = TRUE,
                                       er_reps    = 1L,
                                       seed       = 1) {
  
  stopifnot(is_igraph(g))
  
  # ------------------------- 1. Degree & tail fit
  k   <- degree(g, mode = if (directed) "all" else "total")
  fit <- igraph::fit_power_law(k[k >= 1])
  
  # ------------------------- 2. Path length & clustering
  L <- mean_distance(g, directed = directed, unconnected = TRUE,
                     weights = NA)
  C <- transitivity(g, type = "global")
  
  # ------------------------- 3. Small-world index σ
  set.seed(seed)
  L_er <- C_er <- numeric(er_reps)
  for (i in seq_len(er_reps)) {
    g_er <- sample_gnm(vcount(g), ecount(g),
                       directed = directed, loops = FALSE)
    L_er[i] <- mean_distance(g_er, directed = directed, unconnected = TRUE, 
                             weights = NA)
    C_er[i] <- transitivity(g_er, type = "global")
  }
  sigma <- (C / mean(C_er)) / (L / mean(L_er))
  
  # ------------------------- 4. Global & local efficiency
  global_eff <- function(h) {
    D <- distances(h, mode = if (directed) "all" else "all",
                   weights = NA)
    diag(D) <- Inf
    mean(1 / D[is.finite(D)])
  }
  
  # global
  E_glob <- global_eff(g)
  
  # local: ego graphs of radius 1
  ego_subs <- make_ego_graph(g, order = 1,
                             mode = if (directed) "all" else "all")
  E_loc <- mean(vapply(ego_subs, global_eff, numeric(1)))
  
  # ------------------------- 5. Inequality
  gini_deg <- ineq::Gini(k)
  
  # ------------------------- 6. Return tidy vector
  c(
    alpha_hat   = fit$alpha,
    ks_distance = fit$KS.stat,
    gini        = gini_deg,
    L           = L,
    C           = C,
    sigma       = sigma,
    E_glob      = E_glob,
    E_loc       = E_loc
  )
}



xtra_metrics_list <- lapply(graphs, compute_graph_metrics_xtra)

# turn it into a data frame
xtra_metrics_df <- do.call(rbind, lapply(names(xtra_metrics_list), function(n) {
  data.frame(graph = n, t(xtra_metrics_list[[n]]))
}))
print(xtra_metrics_df)


