### Compute network metrics -------------------------------------------------------
# Generates **two** data frames ready for publication:
#   1) `binary_df`   - all unweighted metrics *and* their normalised variants.
#   2) `weighted_df` - weighted metrics (Real & GSIBW2) plus normalised variants.
#
# Normalisation rules implemented:
#   . Degree-based               ??? divide by (n ??? 1)
#   . Edge / vertex connectivity ??? divide by n
#   . Diameter & average path-length ??? divide by (n ??? 1)
#   . Strength-based (weighted)  ??? divide by (n ??? 1)
#   . Metrics already in [0,1]   ??? left untouched.
# ------------------------------------------------------------------------------

source(here::here("R","ensure.R"))
ensure_main_graphs()
ensure_baselines() 


compute_graph_metrics <- function(g) {
  if (!inherits(g, "igraph")) stop("`g` must be an igraph object")
  requireNamespace("igraph", quietly = TRUE)
  
  # ------------------------------------------------------------------
  # 1.  Binary metrics (weights ignored)
  # ------------------------------------------------------------------
  n          <- vcount(g)
  m          <- ecount(g)
  n_minus_1  <- ifelse(n > 1, n - 1, NA)
  
  # Degrees -----------------------------------------------------------
  deg_all    <- degree(g, mode = "all")        # total (in + out)
  deg_in     <- degree(g, mode = "in")         # in-degree for directed graphs
  
  max_deg    <- max(deg_all)
  min_deg    <- min(deg_all)
  avg_deg    <- mean(deg_all)                   # 2m / n
  avg_in_deg <- mean(deg_in)                    #  m / n (??? avg_deg for directed)
  
  # Connectivity & structure -----------------------------------------
  edge_conn  <- edge_connectivity(g)
  vert_conn  <- vertex_connectivity(g)
  
  dens_bin   <- edge_density(g, loops = FALSE)
  diam_bin   <- diameter(g, directed = TRUE, weights = NA)
  apl_bin    <- mean_distance(g, directed = TRUE, weights = NA)
  trans_bin  <- transitivity(g, type = "average", weights = NA)
  assort_bin <- assortativity_degree(g, directed = TRUE)
  
  # Aggregate centralities -------------------------------------------
  betw_bin_mean <- mean(betweenness(g, directed = TRUE, weights = NA))
  clos_bin_mean <- mean(closeness(g, mode = "all", weights = NA))
  eig_bin_mean  <- mean(eigen_centrality(g, directed = TRUE, weights = NA)$vector)
  reciprocity_bin <- reciprocity(g)
  radius_bin      <- radius(g, mode = "all")
  
  # Normalised binary metrics ----------------------------------------
  max_deg_norm      <- max_deg    / n_minus_1
  min_deg_norm      <- min_deg    / n_minus_1
  avg_deg_norm      <- avg_deg    / n_minus_1
  avg_in_deg_norm   <- avg_in_deg / n_minus_1
  
  edge_conn_norm <- edge_conn / n
  vert_conn_norm <- vert_conn / n
  
  diam_norm <- diam_bin / n_minus_1
  apl_norm  <- apl_bin  / n_minus_1
  
  # ------------------------------------------------------------------
  # 2.  Weighted metrics (populated only if weights exist)
  # ------------------------------------------------------------------
  if (!is.null(E(g)$weight)) {
    wt <- E(g)$weight
    
    diam_wt <- diameter(g, directed = TRUE, weights = 1/wt)
    apl_wt  <- mean_distance(g, directed = TRUE, weights = 1/wt)
    
    str_all <- strength(g, mode = "all", weights = wt)
    max_str <- max(str_all)
    min_str <- min(str_all)
    avg_str <- mean(str_all)
    
    trans_wt  <- transitivity(g, type = "average", weights = wt)
    assort_wt <- assortativity(g,
                               values    = strength(g, mode = "out", weights = wt),
                               values.in = strength(g, mode = "in",  weights = wt))
    
    betw_wt_mean <- mean(betweenness(g, directed = TRUE, weights = 1/wt))
    clos_wt_mean <- mean(closeness(g, mode = "all", weights = wt))
    eig_wt_mean  <- mean(eigen_centrality(g, directed = TRUE, weights = wt)$vector)
    pr_wt_mean   <- mean(page_rank(g, directed = TRUE, weights = wt)$vector)
    
    # Normalised weighted metrics ------------------------------------
    diam_wt_norm <- diam_wt / n_minus_1
    apl_wt_norm  <- apl_wt  / n_minus_1
    max_str_norm <- max_str / n_minus_1
    min_str_norm <- min_str / n_minus_1
    avg_str_norm <- avg_str / n_minus_1
  } else {
    diam_wt <- apl_wt <- max_str <- min_str <- avg_str <- trans_wt <- assort_wt <-
      betw_wt_mean <- clos_wt_mean <- eig_wt_mean <- pr_wt_mean <-
      diam_wt_norm <- apl_wt_norm <- max_str_norm <- min_str_norm <- avg_str_norm <- NA
  }
  
  # ------------------------------------------------------------------
  # 3.  Assemble and return (named vector)
  # ------------------------------------------------------------------
  c(
    # Counts & components
    nodes                       = n,
    edges                       = m,
    components                  = igraph::count_components(g),
    
    # ------------------- Binary metrics -------------------
    max_degree                  = max_deg,
    min_degree                  = min_deg,
    avg_degree_all              = avg_deg,
    avg_degree_in               = avg_in_deg,
    edge_connectivity           = edge_conn,
    vertex_connectivity         = vert_conn,
    diameter                    = diam_bin,
    average_path_length         = apl_bin,
    transitivity                = trans_bin,
    density                     = dens_bin,
    degree_assortativity        = assort_bin,
    avg_betweenness             = betw_bin_mean,
    avg_closeness               = clos_bin_mean,
    avg_eigenvector             = eig_bin_mean,
    reciprocity                 = reciprocity_bin,
    radius                      = radius_bin,
    
    # -------- Normalised binary variants --------
    max_degree_norm             = max_deg_norm,
    min_degree_norm             = min_deg_norm,
    avg_degree_all_norm         = avg_deg_norm,
    avg_degree_in_norm          = avg_in_deg_norm,
    edge_connectivity_norm      = edge_conn_norm,
    vertex_connectivity_norm    = vert_conn_norm,
    diameter_norm               = diam_norm,
    average_path_length_norm    = apl_norm,
    
    # ------------------- Weighted metrics ------------------
    diameter_weighted                   = diam_wt,
    average_path_length_weighted        = apl_wt,
    max_strength_weighted               = max_str,
    min_strength_weighted               = min_str,
    avg_strength_weighted               = avg_str,
    transitivity_weighted               = trans_wt,
    degree_assortativity_weighted       = assort_wt,
    avg_betweenness_weighted            = betw_wt_mean,
    avg_closeness_weighted              = clos_wt_mean,
    avg_eigenvector_weighted            = eig_wt_mean,
    avg_pagerank_weighted               = pr_wt_mean,
    
    # -------- Normalised weighted variants --------
    diameter_weighted_norm              = diam_wt_norm,
    average_path_length_weighted_norm   = apl_wt_norm,
    max_strength_weighted_norm          = max_str_norm,
    min_strength_weighted_norm          = min_str_norm,
    avg_strength_weighted_norm          = avg_str_norm
  )
}

# ----------------------------------------------------------------------------
# APPLY FUNCTION TO ALL GRAPHS -------------------------------------------------
# ----------------------------------------------------------------------------

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

metrics_list   <- lapply(graphs, compute_graph_metrics)
metrics_matrix <- do.call(cbind, metrics_list)
metrics_df     <- as.data.frame(metrics_matrix)

# Correct assortativity for undirected BA graphs
metrics_df["degree_assortativity", "BA"]     <- assortativity_degree(as_undirected(g_pa))
metrics_df["degree_assortativity", "BA_sub"] <- assortativity_degree(as_undirected(g_pa_sub))

# ----------------------------------------------------------------------------
# BUILD FINAL TABLES -----------------------------------------------------------
# ----------------------------------------------------------------------------

weighted_tag <- "_weighted"

binary_rows <- grep(weighted_tag, rownames(metrics_df), invert = TRUE, value = TRUE)
binary_df   <- metrics_df[binary_rows, ]

weighted_rows <- grep(weighted_tag, rownames(metrics_df), value = TRUE)
weighted_df   <- metrics_df[weighted_rows, c("Glcc", "GSIBW2")]

# Format numeric output (4 d.p.) ------------------------------------------------
format_fun <- function(x) formatC(as.numeric(x), format = "f", digits = 4, drop0trailing = TRUE)
binary_df[]   <- lapply(binary_df,   format_fun)
weighted_df[] <- lapply(weighted_df, format_fun)

# `binary_df` and `weighted_df` now include average_degree_in and
# its normalised variant alongside previous metrics.


