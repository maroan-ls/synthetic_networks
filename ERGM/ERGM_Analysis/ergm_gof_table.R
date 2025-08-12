library(igraph)
library(intergraph)

compute_gof_metrics <- function(net,
                                w_attr = "w_scale",
                                tail1 = 400,  # ???400m
                                tail2 = 1000  # ???1bn
) {
  # Convert to igraph if needed
  g <- if (inherits(net, "igraph")) net else intergraph::asIgraph(net)
  if (!is_directed(g)) stop("Expected a directed network.")
  
  # Pull weights
  w <- edge_attr(g, w_attr)
  if (is.null(w)) stop(sprintf("Edge attribute '%s' not found.", w_attr))
  w <- as.numeric(w)
  
  # Binary view for density/reciprocity (edges exist iff w > 0 in ERGM-count sims)
  # In case zeros got encoded as edges with w=0, drop them from the binary view:
  g_bin <- delete_edges(g, E(g)[w == 0])
  
  # Strengths (weighted degrees)
  out_str <- strength(g, mode = "out", weights = w)
  
  # Metrics for Table GOF-1
  c(
    density_nonzero     = edge_density(g_bin, loops = FALSE),           # share of dyads with w>0
    total_weight        = sum(w, na.rm = TRUE),                          # sum of weights (millions ???)
    tail_ge_400         = sum(w >= tail1, na.rm = TRUE),                 # count of edges ??? ???400m
    tail_ge_1000        = sum(w >= tail2, na.rm = TRUE),                 # count of edges ??? ???1bn
    reciprocity_rate    = reciprocity(g_bin, mode = "default"),          # fraction of reciprocal ties
    mean_out_strength   = mean(out_str, na.rm = TRUE),                   # mean lender strength (weighted out-degree)
    sd_out_strength     = sd(out_str, na.rm = TRUE), # dispersion of out-strength
    gini = Gini(strength(g, mode   = "all", weights = w)),
    gini_out = Gini(strength(g, mode   = "out", weights = w)),
    gini_in = Gini(strength(g, mode   = "in", weights = w))
  )
}

# --- Run on observed and simulations
obs_stats <- compute_gof_metrics(obs)

# 'sims' is typically a plain list / 'network.list'
if (inherits(sims, "network")) sims <- list(sims)

sim_stats <- t(vapply(sims, compute_gof_metrics,
                      FUN.VALUE = obs_stats))   # ensures same metrics/length
colnames(sim_stats) <- names(obs_stats)

# --- Summarise + Z-scores
sim_mean <- colMeans(sim_stats)
sim_sd   <- apply(sim_stats, 2, sd)

z_scores <- (obs_stats - sim_mean) / sim_sd

gof1 <- data.frame(
  metric   = names(obs_stats),
  observed = as.numeric(obs_stats),
  sim_mean = as.numeric(sim_mean),
  sim_sd   = as.numeric(sim_sd),
  z        = as.numeric(z_scores),
  row.names = NULL,
  check.names = FALSE
)

# Optional: nice rounding per metric type
round_to <- c(density_nonzero = 3, total_weight = 0, tail_ge_400 = 0, tail_ge_1000 = 0,
              reciprocity_rate = 3, mean_out_strength = 1, sd_out_strength = 1)
for (m in names(round_to)) gof1[gof1$metric == m, c("observed","sim_mean","sim_sd","z")] <-
  round(gof1[gof1$metric == m, c("observed","sim_mean","sim_sd","z")], round_to[m])


knitr::kable(gof1, format = "latex", booktabs = TRUE,
             caption = "Goodness-of-fit (valued ERGM): scalar statistics with simulation-based Z-scores.",
             align = c("l", "r","r","r","r"))
