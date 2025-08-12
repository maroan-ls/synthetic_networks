# Packages
library(igraph)
library(intergraph)

# Convert to igraph and make:
#  - g_wire_reverse: directed, weighted
#  - g_wire_rb: directed, BINARY (drop edges with w==0)
as_igraph_pair <- function(net, w_attr = "w_scale") {
  g_wire_reverse <- if (inherits(net, "igraph")) net else intergraph::asIgraph(net)
  w   <- as.numeric(edge_attr(g_wire_reverse, w_attr))
  if (is.null(w)) stop(sprintf("Edge attribute '%s' not found.", w_attr))
  # Binary view: remove zero-weight edges
  g_wire_rb <- delete_edges(g_wire_reverse, E(g_wire_reverse)[is.na(w) | w == 0])
  list(g_wire_reverse = g_wire_reverse, g_wire_rb = g_wire_rb, w = w)
}

# Helper: pad a vector to length L with zeros at the tail
pad_right <- function(x, L) { c(x, rep(0, max(0, L - length(x)))) }

#-----
###PLOTS
#----
plot_gof_degree <- function(obs, sims, mode = c("in","out"), w_attr = "w_scale",
                            q = c(0.025, 0.5, 0.975), main_prefix = "Degree PMF") {
  mode <- match.arg(mode)
  # Observed
  oi <- as_igraph_pair(obs, w_attr)
  d_obs <- degree(oi$g_wire_rb, mode = mode)
  max_k <- max(d_obs)
  # Simulated PMFs
  pmf_list <- lapply(sims, function(net) {
    gi <- as_igraph_pair(net, w_attr)
    dk <- degree(gi$g_wire_rb, mode = mode)
    tab <- tabulate(dk + 1) # degrees start at 0
    pmf <- tab / sum(tab)
    pmf
  })
  max_k <- max(max_k, max(sapply(pmf_list, length)) - 1L)
  pmf_mat <- sapply(pmf_list, function(p) pad_right(p, max_k + 1L))
  # Envelopes
  lo <- apply(pmf_mat, 1, quantile, probs = q[1], na.rm = TRUE)
  md <- apply(pmf_mat, 1, quantile, probs = q[2], na.rm = TRUE)
  hi <- apply(pmf_mat, 1, quantile, probs = q[3], na.rm = TRUE)
  # Observed PMF
  tab_obs <- tabulate(d_obs + 1, nbins = max_k + 1L)
  pmf_obs <- tab_obs / sum(tab_obs)
  # Plot
  ks <- 0:max_k
  plot(ks, pmf_obs, type = "h", lwd = 2,
       xlab = paste(mode, "degree"), ylab = "Probability",
       main = paste(main_prefix, "(", mode, ")"))
  polygon(c(ks, rev(ks)), c(lo, rev(hi)), border = NA, col = adjustcolor("grey80", .7))
  lines(ks, md, lwd = 2)
  points(ks, pmf_obs, pch = 19)
  legend("topright", bty = "n",
         legend = c("Observed", "Median sim", "95% envelope"),
         lty = c(NA,1,NA), pch = c(19, NA, 15),
         pt.cex = c(1, NA, 2), col = c("black","black","grey80"))
}

# --- degree PMF (binary) with DISCRETE 95% envelopes ---
plot_gof_degree_discrete <- function(obs, sims,
                                     mode = c("in","out"),
                                     w_attr = "w_scale",
                                     q = c(0.025, 0.5, 0.975),
                                     halfwidth = 0.25,
                                     show_legend = TRUE,
                                     main_prefix = "a) Degree PMF") {
  mode <- match.arg(mode)
  
  oi <- as_igraph_pair(obs, w_attr)
  d_obs <- degree(oi$g_wire_rb, mode = mode)
  
  # PMF for one vector of degrees (includes zeros)
  pmf_of <- function(d, L = NULL) {
    L <- if (is.null(L)) max(d) else max(L, max(d))
    tab <- tabulate(d + 1, nbins = L + 1L)
    tab / sum(tab)
  }
  
  # Sim PMFs
  pmfs <- lapply(sims, function(net) {
    gi <- as_igraph_pair(net, w_attr)
    dk <- degree(gi$g_wire_rb, mode = mode)
    pmf_of(dk, L = 0)
  })
  max_k <- max(c(0L, sapply(pmfs, function(p) length(p)-1L), max(d_obs)))
  pmfs  <- lapply(pmfs, function(p) { if (length(p) < max_k + 1L) c(p, rep(0, max_k + 1L - length(p))) else p })
  pmf_mat <- do.call(cbind, pmfs)
  
  lo <- apply(pmf_mat, 1, quantile, probs = q[1], na.rm = TRUE)
  md <- apply(pmf_mat, 1, quantile, probs = q[2], na.rm = TRUE)
  hi <- apply(pmf_mat, 1, quantile, probs = q[3], na.rm = TRUE)
  
  pmf_obs <- pmf_of(d_obs, L = max_k)
  ks <- 0:max_k
  
  plot(ks, pmf_obs, type = "n",
       xlab = paste(mode, "degree"), ylab = "Probability",
       main = paste(main_prefix, "(", mode, ")"),
       xlim = c(-0.5, max_k + 0.5),
       ylim = c(0, max(hi, pmf_obs, na.rm = TRUE) * 1.1),
       xaxt = "n")
  axis(1, at = ks, labels = ks)
  
  # discrete envelope as rectangles centered on integer degrees
  rect(ks - halfwidth, lo, ks + halfwidth, hi, col = adjustcolor("grey80", .85), border = NA)
  # median tick
  segments(ks - halfwidth, md, ks + halfwidth, md, lwd = 2)
  # observed stems + points
  segments(ks, 0, ks, pmf_obs, lwd = 2)
  points(ks, pmf_obs, pch = 19)
  
  if (show_legend) {
    legend("topright", bty = "n",
           legend = c("Observed", "Median sim", "95% envelope"),
           pch = c(19, NA, 15), lty = c(NA, 1, NA), lwd = c(NA, 2, NA),
           col = c("black","black","grey80"))
  }
}

# --- strength CCDF (weighted degree) with optional legend ---
plot_gof_strength_ccdf <- function(obs, sims, mode = c("in","out"),
                                   w_attr = "w_scale",
                                   q = c(0.025, 0.5, 0.975),
                                   main_prefix = "Strength CCDF",
                                   show_legend = TRUE,
                                   logx = FALSE) {
  mode <- match.arg(mode)
  oi <- as_igraph_pair(obs, w_attr)
  s_obs <- strength(oi$g_wire_reverse, mode = mode, weights = edge_attr(oi$g_wire_reverse, w_attr))
  s_obs <- s_obs[s_obs > 0]
  pgrid <- seq(0, 0.99, by = 0.01)
  q_obs <- as.numeric(quantile(s_obs, probs = pgrid, names = FALSE))
  
  q_sims <- sapply(sims, function(net) {
    gi <- as_igraph_pair(net, w_attr)
    s <- strength(gi$g_wire_reverse, mode = mode, weights = edge_attr(gi$g_wire_reverse, w_attr))
    s <- s[s > 0]; if (!length(s)) return(rep(0, length(pgrid)))
    as.numeric(quantile(s, probs = pgrid, names = FALSE))
  })
  lo <- apply(q_sims, 1, quantile, probs = q[1], na.rm = TRUE)
  md <- apply(q_sims, 1, quantile, probs = q[2], na.rm = TRUE)
  hi <- apply(q_sims, 1, quantile, probs = q[3], na.rm = TRUE)
  
  y <- 1 - pgrid
  plot(q_obs, y, type = "n",
       xlab = paste(mode, "strength (USD, millions)"),
       ylab = "CCDF  (P(S ≥ x))",
       main = paste(main_prefix, "(", mode, ")"),
       log = if (logx) "x" else "")
  polygon(c(lo, rev(hi)), c(y, rev(y)), col = adjustcolor("grey80", .6), border = NA)
  lines(md, y, lwd = 2)
  lines(q_obs, y, lwd = 2, lty = 2)
  if (show_legend) {
    legend("topright", bty = "n",
           legend = c("Observed", "Median sim", "95% envelope"),
           lty = c(2,1,NA), pch = c(NA, NA, 15), lwd = c(2,2,NA),
           col = c("black","black","grey80"))
  }
}


# --- geodesic (discrete envelope) with optional legend ---
plot_gof_geodesic_discrete <- function(obs, sims, w_attr = "w_scale",
                                       q = c(0.025, 0.5, 0.975),
                                       main = "Geodesic distance (undirected)",
                                       halfwidth = 0.25,
                                       show_legend = TRUE) {
  oi <- as_igraph_pair(obs, w_attr)
  g_u <- as.undirected(oi$g_wire_rb, mode = "collapse")
  d_obs <- distances(g_u); d_obs <- d_obs[is.finite(d_obs) & d_obs > 0]
  
  pmf_of <- function(d, L = NULL) {
    if (!length(d)) return(numeric(0))
    L <- if (is.null(L)) max(d) else max(L, max(d))
    tab <- table(factor(d, levels = 1:L))
    as.numeric(tab) / sum(tab)
  }
  
  pmfs <- lapply(sims, function(net) {
    gi <- as_igraph_pair(net, w_attr)
    gu <- as.undirected(gi$g_wire_rb, mode = "collapse")
    d  <- distances(gu); d <- d[is.finite(d) & d > 0]
    pmf_of(d, L = 1)
  })
  max_d <- max(c(1, sapply(pmfs, length), if (length(d_obs)) max(d_obs) else 1))
  pmfs  <- lapply(pmfs, function(p) if (length(p) < max_d) c(p, rep(0, max_d - length(p))) else p)
  pmf_mat <- do.call(cbind, pmfs)
  
  lo <- apply(pmf_mat, 1, quantile, probs = q[1], na.rm = TRUE)
  md <- apply(pmf_mat, 1, quantile, probs = q[2], na.rm = TRUE)
  hi <- apply(pmf_mat, 1, quantile, probs = q[3], na.rm = TRUE)
  
  pmf_obs <- pmf_of(d_obs, L = max_d)
  xs <- 1:max_d
  
  plot(xs, pmf_obs, type = "n",
       xlab = "Distance", ylab = "Probability",
       xlim = c(0.5, max_d + 0.5),
       ylim = c(0, max(hi, pmf_obs, na.rm = TRUE) * 1.1),
       xaxt = "n", main = main)
  axis(1, at = xs, labels = xs)
  
  rect(xs - halfwidth, lo, xs + halfwidth, hi, col = adjustcolor("grey80", .85), border = NA)
  segments(xs - halfwidth, md, xs + halfwidth, md, lwd = 2)
  segments(xs, 0, xs, pmf_obs, lwd = 2)
  points(xs, pmf_obs, pch = 19)
  
  if (show_legend) {
    legend("topright", bty = "n",
           legend = c("Observed","Median sim","95% envelope"),
           pch = c(19, NA, 15), lty = c(NA,1,NA), lwd = c(NA,2,NA),
           col = c("black","black","grey80"))
  }
}

# --- edge-weight CCDF with optional legend (unchanged logic) ---
plot_gof_edgeweight_ccdf <- function(obs, sims, w_attr = "w_scale",
                                     q = c(0.025, 0.5, 0.975),
                                     thresholds = c(400, 1000),
                                     main = "Edge-weight CCDF",
                                     show_legend = TRUE) {
  oi <- as_igraph_pair(obs, w_attr)
  w_obs <- oi$w[oi$w > 0]
  pgrid <- seq(0, 0.99, by = 0.01)
  q_obs <- as.numeric(quantile(w_obs, probs = pgrid, names = FALSE))
  
  q_sims <- sapply(sims, function(net) {
    gi <- as_igraph_pair(net, w_attr)
    w <- as.numeric(edge_attr(gi$g_wire_reverse, w_attr))
    w <- w[!is.na(w) & w > 0]
    if (!length(w)) return(rep(0, length(pgrid)))
    as.numeric(quantile(w, probs = pgrid, names = FALSE))
  })
  lo <- apply(q_sims, 1, quantile, probs = q[1], na.rm = TRUE)
  md <- apply(q_sims, 1, quantile, probs = q[2], na.rm = TRUE)
  hi <- apply(q_sims, 1, quantile, probs = q[3], na.rm = TRUE)
  
  y <- 1 - pgrid
  plot(q_obs, y, type = "n",
       xlab = "Edge weight (USD, millions)",
       ylab = "CCDF  (P(W ≥ x))",
       main = main)
  polygon(c(lo, rev(hi)), c(y, rev(y)), col = adjustcolor("grey80", .6), border = NA)
  lines(md, y, lwd = 2)
  lines(q_obs, y, lwd = 2, lty = 2)
  abline(v = thresholds, lty = 3)
  if (show_legend) {
    legend("topright", bty = "n",
           legend = c("Observed", "Median sim", "95% envelope", "$400m/$1bn"),
           lty = c(2,1,NA,3), pch = c(NA, NA, 15, NA), lwd = c(2,2,NA,1),
           col = c("black","black","grey80","black"))
  }
}


# If 'sims' came back as a 'network' instead of list, wrap it:
if (inherits(sims, "network")) sims <- list(sims)

par(mfrow = c(2,2), mar = c(4,4,1.5,0.5))
plot_gof_degree_discrete(obs, sims, mode = "out", show_legend = FALSE)
plot_gof_degree_discrete(obs, sims, mode = "in",  show_legend = TRUE)
plot_gof_strength_ccdf  (obs, sims, mode = "out", show_legend = FALSE)
plot_gof_strength_ccdf  (obs, sims, mode = "in",  show_legend = TRUE)

par(mfrow = c(1,2), mar = c(4,4,1.5,0.5))
plot_gof_geodesic_discrete(obs, sims, show_legend = FALSE)
plot_gof_edgeweight_ccdf(obs, sims, show_legend = TRUE)