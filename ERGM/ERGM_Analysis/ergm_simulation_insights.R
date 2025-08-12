# --- requirements
library(igraph)
library(intergraph)

# ---------- shared utilities ----------
as_igraph_pair <- function(net, w_attr = "w_scale") {
  g_wire_reverse <- if (inherits(net, "igraph")) net else intergraph::asIgraph(net)
  w   <- as.numeric(edge_attr(g_wire_reverse, w_attr))
  if (is.null(w)) stop(sprintf("Edge attribute '%s' not found.", w_attr))
  g_wire_rb <- delete_edges(g_wire_reverse, E(g_wire_reverse)[is.na(w) | w == 0])  # binary view
  list(g_wire_reverse = g_wire_reverse, g_wire_rb = g_wire_rb, w = w)
}
strength_vec <- function(net, mode = c("in","out"), w_attr = "w_scale") {
  mode <- match.arg(mode)
  P <- as_igraph_pair(net, w_attr)
  strength(P$g_wire_reverse, mode = mode, weights = P$w)
}
gini <- function(x){
  x <- as.numeric(x); x <- x[!is.na(x) & x >= 0]
  n <- length(x); if (n == 0) return(NA_real_); if (all(x==0)) return(0)
  x <- sort(x); (2*sum(seq_len(n)*x)/(n*sum(x))) - (n+1)/n
}
.expand_range <- function(x, pad = 0.05) {  # ensure obs fits in xlim
  r <- range(x, na.rm = TRUE); w <- diff(r); if (w == 0) w <- abs(r[1])*0.1 + 1
  c(r[1] - pad*w, r[2] + pad*w)
}


# ---------- 1) Lorenz curves with envelopes (out or in) ----------
plot_lorenz_envelope <- function(obs, sims, mode = c("out","in"),
                                 w_attr = "w_scale",
                                 q = c(0.025, 0.5, 0.975),
                                 main_prefix = "Lorenz curve") {
  mode <- match.arg(mode)
  pgrid <- seq(0, 1, by = 0.01)
  
  L_obs <- lorenz_vals(strength_vec(obs, mode, w_attr), pgrid)
  L_sims <- sapply(sims, function(net) lorenz_vals(strength_vec(net, mode, w_attr), pgrid))
  
  lo <- apply(L_sims, 1, quantile, probs = q[1], na.rm = TRUE)
  md <- apply(L_sims, 1, quantile, probs = q[2], na.rm = TRUE)
  hi <- apply(L_sims, 1, quantile, probs = q[3], na.rm = TRUE)
  
  plot(pgrid, L_obs, type = "n", xlab = "Cumulative share of banks",
       ylab = "Cumulative share of total strength",
       main = sprintf("%s (%s-strength)", main_prefix, mode))
  abline(0, 1, lty = 3)  # line of equality
  polygon(c(pgrid, rev(pgrid)), c(lo, rev(hi)),
          col = adjustcolor("grey80", .6), border = NA)
  lines(pgrid, md, lwd = 2)            # median sim
  lines(pgrid, L_obs, lwd = 2, lty = 2)  # observed
  legend("topleft", bty = "n",
         legend = c("Observed", "Median sim", "95% envelope", "Equality"),
         lty = c(2,1,NA,3), pch = c(NA, NA, 15, NA),
         lwd = c(2,2,NA,1), col = c("black","black","grey80","black"))
}

# ---------- 2) Gini + Top-k share histograms ----------

.add_obs_label <- function(x, fmt = "%.2f") {
  usr <- par("usr")                 # panel-specific y-range
  text(x = x*0.93, y = usr[4]*0.95, 
       labels = sprintf("Obs = %s", sprintf(fmt, x)),
       col = 1, xpd = NA, adj = c(0, 1), cex = 1)
}
plot_gini_topk_grid <- function(obs, sims, ks = c(5,10), w_attr = "w_scale") {
  stopifnot(length(ks) == 2)  # grid expects two top-k columns
  modes <- c("out","in")
  old <- par(mfrow = c(2, 3), mar = c(4,4,2,1))
  on.exit(par(old), add = TRUE)
  
  for (mode in modes) {
    # prepare strengths
    s_obs  <- strength_vec(obs, mode, w_attr)
    s_sims <- lapply(sims, strength_vec, mode = mode, w_attr = w_attr)
    
    # ---- Gini
    g_obs  <- gini(s_obs)
    g_sims <- vapply(s_sims, gini, numeric(1))
    xlim <- .expand_range(c(g_sims, g_obs), pad = 0.06)
    hist(g_sims, breaks = "FD", col = "skyblue",
         main = paste("Gini (", mode, "-strength)", sep=""),
         xlab = "Gini", xlim = xlim)
    abline(v = g_obs, col = 2, lwd = 3)
    .add_obs_label(g_obs, fmt = "%.2f")
    
    #legend("topright", bty="n", legend="Observed", lwd=3, col=2)
    
    # ---- Top-k shares
    top_share <- function(s, k) sum(sort(s, TRUE)[seq_len(min(k, length(s)))]) / sum(s)
    for (k in ks) {
      ts_obs  <- top_share(s_obs, k)
      ts_sims <- vapply(s_sims, top_share, numeric(1), k = k)
      xlim <- .expand_range(c(ts_sims, ts_obs), pad = 0.06)
      hist(ts_sims, breaks = "FD", col = "skyblue",
           main = paste("Top-", k, " share (", mode, ")", sep=""),
           xlab = "Share of total", xlim = xlim)
      abline(v = ts_obs, col = 2, lwd = 3)
      .add_obs_label(ts_obs, fmt = "%.2f")
      #legend("topright", bty="n", legend="Observed", lwd=3, col=2)
    }
  }
}

plot_exceedance_counts <- function(obs, sims, thresholds = c(400, 1000),
                                   w_attr = "w_scale") {
  get_counts <- function(net) {
    P <- as_igraph_pair(net, w_attr)
    sapply(thresholds, function(t) sum(P$w >= t, na.rm = TRUE))
  }
  c_obs  <- get_counts(obs)
  c_sims <- t(vapply(sims, get_counts, numeric(length(thresholds))))
  
  old <- par(mfrow = c(1, length(thresholds)), mar = c(4,4,2,1))
  on.exit(par(old), add = TRUE)
  
  for (j in seq_along(thresholds)) {
    xlim <- .expand_range(c(c_sims[,j], c_obs[j]), pad = 0.06)
    hist(c_sims[, j], breaks = "FD", col = "skyblue",
         main = paste("\U2265 $", format(thresholds[j], big.mark=","), " m (count)", sep=""),
         xlab = "Count", xlim = xlim)
    abline(v = c_obs[j], col = 2, lwd = 3)
    .add_obs_label(c_obs[j], fmt = "%.0f")
    #legend("topright", bty="n", legend="Observed", lwd=3, col=2)
  }
}


# If 'sims' is a single network, wrap it as list:
if (inherits(sims, "network")) sims <- list(sims)

# Lorenz curves (out and in)
par(mfrow = c(1,2), mar = c(4,4,4,1))
plot_lorenz_envelope(obs, sims, mode = "out")
plot_lorenz_envelope(obs, sims, mode = "in")

# 2×3 grid: Gini + Top-5/Top-10 for out & in
plot_gini_topk_grid(obs, sims, ks = c(5,10))

# Exceedance counts (>= $400m, >= $1bn)
plot_exceedance_counts(obs, sims, thresholds = c(400, 1000))
