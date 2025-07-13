# Small network analysis
set.seed(123)


summary(fit_wire1)
mcmc.diagnostics(fit_wire1)  # check trace & autocorr

sims <- simulate(fit_wire0, nsim = 100, output = "stats",
                 statsonly = FALSE)
obs  <- net_wire %e% "w_scale"

edgewiseDist <- function(net) as.numeric(net_wire %e% "w_scale")

sim.distr <- lapply(sims, edgewiseDist)
boxplot(sim.distr, ylim = range(obs),
        main = "Edge-weight distribution in simulations vs observed")
points(obs, col = 2, pch = 20)  # observed counts



############################################################
# 0.  What you already have --------------------------------
############################################################
#fit_wire0   # <- your fitted valued ERGM object
w_obs   <- as.numeric(net_wire %e% "w_scale")

############################################################
# 1.  Simulate replicate networks --------------------------
############################################################
simlist <- simulate(fit_wire0, nsim = 1000, output = "network")

############################################################
# 2.  Function that extracts edge weights *and* any
#     network-level stats you care about -------------------
############################################################
extract_stats <- function(net) {
  w <- as.numeric(net_wire %e% "w_scale")
  c(total = sum(w),
    mean  = mean(w),
    var   = var(w),
    p95   = quantile(w, .95),
    # degree stats:
    mean_indeg  = mean(sna::degree(net, cmode = "indegree")),
    mean_outdeg = mean(sna::degree(net, cmode = "outdegree")))
}

stats_sim <- t(vapply(simlist, extract_stats, numeric(6)))
stats_obs <- extract_stats(net_wire)

############################################################
# 3.  Edge-weight distribution: QQ envelope ---------------
############################################################
qqplot <- function(obs, sims, main = "", ...) {
  qs   <- seq(0, 1, length.out = 201)   # finer grid, esp. in the tail
  qobs <- quantile(obs, qs)
  qsims <- apply(sims, 2, quantile, qs)   # nsim × 101 matrix
  qlo  <- apply(qsims, 1, quantile, .025)
  qhi  <- apply(qsims, 1, quantile, .975)
  plot(qobs, qobs, type = "n",
       xlab = "Theoretical quantile (model)",
       ylab = "Observed quantile", main = main)
  polygon(c(qobs, rev(qobs)), c(qlo, rev(qhi)),
          col = adjustcolor("grey80", .5), border = NA)
  lines(qobs, apply(qsims, 1, median), lwd = 2)
  points(qobs, qobs, pch = 19, cex = .6)
  abline(0, 1, lty = 2)
}

# assemble weight matrix: each column is one simulation

w_sims <- sapply(simlist, function(net) as.numeric(net_wire %e% "w_scale"),
                 simplify = "matrix")
qqplot(w_obs, w_sims,
       main = "Edge-weight QQ-envelope\n95% shaded, median line")

############################################################
# 4.  Network-level statistics envelope -------------------
############################################################
stat_names <- colnames(stats_sim)
par(mfrow = c(2, 3))
for(i in seq_along(stat_names)) {
  boxplot(stats_sim[, i], horizontal = TRUE, outline = FALSE,
          main = stat_names[i])
  abline(v = stats_obs[i], col = 2, lwd = 2)  # red line = observed
}


library(coda)

samp <- as.mcmc.list(fit_wire0$sample)   # last-round MCMC sample

# Effective sample size: aim for >200 per stat
effectiveSize(samp)

# Geweke diagnostic: |Z| < 1.96 ??? fail to reject convergence
geweke.diag(samp)

# Autocorrelation at lag 1: ideally < 0.1
acf_res <- autocorr.diag(samp, lags = 1)
acf_res

