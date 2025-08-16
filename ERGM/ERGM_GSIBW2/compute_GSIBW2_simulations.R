library(statnet)      # loads network, sna, ergm
library(ergm.count)   # valued terms
set.seed(123)

source(here::here("R","ensure.R"))
ensure_ergm_fit()            # loads fit_wire if not already present


# your fitted model object
fit <- fit_wire2         # change to your object name
obs <- fit$network        # the observed network
w_obs <- as.numeric(obs %e% "w_scale")

## --------- define a function that returns *ONE* row of stats ----------
gof_stats <- function(net) {
  w <- as.numeric(net %e% "w_scale")
  outdeg <- sna::degree(net, cmode = "outdegree")

  c(
    # ---- edge-weight distribution -------------
    mean_w   = mean(w),
    var_w    = var(w),
    p95_w    = quantile(w, .95, names = FALSE),
    zero_pct = mean(w == 0),

    # ---- degree heterogeneity -----------------
    mean_outdeg = mean(outdeg),
    var_outdeg  = var(outdeg),      # simpler than GWD

    # ---- reciprocity --------------------------
    mutual_cnt  = sna::mutuality(net)  # total number of mutual dyads
  )
}

nsim <- 500            # 500 - 1 000 is typical
sims <- simulate(fit, nsim = nsim, output = "network",
                 control = control.simulate.ergm(MCMC.burnin = 1e5))

# matrix: nsim ? n_stats
sim_stats <- t(vapply(sims, gof_stats, numeric(length(gof_stats(obs)))))
obs_stats <- gof_stats(obs)
colnames(sim_stats) <- names(obs_stats)



par(mfrow = c(2, 4), mar = c(4, 4, 2, 1))
for(i in seq_along(obs_stats)) {
  boxplot(sim_stats[, i], horizontal = TRUE, outline = FALSE,
          main = names(obs_stats)[i], xlab = "")
  abline(v = obs_stats[i], col = 2, lwd = 2)   # red = observed
}


# ------- (B) QQ-envelope for the full weight distribution -
qs <- seq(0, 1, by = .01)

# observed quantiles
q_obs <- quantile(w_obs, probs = qs, names = FALSE)

# matrix: prob ? sim  (handles different edge counts)
q_sim <- sapply(sims, function(net)
  quantile(as.numeric(net %e% "w_scale"), probs = qs, names = FALSE))

# 95 % envelope
lo <- apply(q_sim, 1, quantile, .025)
hi <- apply(q_sim, 1, quantile, .975)
med <- apply(q_sim, 1, median)

plot(q_obs, q_obs, type = "n",
     xlab = "Theoretical quantile (model)",
     ylab = "Observed quantile",
     main = "Edge-weight QQ envelope")
polygon(c(q_obs, rev(q_obs)), c(lo, rev(hi)),
        col = adjustcolor("grey80", .6), border = NA)
lines(q_obs, med, lwd = 2)
points(q_obs, q_obs, pch = 19, cex = .4)
abline(0, 1, lty = 2)


logLik(fit_wire2, add = TRUE)

mcmc.diagnostics(fit_wire2, which = "plots")

saveRDS(c("sims","obs"), here::here("data/derived/models","ergm_sims.rds"))
