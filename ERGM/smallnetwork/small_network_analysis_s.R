library(statnet)      # loads network, sna, ergm
library(ergm.count)   # valued terms
set.seed(123)

# your fitted model object
fit <- fit_wire1         # change to your object name
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

# matrix: nsim × n_stats
sim_stats <- t(vapply(sims, gof_stats, numeric(length(gof_stats(obs)))))
obs_stats <- gof_stats(obs)
colnames(sim_stats) <- names(obs_stats)




