sim5 <- simulate(fit0, nsim = 5, constraints = ~observed,
                 control = control.simulate.ergm(MCMC.burnin   = 5e4,
                                                 MCMC.interval = 1e4))

sapply(sim5, function(g) max(g %e% "w_l10"))   # largest log10-weight


## One matrix row per chain
acc <- do.call(rbind, lapply(attr(fit0$sample, "mcmc"), attr,
                             "acceptance.rate"))
mean(acc)    


library(coda)
## Convert first chain only, thin 100 draws
tr  <- as.mcmc(fit0$sample[[1]][ seq(1, nrow(fit0$sample[[1]]), 100), ])
plot(tr)     



acc <- vapply(fit0$sample, attr, numeric(1), which = "acceptance.rate")
mean(acc)  

str(fit0$sample[[1]])


ergm.count:::mean_var_plot(fit0)


install.packages("network")
library(network)
library(sna)
maxout <- sna::degree(net_l_s, cmode = "outdegree")   # length 3 674
maxin  <- sna::degree(net_l_s, cmode = "indegree")
max(maxout)
max(maxin)
