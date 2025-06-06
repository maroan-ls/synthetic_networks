#Exploratory fit run
library(ergm)
library(ergm.count)

net_l_s %e% "w_l10h" <- round((log10(net_l_s %e% "weight" + 1))/2)

library(ergm)
library(ergm.count)


ctrl2 <- control.ergm(
  init            = c("(offset) nonzero" = -50,   # << super-sparse
                      sum = -5, CMP = 0.5),
  MCMLE.maxit     = 2,           # scout only
  MCMC.burnin     = 1e5,
  MCMC.interval   = 5e3,
  MCMC.samplesize = 5e3,
  MCMC.prop.weights = "random",  # default valued proposal never fails
  MCMC.prop.args  = list(p0 = 0.999),
  MCMLE.density.guard = 1e5,  # << effectively off
  parallel        = 12,
  seed            = 123)

fit2 <- ergm(net_l_s ~ offset(nonzero) + sum + CMP,
             offset.coef = -50,              # must match the control.init
             response  = "w_l10h",
             reference = ~Poisson,
             control   = ctrl2,
             verbose   = TRUE)
