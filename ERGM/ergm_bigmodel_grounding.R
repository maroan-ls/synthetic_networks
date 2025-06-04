# build the model frame but don't estimate yet
m0 <- ergm(net_l_s ~ edges + mutual(form="nabsdiff"),
           response   = "weight",
           reference  = ~Geometric,
           estimate   = "CD")     # just to create the model object

# simulate a single long chain from *fixed* coefficients
ctrl.sim <- control.simulate.ergm(MCMC.burnin   = 1e5,
                                  MCMC.interval = 2e4,
                                  MCMC.prop.weights = "random",
                                  MCMC.prop.args   = list(p0 = .9))
sim      <- simulate(m0$formula, coef = rep(0,2), nsim = 1,
                     response = "weight", reference = ~Geometric,
                     control  = ctrl.sim)

summary(sim ~ edges + mutual(form="nabsdiff"))



fit0 <- ergm(net_l_s ~ sum + nonzero,
             response   = "weight",
             reference  = ~Poisson,
             verbose = TRUE, # Poisson usually mixes better
             control = control.ergm(
               init.method     = "zeros",
               MCMC.prop.weights = "TNT",
               MCMC.prop.args   = list(p0 = .95),  # favour jumps to 0
               MCMC.burnin      = 2e5,
               MCMC.interval    = 2e4,
               MCMC.samplesize  = 2e4,
               MCMLE.maxit      = 40,
               parallel         = 12,
               MCMLE.density.guard.min = 5000))



net_l_s %e% "w_log10" <- round(log10(net_l_s %e% "weight" + 1))
summary(net_l_s %e% "w_log10")



fit01 <- ergm(net_l_s ~ sum + nonzero,
             response   = "w_log10",
             reference  = ~Poisson,
             verbose = TRUE, # Poisson usually mixes better
             control = control.ergm(
               init.method     = "zeros",
               MCMC.prop.weights = "TNT",
               MCMC.prop.args   = list(p0 = .95),  # favour jumps to 0
               MCMC.burnin      = 2e5,
               MCMC.interval    = 2e4,
               MCMC.samplesize  = 2e4,
               MCMLE.maxit      = 40,
               parallel         = 12,
               MCMLE.density.guard.min = 5000))

