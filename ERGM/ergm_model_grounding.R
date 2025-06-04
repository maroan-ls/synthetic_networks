fit_weight20 <- ergm(net_l_s ~ edges + mutual(form = "nabsdiff"),
                     response = "weight", 
                     reference = ~Geometric,
                     verbose = 5,
                     control = control.ergm(MCMC.burnin = 10000, 
                                            MCMC.samplesize = 50000,
                                            MCMC.interval = 1000, 
                                            MCMLE.maxit = 60,, 
                                            MCMLE.steplength = 0.25, 
                                            MCMLE.density.guard = 50,
                                            parallel = 12,
                                            seed = 123))


fit_weight20 <- ergm(net_l_s ~ sum + nonzero,
                     response = "weight", 
                     reference = ~Geometric,
                     verbose = 5,
                     control = control.ergm(MCMC.burnin = 10000, 
                                            MCMC.samplesize = 50000,
                                            MCMC.interval = 1000, 
                                            MCMLE.maxit = 60,, 
                                            MCMLE.steplength = 0.25, 
                                            MCMLE.density.guard = 50,
                                            parallel = 12,
                                            seed = 123))


fit_weight19 <- ergm(net_sib ~ edges + mutual(form = "nabsdiff"),
                     response = "weight", 
                     reference = ~Geometric,
                     control = control.ergm(MCMC.samplesize = 2000,
                                            seed = 123))


mcmc.diagnostics(fit_weight19, center=TRUE)
