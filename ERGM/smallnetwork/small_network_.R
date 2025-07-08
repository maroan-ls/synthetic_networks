
#Exploratory fit run
library(ergm)
library(ergm.count)

# Fitting Wire network


fitnp_wire1 <- ergm(net_wire ~ edges + mutual(form = "nabsdiff"),
                     response = "weight", 
                     reference = ~Geometric,
                     control = control.ergm(MCMC.samplesize = 2000,
                                            seed = 123))

fit_wire2 <- ergm(net_wire ~ edges + mutual(form = "nabsdiff"),
                   response = "weight", 
                   reference = ~Geometric,
                   control = control.ergm(MCMC.samplesize = 10000,
                                          seed = 123))

mcmc.diagnostics(fit_wire2)
