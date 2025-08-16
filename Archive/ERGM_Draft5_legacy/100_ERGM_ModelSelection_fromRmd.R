# Source Rmd: ERGM/fullnetwork_tries/ERGM_ModelSelection.Rmd
# Generated: 2025-08-16 08:00
# Purpose: converted from Rmd for pipeline/audit use

suppressPackageStartupMessages({ library(here) })
set.seed(123)

#' ---
#' title: "ERGM2"
#' author: "Maroan El Sirfy"
#' date: "2025-01-27"
#' output: html_document
#' ---
#' This is just a mixed first tries document of fitting the full network into an ergm
#' 
#' 
#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
# install.packages("statnet")
# install.packages("ergm.count")
# install.packages("igraph")
# install.packages("network")
# install.packages("tergm")
# install.packages("ergm.multi")

## -----------------------------------------------------------------------------------------------------------------------------------------
library(ergm)
library(igraph)
library(network)
library(intergraph)
library(ergm.count)
library(statnet)
library(tergm)

#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
temp_n_weight <- network::get.edge.attribute(net, "weight")
typeof(temp_n_weight)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
fit_base <- ergm(net ~ edges)
summary(fit_base)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weights <- ergm(net ~ edges + edgecov(net, "weight"))
summary(fit_weights)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
gof_fit <- gof(fit_weights4, control = control.gof.formula(nsim = 10), verbose = 4)

plot(gof_fit)

#' Okay this worked, at least there were no major errors, after I fixed your mistake.
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weights2 <- ergm(net ~ edges + idegree(1:4) + odegree(1:4))
summary(fit_weights2)

## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weights2 <- ergm(net ~ edges + idegree(1:4) + odegree(1:4))

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
gof_fit2 <- gof(fit_weights2, control = control.gof.formula(nsim = 10), verbose = 4)
plot(gof_fit2)

## -----------------------------------------------------------------------------------------------------------------------------------------
mcmc.diagnostics(fit_weights2)

#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
log_weights <- log1p(network::get.edge.attribute(net, "weight"))
network::set.edge.attribute(net, "log_weight", log_weights)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weights3 <- ergm(net ~ edges + edgecov(net, "log_weight"))
summary(fit_weights3)

#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
# Convert to adjacency matrix with weights
weight_matrix <- as.matrix(network::as.sociomatrix(net, attr = "weight"))

in_weights <- colSums(weight_matrix)  # Total borrowed per node
out_weights <- rowSums(weight_matrix) # Total lent per node

# Assign as nodal attributes in the network object
network::set.vertex.attribute(net, "in_weight", in_weights)
network::set.vertex.attribute(net, "out_weight", out_weights)

rm(weight_matrix)

#' 
#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weights4s <- ergm(net ~ edges + nodeicov("in_weight") + nodeocov("out_weight"))

summary(fit_weights4)

#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weights5 <- ergm(net ~ edges + nodeicov("in_weight") + nodeocov("out_weight") + 
     nodematch("in_weight") + nodematch("out_weight"))
summary(fit_weights5)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weights6 <- ergm(net ~ edges + edgecov(net, "weight") + nodeicov("in_weight") + nodeocov("out_weight"))
summary(fit_weights6)

## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weights7 <- ergm(net ~ edges + edgecov(net, "weight"),
                     response = "weight",
                     reference = ~Geometric,
                     control = control.ergm(MCMC.samplesize = 2000,
                                          seed = 123))
summary(fit_weights7)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weights6 <- ergm(net ~ sum + nonzero,
                     response = "weight",
                     reference = ~Geometric,
                     control = control.ergm(seed = 123))#,
                                         # parallel = 4))
summary(fit_weights6)


#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weight8 <- ergm(net ~ edges + nodeocov("out_weight") + transitiveties, 
                   response = "weight", 
                   reference = ~Geometric,
                   control = control.ergm(MCMC.samplesize = 2000,
                                          seed = 123))
summary(fit_weight8)

#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weight9 <- ergm(net ~ sum, 
                   response = "weight", 
                   reference = ~Geometric,
                   control = control.ergm(MCMC.samplesize = 2000,
                                          seed = 123))
summary(fit_weight9)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weight10 <- ergm(net ~ edges + nodeocov("out_weight") + transitiveties, 
                   response = "weight", 
                   reference = ~Geometric,
                   control = control.ergm(MCMC.samplesize = 2000,
                                          seed = 123))
summary(fit_weight10)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
mcmc.diagnostics(fit_weight10)

## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weight11 <- ergm(net ~ transitiveweights("geomean"), 
                   response = "weight", 
                   reference = ~Geometric,
                   control = control.ergm(MCMC.samplesize = 3000,
                                          seed = 123))
summary(fit_weight11)


#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weight12 <- ergm(net ~ edges + mutual(form = "nabsdiff"), 
                     response = "weight", reference = ~Geometric, 
                     control = control.ergm(seed = 123))
summary(fit_weight12)

#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weight13 <- ergm(net ~ edges + mutual(form = "nabsdiff"),
                     response = "weight", 
                     reference = ~Geometric,
                     control = control.ergm(MCMC.samplesize = 2000,
                                            seed = 123))
summary(fit_weight13)

#' 
#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
f13_sim <- simulate(fit_weight13, monitor=~transitiveweights("geomean", "sum", "geomean"), nsim=10, output="stats")
(colnames(f13_sim))

#' 
#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
f13_sim2 <- simulate(fit_weight13, seed=123)
summary(net)
summary(f13_sim2)

#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
# How high is the transitiveweights statistic in the observed network?
f13.obs <- summary(net ~ transitiveweights("geomean", "sum", "geomean"), response="weight")
f13.obs

#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
plot(density(f13_sim))
abline(v = f13.obs)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
min(mean(f12_sim > f12.obs), mean(f12_sim < f12.obs)) * 2

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
network::list.vertex.attributes(net)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weight14 <- ergm(net ~ edges + mutual(form = "nabsdiff"),
                     response = "weight", 
                     reference = ~Geometric,
                     control = control.ergm(MCMC.burnin = 50000, 
                                            MCMC.samplesize = 50000, 
                                            MCMC.interval = 10000, 
                                            MCMLE.maxit = 50,, 
                                            MCMLE.steplength = 0.25, 
                                            MCMLE.density.guard = 100,
                                            seed = 123))
summary(fit_weight14)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
fit_weight15 <- ergm(net_full ~ edges + mutual(form = "nabsdiff"),
                     response = "weight", 
                     reference = ~Geometric,
                     control = control.ergm(MCMC.burnin = 50000, 
                                            MCMC.samplesize = 5000, 
                                            MCMC.interval = 10000, 
                                            MCMLE.maxit = 50,, 
                                            MCMLE.steplength = 0.25, 
                                            MCMLE.density.guard = 100,
                                            seed = 123))
summary(fit_weight15)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
#--- All of these fit_weight20 fail the hotelling.diff.test ----
fit_weight20 <- ergm(net_l_s ~ edges + mutual(form = "nabsdiff"),
                     response = "weight", 
                     reference = ~Geometric,
                     verbose = 5,
                     control = control.ergm(MCMC.burnin = 5000, 
                                            MCMC.samplesize = 5000,
                                            MCMC.interval = 10000, 
                                            MCMLE.maxit = 30,, 
                                            MCMLE.steplength = 0.2, 
                                            MCMLE.density.guard = 10,
                                            parallel = 12,
                                            seed = 123))

# changing steplenght for more sampling
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
                                            seed = 123))

# Changing term mutual to nonzero 
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

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------

# --- This works ----
fit_weight21 <- ergm(net_sib ~ edges + mutual(form = "nabsdiff"),
                     response = "weight", 
                     reference = ~Geometric,
                     control = control.ergm(MCMC.samplesize = 2000,
                                            seed = 123))
summary(fit_weight21)


