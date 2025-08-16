
# This document is just many many tries of ergm that have been somewhat documentd
# All models here will fail for one reason or another

# library(ergm)
# library(igraph)
# library(network)
# library(intergraph)
# library(ergm.count)
# library(statnet)


#--- Trying to adjust scaling, checking for distribution ----

# Cchecking the distribution with AIC which of the ergm.count supported fit better
w <- get.edge.attribute(net_l_s, "weight")
w <- w[w > 0]                      # ignore the zeros; they are modelled separately
dispersion <- var(w) / mean(w)



lambda <- mean(w)
logLikPois <- sum(dpois(w,  lambda, log = TRUE))

p      <- 1 / (1 + lambda)          # MLE for the geometric with support 0,1,2,.
logLikGeom <- sum(dgeom(w, p, log = TRUE))

AIC_Pois  <- -2*logLikPois  + 2      # k = 1
AIC_Geom  <- -2*logLikGeom  + 2


p_obs <- 16177 / (network.dyadcount(net_l_s, directed = TRUE))  # 26 977 002
init_nonzero <- log(p_obs / (1 - p_obs))  



#####################
## 3·1  Down-scale the weights ??? log10 plus rounding
net_l_s %e% "w_log10" <- round(log10(net_l_s %e% "weight" + 1))




## 3·2  Fit a baseline model on the scaled weights
# --- This fails with exceeding edges factor 20 -----------
ctrl <- control.ergm(
  init.method      = "zeros",
  init             = c(init_nonzero, 0),   # c(nonzero, sum)
  MCMC.prop.weights = "TNT",
  MCMC.prop.args   = list(p0 = 0.98),      # 98 % of proposals set weight to 0
  MCMC.burnin      = 2e5,
  MCMC.interval    = 2e4,
  MCMC.samplesize  = 1e4,
  MCMLE.density.guard.min = 1e4,           # allow up to 1× the observed edges
  parallel         = 12)

fit0 <- ergm(net_l_s ~ nonzero + sum,
             response   = "w_log10",
             reference  = ~Geometric, # or ~Poisson + CMP
             verbose = TRUE, 
             control    = ctrl)


p_obs  <- 16177 / network.dyadcount(net_l_s, directed = TRUE)  # 0.000597
off.nz <- log(p_obs / (1 - p_obs))   # ??? -7.42  (Geometric log-odds of non-zero)



# --- This fails with exceeding edges factor 5 ----

fit0 <- ergm(
  net_l_s ~ offset(nonzero) + sum,           # no free parameter on edge presence
  offset.coef = off.nz,                      # fixed penalty
  response   = "w_log10",                      # log10-scaled weights
  reference  = ~Geometric,
  verbose = TRUE,
  control = control.ergm(
    init.method      = "zeros",
    MCMC.prop.weights = "TNT",
    MCMC.prop.args   = list(p0 = 0.995),   # 99.5 % of proposals jump to 0
    MCMC.burnin      = 2e5,
    MCMC.interval    = 2e4,
    MCMC.samplesize  = 2e4,
    MCMLE.density.guard.min = 16177,       # observed count ??? guard trips only if > 16100
    MCMLE.density.guard     = 5,           # .and > 5 × obs
    parallel         = 12)
)


# --- This fails with exceeding edges factor 20 -----------
# Even though constraint is now observed ??? 
# Shit is on fire -> apparently ~observed is not useful in ergm.count fuc!
fit0 <- ergm(
  net_l_s ~ sum,                         # weight parameter only
  response     = "w_log10",                # log10-scaled weights
  reference    = ~Geometric,
  
  ## freeze the SUPPORT, not just the count
  constraints  = ~observed,              # proposals may touch only existing ties
  verbose = TRUE,
  control = control.ergm(
    init.method       = "zeros",
    MCMC.prop.weights = "random",      # "random" works with class "c" + observed
    MCMC.prop.args    = list(p0 = 0.999), # 99 % of proposals set the weight to 0
    MCMC.burnin       = 2e5,
    MCMC.interval     = 1e4,
    MCMC.samplesize   = 1e4,
    parallel          = 12,
    MCMLE.density.guard.min = 16177   # guard won't trigger: edge set is fixed
    )
)



# build the model frame but don't estimate yet
### ------ This fails with hoteling error -----
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


# --- This fails with exceeding edges factor 20 ----
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


# Scaling weights
net_l_s %e% "w_log10" <- round(log10(net_l_s %e% "weight" + 1))
summary(net_l_s %e% "w_log10")


# --- This fails with exceeding edges factor 20 ---- 
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



# ----------------------------- 2iterations test run ------------------
#--- Fails the the hotelling.diff.test ----

fit0_2it <- ergm(net_l_s ~ offset(nonzero) + sum,
                 offset.coef = -25,
                 response    = "w_l10",
                 reference   = ~Geometric,
                 verbose = TRUE,
                 control = control.ergm(
                   MCMLE.maxit = 2,           # stop after 2 outer iterations
                   MCMC.burnin = 1e5,
                   MCMC.samplesize = 5e3,
                   MCMC.interval = 5e3,
                   MCMC.prop.args = list(p0 = .999),
                   parallel = 12,
                   seed = 123))


# --- This fails with exceeding edges factor 20 ----




fit1 <- ergm(
  net_l_s ~ offset(nonzero) +                 # keep edges frozen for now
    sum + CMP,                        # weight (sum)  + dispersion
  offset.coef = -25,                          # ??? exp(-25) ??? practically no new edges
  response    = "w_l10",
  reference   = ~Poisson,                     # Poisson + CMP lets tail widen
  init        = c(sum = -5, CMP = 0.5),       # start with weights strongly penalised
  verbose           = TRUE,
  control = control.ergm(
    init.method       = "zero",
    MCMC.prop.weights = "random",
    MCMC.prop.args    = list(p0 = 0.9999),
    MCMC.burnin       = 2e5,
    MCMC.interval     = 1e4,
    MCMC.samplesize   = 1e4,
    parallel          = 12,
    seed = 123)
)






# NEW TRY ------
# The following model fit3 does not fail but runs forever -> no convergence


# tame the weights once: log10  âžœ divide by 3  âžœ round
net_l_s %e% "w_l10s" <- round( log10(net_l_s %e% "weight" + 1) / 3 )
summary(net_l_s %e% "w_l10s")   # should be 0 â€¦ 4

# build a labelled start vector of length 3
init_vec <- c("(offset) nonzero" = -40,   # placeholder for the offset
              sum                 = -5,
              CMP                 = 0.5)

fit3 <- ergm(
  net_l_s ~ offset(nonzero) + sum + CMP,
  offset.coef = -40,                       # fixed offset value
  response    = "w_l10s",
  reference   = ~Poisson,
  verbose = TRUE,
  control = control.ergm(
    init              = init_vec,   # â† now the right length
    MCMLE.maxit       = 2,          # scout run
    MCMC.burnin       = 1e5,
    MCMC.interval     = 5e3,
    MCMC.samplesize   = 5e3,
    MCMC.prop.weights = "random",
    MCMC.prop.args    = list(p0 = .999),
    MCMLE.density.guard.min = 1e12,
    MCMLE.density.guard     = 1e12,
    parallel          = 12,
    seed              = 123))


# --- This fails with exceeding edges factor 20 ----

fit_w <- ergm(
  net_l_s ~ sum + CMP,            # no (offset) nonzero term at all
  response    = "w_l10s",         # 0â€¦4 range we already created
  reference   = ~Poisson,
  constraints = ~observed,        # <-- sampler may touch only the 16 177 ties
  control = control.ergm(
    init.method     = "zeros",
    MCMLE.maxit     = 10,      # converges in <1 h with 4 cores
    MCMC.burnin     = 2e4,
    MCMC.interval   = 1e3,
    MCMC.samplesize = 3e3,
    MCMC.prop.weights = "random",
    parallel        = 4,
    seed            = 123),
  verbose = TRUE)





