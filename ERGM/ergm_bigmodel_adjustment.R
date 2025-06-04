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


# ---  prep  -------------------------------------------------------------
net_l_s %e% "w_l10" <- round(log10(net_l_s %e% "weight" + 1))

# choose a coefficient that makes P(new edge) ??? exp(-20)  ??? 2×10??????
off.nz <- -20  

fit0 <- ergm(
  net_l_s ~ offset(nonzero) + sum,
  offset.coef = off.nz,
  response    = "w_l10",
  reference   = ~Geometric,
  verbose = TRUE,
  control = control.ergm(
    init.method       = "zeros",
    MCMC.prop.weights = "random",        # works with valued + offset
    MCMC.prop.args    = list(p0 = 0.9999),
    MCMC.burnin       = 2e5,
    MCMC.interval     = 1e4,
    MCMC.samplesize   = 1e4,
    parallel          = 12,
    MCMLE.density.guard.min = 1e5,       # guard will never trip now
)
)
