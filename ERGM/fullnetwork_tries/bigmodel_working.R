# In this document there is working models are put
# working does not mean good, Just means it did not fail during ergm()
# It also includes first analysis to see if the models are any good (spoiler they are not)

# library(ergm)
# library(igraph)
# library(network)
# library(intergraph)
# library(ergm.count)
# library(statnet)



# --- This works ----
# Do not try to parallelize, will fail otherwise
fit_weight19 <- ergm(net_sib ~ edges + mutual(form = "nabsdiff"),
                     response = "weight", 
                     reference = ~Geometric,
                     control = control.ergm(MCMC.samplesize = 2000,
                                            seed = 123))



mcmc.diagnostics(fit_weight19, center=TRUE)


### THIS fit0 WORKS finally ----
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


net_l_s %e% "w_l10h" <- round((log10(net_l_s %e% "weight" + 1))/2)





#-------------------------THIS fit2 WORKS------------------------------

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
# -----------------------------------------------------------------

## 3A·1 acceptance ------------------------
acc <- vapply(fit2$sample, attr, numeric(1), which = "acceptance.rate")
mean_acc <- mean(acc, na.rm = TRUE)

## 3A·2 trace span ------------------------
library(coda)
range_sum <- range(as.mcmc(fit2$sample[[1]][ , "sum"]))

## 3A·3 weight tail check -----------------
sim3 <- simulate(fit2, nsim = 3,
                 constraints = ~observed,          # freezes zero/non-zero status
                 control = control.simulate.ergm(
                   MCMC.burnin   = 2e4,
                   MCMC.interval = 5e3,
                   MCMC.prop.weights = "random"))
tail_max <- sapply(sim3, \(g) max(g %e% "w_l10h"))

sapply(fit2$sample, function(ch) attr(ch, "acceptance.rate"))




#------------------ THIS IS TESTING SAMPLE CODE FOR ABOVE MODELS------------

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

