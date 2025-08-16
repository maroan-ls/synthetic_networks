source(here::here("R","ensure.R"))
ensure_ergm_sims()
ensure_main_graphs("net_wire")


# core packages
library(network)      # network object
library(ergm)         # ERGM machinery
library(ergm.count)   # count-valued extensions
set.seed(123)        # reproducibility



# ---- (optional but recommended) rescale --------------------
# very large counts can cause extreme Poisson means
scale_factor <- 1e6          # pick a power of 10
net_wire %e% "w_scale" <- round(net_wire %e% "weight" / scale_factor)
summary(net_wire %e% "w_scale")


# THIS IS WORKING AND FITTING NON DEGERNERATE 
fit_wire0 <- ergm(
  net_wire ~ nonzero + sum,
  response = "w_scale",
  reference = ~Geometric,
  control = control.ergm(
    MCMC.burnin       = 5e5,
    MCMC.interval     = 1e6,      
    MCMC.samplesize   = 6e4,
    MCMC.return.stats = Inf,
    parallel          = 4,
    seed              = 123
  )
)



# AFTER SUCESSFUL fitwire0 first actual tuning
## THIS WORKS


### 1)  flag the high-activity senders -------------------------
outdeg <- sna::degree(net_wire, cmode = "outdegree")
# add a logical (0/1) attribute to the *network* object
net_wire %v% "big_sender" <- outdeg >= 6    # choose the cut-point you prefer

### 2)  fit ----------------------------------------------------

tm <- system.time({                 # Measures the time it takes to run
  
  fit_wire2 <- ergm(
    net_wire ~ nonzero + sum
    + greaterthan(threshold = c(400,1000))  # fattens the tail
    + nodeofactor("big_sender")              # boosts out-degree variance
    + offset(mutual),                        # forces reciprocity down
    offset.coef = c(-5),            # large negative penalty on mutual ties
    
    response    = "w_scale",
    reference   = ~Geometric,
    control     = control.ergm(
      MCMC.burnin     = 5e5,
      MCMC.interval   = 1e6,
      MCMC.samplesize = 6e4,
      MCMC.return.stats = Inf,
      parallel        = 4,
      seed            = 123)
  )
  
})


b

###
outdeg <- sna::degree(net_wire, cmode = "outdegree")
# add a logical (0/1) attribute to the *network* object
net_wire %v% "big_sender" <- outdeg >= 4    # choose the cut-point you prefer

### 2)  fit ----------------------------------------------------

tm <- system.time({                 # Measures the time it takes to run it
  
  fit_wire3 <- ergm(
    net_wire ~ nonzero + sum
    + greaterthan(threshold = c(350,1000))  # fattens the tail
    + nodeofactor("big_sender")              # boosts out-degree variance
    + offset(mutual),                        # forces reciprocity down
    offset.coef = c(-4),            # large negative penalty on mutual ties
    
    response    = "w_scale",
    reference   = ~Geometric,
    control     = control.ergm(
      MCMC.burnin     = 5e5,
      MCMC.interval   = 1e6,
      MCMC.samplesize = 6e4,
      MCMC.return.stats = Inf,
      parallel        = 4,
      seed            = 123)
  )
  
})


# Just so I have a minimal model that runs in minutes, it is derived from above to test terms 
fitw1 <- ergm(net_wire ~ nonzero + sum
              + greaterthan(threshold = 1000)
              + sociality(type = "sender"),      # tamps down reciprocity
              response = "w_scale",
              reference = ~Geometric,
              control = control.ergm(
                MCMC.burnin       = 20000,
                MCMC.interval     = 10000,      
                MCMC.samplesize   = 10000,
                MCMC.return.stats = Inf,
                parallel          = 4,
                seed              = 123
              )
)

mcmc.diagnostics(fit_wire2)