
source(here::here("R","ensure.R"))

# Skip recomputation if a cached fit exists (default)
RECOMPUTE_FIT <- getOption("recompute_fit", FALSE)

if (!RECOMPUTE_FIT && file.exists(here::here("data/derived/models","ergm_fit.rds"))) {
  fit_wire2 <- readRDS(here::here("data/derived/models","ergm_fit.rds"))
  message("Loaded cached ERGM fit. Set options(recompute_fit=TRUE) to recompute.")
} else {
  ensure_main_graphs()  # make sure graphs are available before fitting

  
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
  
  mcmc.diagnostics(fit_wire2)
  
  
    saveRDS(fit_wire2, here::here("data/derived/models","ergm_fit.rds"))
}



