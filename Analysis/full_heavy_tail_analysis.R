## For calculating heavy tail and degree distributions
source(here::here("R","ensure.R"))
ensure_main_graphs("g_l_s")

library(ineq)     # if not installed: install.packages("ineq")

deg <- degree(g_l_s, mode = "all")   # or "out" / "in" as needed
gini_k <- ineq::Gini(deg)

# Print with four decimals
sprintf("Gini_k = %.4f", gini_k)

library(poweRlaw)   # install.packages("poweRlaw")


# Set up discrete power-law model
pl <- displ$new(deg)

# Estimate xmin and alpha via maximum likelihood
est_xmin <- estimate_xmin(pl)
pl$setXmin(est_xmin)
est_par  <- estimate_pars(pl)
alpha    <- est_par$pars

# Optional: goodness-of-fit and p-value
powerlaw_gof <- bootstrap_p(pl, no_of_sims = 500, threads = 12, seed = 123)

cat(sprintf("Tail exponent alpha = %.2f (xmin = %d, p = %.3f)\n",
            alpha, pl$getXmin(), powerlaw_gof$p))




# Strength unbalance 
sprintf("Gini_k all = %.4f", ineq::Gini(strength(g_l_s, mode = "all"))) 
sprintf("Gini_k in = %.4f", ineq::Gini(strength(g_l_s, mode = "in"))) 
sprintf("Gini_k out = %.4f", ineq::Gini(strength(g_l_s, mode = "out"))) 

# Strength unbalance 
sprintf("Gini_k all = %.4f", ineq::Gini(strength(g_l_s, mode = "all", weights = E(g_l_s)$weight))) 
sprintf("Gini_k in = %.4f", ineq::Gini(strength(g_l_s, mode = "in", weights = E(g_l_s)$weight))) 
sprintf("Gini_k out = %.4f", ineq::Gini(strength(g_l_s, mode = "out", weights = E(g_l_s)$weight))) 
