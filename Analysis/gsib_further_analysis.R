# GSIB GWIRE JUSTIFICATION

source(here::here("R","ensure.R"))
ensure_main_graphs()

gsib_ids <- match(importantbanks, V(g_l_s)$name)   # 1-based vertex indices
#  If it is already a numeric index vector, just:
# gsib_ids <- importantbanks
# -------------------------------------------------------------------

# 1.  Exposure shares -----------------------------------------------
#  (total = in- + out-strength)  -------------------------------------
total_exposure_lcc <- strength(g_l_s, , weights = E(g_l_s)$weight)

gsib_exposure  <- sum(in_strength[gsib_ids]) +
  sum(out_strength[gsib_ids])

share_exp_total <- gsib_exposure / sum(total_exposure_lcc)       # 0-1 scale

#  Optional: out-strength share only (your 54 %)
sum(out_strength[gsib_ids]) /
  sum(out_strength)                    # 0.5400.
# -------------------------------------------------------------------

# 2.  Inverse-weight betweenness shares -----------------------------
bw_inv <- weighted_degree$Betweenness       # vector already ??? 1 / weight

total_bw <- sum(bw_inv)
gsib_bw  <- sum(bw_inv[gsib_ids])

share_bw <- gsib_bw / total_bw              # share of flow through GSIBs
# -------------------------------------------------------------------

# 3.  Rank positions in that centrality -----------------------------
bw_rank <- rank(-bw_inv, ties.method = "first")  # 1 = highest

gsibs_in_top20 <- sum(bw_rank[gsib_ids] <= 14)
gsibs_in_top50 <- sum(bw_rank[gsib_ids] <= 38)
# -------------------------------------------------------------------

# 4.  Strength-Gini for the 38-node GSIB core -----------------------
strength_core <- strength(g_wire,
                          mode   = "all",
                          weights = E(g_wire)$weight)

G_s_core <- Gini(strength_core)             # e.g. 0.84
# -------------------------------------------------------------------
# ----------------  degree (binary)  --------------------------------
deg_bin <- degree(g_l_s, mode = "all")
rank_bin <- rank(-deg_bin, ties.method = "first")

sum(rank_bin[gsib_ids] <= 30)   # GSIBs in binary top-50
median(rank_bin[gsib_ids])      # median GSIB rank

# ----------------  strength (weighted)  ----------------------------
total_exposure_lcc <- strength(g_l_s, mode = "all", weights = E(g_l_s)$weight)
rank_str <- rank(-total_exposure_lcc, ties.method = "first")

sum(rank_str[gsib_ids] <= 30)   # GSIBs in weighted-degree top-50
median(rank_str[gsib_ids])      # median GSIB rank

# 5.  (Optional) Degree Gin?s on the full LCC -----------------------
deg_all  <- degree(lcc, mode = "all")
deg_out  <- degree(lcc, mode = "out")
deg_in   <- degree(lcc, mode = "in")

G_k_all  <- Gini(deg_all)   # 0.8015
G_k_out  <- Gini(deg_out)   # 0.9543
G_k_in   <- Gini(deg_in)    # 0.8086
# -------------------------------------------------------------------

#  Quick print so you see numbers in console ------------------------
print(round(c(share_exp_total = share_exp_total,
              share_bw        = share_bw,
              GSIBs_top20_bw  = gsibs_in_top20,
              GSIBs_top50_bw  = gsibs_in_top50,
              G_s_core        = G_s_core), 4))



# Strength unbalance 
sprintf("Gini_k all = %.4f", ineq::Gini(strength(g_wire, mode = "all"))) 
sprintf("Gini_k in = %.4f", ineq::Gini(strength(g_wire, mode = "in"))) 
sprintf("Gini_k out = %.4f", ineq::Gini(strength(g_wire, mode = "out"))) 

# Strength unbalance 
sprintf("Gini_k all = %.4f", ineq::Gini(strength(g_wire, mode = "all", weights = E(g_wire)$weight))) 
sprintf("Gini_k in = %.4f", ineq::Gini(strength(g_wire, mode = "in", weights = E(g_wire)$weight)))
sprintf("Gini_k out = %.4f", ineq::Gini(strength(g_wire, mode = "out", weights = E(g_wire)$weight))) 



mean(betweenness(g_wire, directed = TRUE, weights = 1/E(g_wire)$weight, normalized = TRUE))
mean(betweenness(g_l_s, directed = TRUE, weights = 1/(E(g_l_s)$weight/1e6), normalized = TRUE))

mean(betweenness(g_wire, directed = TRUE, weights = NA, normalized = TRUE))
mean(betweenness(g_l_s, directed = TRUE, weights = NA, normalized = TRUE))

mean_distance(g_wire, directed = TRUE, weights = NA)/(vcount(g_wire)-1)
mean_distance(g_l_s, directed = TRUE, weights = NA)/(vcount(g_wire)-1)

assortativity_degree(g_wire, directed = TRUE)

