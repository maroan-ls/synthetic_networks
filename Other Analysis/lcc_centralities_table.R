library(igraph)
library(dplyr)
library(tidyr)

# --- g is the igraph object for the full graph -------------------
E(g_l_s)$len <- 1 / E(g_l_s)$weight          # inverse-weight distances

# 1. Compute centralities
centralities_full_2 <- tibble(
  bank          = V(g_l_s)$name,
  Degree        = degree(g_l_s, mode = "all"),
  Strength      = strength(g_l_s, weights = E(g_l_s)$weight),
  Betweenness_1w = betweenness(g_l_s, directed = TRUE, weights = E(g_l_s)$len),
  Eigenvector_w  = eigen_centrality(g_l_s, directed = TRUE,
                                    weights = E(g_l_s)$weight)$vector,
  PageRank_w     = page_rank(g_l_s, directed = TRUE,
                             weights = E(g_l_s)$weight)$vector
)

# 2. Extract Top-10 for each metric
top10_centralities_long <- centralities_full_2 %>%
  pivot_longer(-bank, names_to = "metric", values_to = "value") %>%
  group_by(metric) %>%
  arrange(desc(value)) %>%
  slice_head(n = 10) %>%           # keep the top 10
  mutate(rank = row_number()) %>%
  ungroup()

# 3. Re-shape into the publication format (rows = rank, cols = metric)
central_full <- top10_centralities_long %>%
  pivot_wider(id_cols = rank, names_from = metric, values_from = bank) %>%
  arrange(rank)

# 4. Inspect or export
print(central_full, n = Inf)        # view in console
# knitr::kable(central_full, booktabs = TRUE)  # LaTeX table if using knitr