source(here::here("R","ensure.R"))
ensure_main_graphs("g_l_s")





#############################################
#---------------  WARNING -------------------
# ONLY RUN IF YOU REALLY NEED TO SEE ALL VIz
# CAN TAKE VERY LONG TO RUN AND PLOT (>1Hr)
#############################################




library(igraph)
library(ggraph)
library(graphlayouts)   # for layout = "stress"

g_draw2 <- g_l_s                         # your graph
set.seed(123)



# Community detection that supports directed graphs
# (uses weights if present, otherwise treats all edges as 1)
w <- if (!is.null(E(g_draw2)$weight)) E(g_draw2)$weight else rep(1, gsize(g_draw2))
comm <- cluster_infomap(g_draw2, e.weights = w)
V(g_draw2)$comm <- membership(comm)

# Build a graph-of-communities by manual aggregation (no contract/simplify needed)
ed <- igraph::as_data_frame(g_draw2, what = "edges")
ed$cf <- V(g_draw2)$comm[ed$from]
ed$ct <- V(g_draw2)$comm[ed$to]
ed$w  <- if (!is.null(ed$weight)) ed$weight else 1

agg <- aggregate(ed$w, by = list(from = ed$cf, to = ed$ct), FUN = sum)
names(agg)[3] <- "weight"
g_comm <- igraph::graph_from_data_frame(agg, directed = is_directed(g_draw2))

# Community sizes for node sizing
csz <- sizes(comm)
V(g_comm)$n <- as.numeric(csz)[match(as.integer(V(g_comm)$name), as.integer(names(csz)))]

ggraph(g_comm, layout = "stress") +
  geom_edge_link(aes(width = weight), alpha = 0.25) +
  geom_node_point(aes(size = n), shape = 21, stroke = 0.3, fill = "white") +
  geom_node_text(aes(label = paste0("C", name)), repel = TRUE, size = 3) +
  scale_size(range = c(4, 18)) + scale_edge_width(range = c(0.2, 2)) +
  theme_graph()



library(tidygraph)

# K-core: keep only high coreness nodes (top 10%)
kc  <- coreness(g_draw2, mode = "all")
keep <- kc >= quantile(kc, 0.90)
g_core <- induced_subgraph(g_draw2, vids = which(keep))

# Label only the top-degree nodes inside the core
deg_core <- degree(g_core)
lab_nodes <- names(sort(deg_core, decreasing = TRUE))[1:30]
V(g_core)$lab <- ifelse(V(g_core)$name %in% lab_nodes, V(g_core)$name, NA)

ggraph(g_core, layout = "stress") +
  geom_edge_link(alpha = 0.08) +
  geom_node_point(aes(size = deg_core), alpha = 0.9) +
  geom_node_text(aes(label = lab), repel = TRUE, size = 3) +
  scale_size(range = c(1.5, 6)) +
  theme_graph()



library(igraph); library(ggraph); library(graphlayouts)
g_core <- {
  kc  <- coreness(g_draw2, mode = "all")
  thr <- quantile(kc, 0.90)  # adjust (e.g., 0.85-0.95) for figure density
  induced_subgraph(g_draw2, vids = which(kc >= thr))
}

deg_core <- degree(g_core, mode = "all")
lab_nodes <- names(sort(deg_core, decreasing = TRUE))[1:min(30, vcount(g_core))]
V(g_core)$lab <- ifelse(V(g_core)$name %in% lab_nodes, V(g_core)$name, NA)

ggraph(g_core, layout = "stress") +
  geom_edge_link(alpha = 0.10) +
  geom_node_point(aes(size = deg_core), alpha = 0.9) +
  geom_node_text(aes(label = lab), repel = TRUE, size = 3) +
  scale_size(range = c(1.5, 6)) +
  theme_graph()

# k-core + labels for top hubs
kc  <- coreness(g_draw2, mode = "all")
degdraw <- degree(g_draw2, mode = "all")
lab_nodes <- names(sort(degdraw, decreasing = TRUE))[1:min(30, vcount(g_draw2))]
V(g_draw2)$lab <- ifelse(V(g_draw2)$name %in% lab_nodes, V(g_draw2)$name, NA)

levels <- sort(unique(kc), decreasing = TRUE)
ring_df <- data.frame(k = levels, r = seq_along(levels))
coords <- matrix(NA_real_, nrow = vcount(g_draw2), ncol = 2)
rad_step <- 1                          # distance between shells (tweak)
for (i in seq_along(levels)) {
  k  <- levels[i]
  ix <- which(kc == k)
  # order nodes within a shell (nice to place hubs evenly)
  ix <- ix[order(degdraw[ix], decreasing = TRUE)]
  m  <- length(ix)
  theta <- if (m == 1) 0 else seq(0, 2*pi, length.out = m+1)[- (m+1)]
  r <- i * rad_step
  coords[ix,1] <- r * cos(theta)
  coords[ix,2] <- r * sin(theta)
}

# labels for top hubs (optional)
lab_ix <- order(degdraw, decreasing = TRUE)[1:min(30, length(degdraw))]
labs   <- rep("", vcount(g_draw2)); labs[lab_ix] <- V(g_draw2)$name[lab_ix]

ggraph(g_draw2, layout = "manual", x = coords[,1], y = coords[,2]) +
  geom_edge_link(alpha = 0.03, linewidth = 0.15) +
  geom_node_point(aes(color = factor(kc), size = degdraw)) +
  scale_size(range = c(1.2, 5)) +
  guides(color = guide_legend(title = "k-core"), size = guide_legend(title = "Degree")) +
  theme_graph()

# plot (ggraph)
ggraph(g_draw2, layout = "manual", x = coords[,1], y = coords[,2]) +
  geom_edge_link(alpha = 0.03, linewidth = 0.10) +
  geom_node_point(aes(size = degdraw), alpha = 0.9) +
  geom_node_text(aes(label = labs), repel = TRUE, size = 3) +
  scale_size(range = c(1.5, 6)) +
  theme_graph()




levels <- sort(unique(kc), decreasing = TRUE)
coords <- matrix(NA_real_, vcount(g_draw2), 2)
for (i in seq_along(levels)) {
  k  <- levels[i]; ix <- which(kc == k)
  ix <- ix[order(degdraw[ix], decreasing = TRUE)]
  m  <- length(ix); theta <- if (m==1) 0 else seq(0, 2*pi, length.out = m+1)[-1]
  r  <- i
  coords[ix,] <- cbind(r*cos(theta), r*sin(theta))
}

# ---- edge thinning: keep links where shells differ by at most 1 ----
ends_idx <- ends(g_draw2, E(g_draw2), names = FALSE)
ku <- kc[ends_idx[,1]]
kv <- kc[ends_idx[,2]]
keep_e <- which(abs(ku - kv) <= 1)        # keep local links
g_plot <- subgraph.edges(g_draw2, keep_e, delete.vertices = FALSE)

library(grid)  # for unit()

ggraph(g_plot, layout = "manual", x = coords[,1], y = coords[,2]) +
  geom_edge_link(alpha = 0.05, linewidth = 0.15) +
  geom_node_point(aes(size = degdraw), alpha = 0.9) +
  scale_size_continuous(
    name   = "Total degree",               # legend title
    breaks = c(100, 200, 300, 400),        # ticks you want
    range  = c(1.3, 5),                    # keep plot sizes the same
    guide  = guide_legend(
      title.position = "top",
      keywidth  = unit(8, "mm"),           # bigger keys
      keyheight = unit(8, "mm"),
      override.aes = list(alpha = 1)       # darker dots in legend
    )
  ) +
  theme_graph() +
  theme(
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 13),
    legend.key.size = unit(8, "mm"),       # extra room if needed
    legend.spacing.y = unit(4, "mm")
  )
