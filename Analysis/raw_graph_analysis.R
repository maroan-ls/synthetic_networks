

source(here::here("R","ensure.R"))
ensure_main_graphs("g")  



# ?????? packages ?????
library(igraph)
library(ggraph)
library(ggplot2)
library(patchwork)      # combines the two panels

# ?????? raw graph (already in memory) ??????????????????????????????????????????????????????????????????????????????
cmp      <- components(g, mode = "weak")
giant_id <- which.max(cmp$csize)

keep_idx <- which(cmp$membership != giant_id)          # 45 vertices
g_out    <- induced_subgraph(g, keep_idx)

## A ? component-size bar chart -----------------------------
sizes_small <- cmp$csize[cmp$csize < cmp$csize[giant_id]]

p_sizes <- ggplot(data.frame(size = small_sizes)) + 
  geom_bar(aes(factor(size)), fill = "#de2d26", width = .7) + 
  labs(x = "Component size", y = "Number of components", 
       subtitle = paste0("Largest connected component = ", 
                         scales::comma(cmp$csize[giant_id]), " vertices")) + 
  theme_minimal(base_size = 9) + 
  theme(plot.subtitle = element_text(size = 9, vjust = -0.6))



## B ? tiny-component gallery (one facet per component) -----
V(g_out)$facet <- factor(components(g_out)$membership)

p_gallery <- ggraph(g_out, layout = "circle") +
  geom_edge_link(
    colour = "grey60",
    arrow  = arrow(length = unit(2, "mm"), type = "closed"),
    end_cap = circle(2, "mm")        # leaves space so arrow isn't clipped
  ) +
  geom_node_point(colour = "#de2d26", size = 3) +
  facet_nodes(~ facet, ncol = 4) +
  theme_graph(base_size = 9) +
  labs(caption = "Each facet: one disconnected component (sizes 2-4, directed)")

## combine & export -----------------------------------------
figure <- p_sizes + p_gallery + plot_layout(widths = c(1, 3))
figure
ggsave("micro_components.pdf", figure, width = 9, height = 7,
       device = cairo_pdf)        # vector; swap to .png if needed


nodes <- data.frame(id = keep, group = "outlier")
edges <- as_data_frame(g_out, "edges")[, 1:2]
names(edges) <- c("from", "to")

visNetwork(nodes, edges, height = "600px") %>%
  visIgraphLayout(layout = "layout_in_circle") %>%  # ultra-fast
  visNodes(color = list(background = "#de2d26"))    %>%
  visOptions(highlightNearest = TRUE)