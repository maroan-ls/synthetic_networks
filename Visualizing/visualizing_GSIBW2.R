source(here::here("R","ensure.R"))
ensure_main_graphs("g_wire")


# 1) Put labels & colors onto the igraph itself (works nicely with visIgraph)
V(g_wire)$label <- V(g_wire)$name

is_imp <- V(g_wire)$name %in% importantbanks
V(g_wire)$color        <- ifelse(is_imp, "#FFD166", "#D3D3D3")   # background
V(g_wire)$frame.color  <- ifelse(is_imp, "#8D6B00", "#7F8C8D")   # border
V(g_wire)$size         <- ifelse(is_imp, 28, 22)                 # optional: a touch larger

visNetwork::visIgraph(g_wire) %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  # 2) Make labels more visible
  visNodes(font = list(size = 22, face = "bold"),
           scaling = list(label = list(enabled = TRUE))) %>%
  # Keep the circle fixed so labels don’t jiggle around
  visPhysics(enabled = FALSE) %>%
  # Helpful interaction so overlap bothers you less while exploring
  visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1)) %>%
  visInteraction(hover = TRUE, navigationButtons = TRUE)


# Attempt 2 
# draw-only copy so your analysis graph stays intact
g_draw <- g_wire

# kill scaling sources
E(g_draw)$value  <- NA
E(g_draw)$weight <- NA   # only on the copy!

# fixed width + visible color
E(g_draw)$width <- 1.8
E(g_draw)$color <- "rgba(179,134,44,0.85)"  # less transparent

V(g_draw)$label       <- V(g_draw)$name
V(g_draw)$color       <- ifelse(is_imp, "#F0A202", "#C9C9C9")
V(g_draw)$frame.color <- ifelse(is_imp, "#7A5A00", "#888888")

visIgraph(g_draw) %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  visEdges(smooth = FALSE) %>%                     # no scaling argument at all
  visNodes(font = list(size = 24, face = "bold",
                       strokeWidth = 4, strokeColor = "#FFFFFF"),
           scaling = list(label = list(enabled = FALSE))) %>%
  visLegend(addNodes = data.frame(label = c("GSIBs","Other"),
                                  shape = "dot",
                                  color = c("#F0A202","#C9C9C9")),
            useGroups = FALSE) %>%
  visPhysics(enabled = FALSE)

#---
vis <- toVisNetworkData(g_draw)  # get nodes/edges data frames

bump_up   <- c("INVESTEC BANK")
bump_down <- c("EMIRATES NBD BANK PJSC", "BLACKSTONE")  # adjust names as needed

vis$nodes$font.size <- 24
vis$nodes$font.face <- "bold"
vis$nodes$font.strokeWidth <- 4
vis$nodes$font.strokeColor <- "#FFFFFF"

vis$nodes$font.vadjust[vis$nodes$label == bump_up]   <-  -20  # push label up
vis$nodes$font.vadjust[vis$nodes$label == bump_down] <- -100  # push label down

visNetwork(vis$nodes, vis$edges) %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  visEdges(smooth = FALSE) %>%                     # no scaling argument at all
  visNodes(font = list(size = 24, face = "bold",
                       strokeWidth = 4, strokeColor = "#FFFFFF"),
           scaling = list(label = list(enabled = FALSE))) %>%
  visLegend(addNodes = data.frame(label = c("GSIBs","Other"),
                                  shape = "dot",
                                  color = c("#F0A202","#C9C9C9")),
            useGroups = FALSE) %>%
  visPhysics(enabled = FALSE)