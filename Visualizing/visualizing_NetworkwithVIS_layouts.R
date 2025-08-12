

visNetwork::visIgraph(g_out) #%>%
  #visIgraphLayout(layout = "layout_with_lgl")


visNetwork::visIgraph(g_l_s) %>%
  visIgraphLayout(layout = "layout_with_lgl")



visNetwork::visIgraph(g_sib) %>%
  visIgraphLayout(layout = "layout_in_circle")


visNetwork::visIgraph(g_ego) %>%
  visIgraphLayout(layout = "layout_in_circle")

visNetwork::visIgraph(g_wire) %>%
  visIgraphLayout(layout = "layout_in_circle")






vis <- visNetwork::visIgraph(g_out) %>%
#visIgraphLayout(layout = "layout_with_lgl"
visIgraphLayout(layout = "layout_with_sugiyama") %>%  # ultra-fast
  visNodes(color = list(background = "#de2d26"))    %>%
  visOptions(highlightNearest = TRUE)

