visNetwork::visIgraph(g_l_s) %>%
  visIgraphLayout(layout = "layout_with_lgl")



visNetwork::visIgraph(g_sib) %>%
  visIgraphLayout(layout = "layout_in_circle")
