library(igraph)
layout <- layout_with_lgl(g_l_s)
visNetwork::visIgraph(g_l_s, layout = "layout_nicely")

