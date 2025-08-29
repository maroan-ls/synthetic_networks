# Source Rmd: BaselineGraphs/init_basline_samplegraphs.Rmd
# Generated: 2025-08-16 08:00
# Purpose: converted from Rmd for pipeline/audit use


source(here::here("R","ensure.R"))
ensure_main_graphs() 

library(igraph)

suppressPackageStartupMessages({ library(here) })
set.seed(123)

#' ---
#' title: "Thesis baseline samplegraphs"
#' author: "Maroan El Sirfy"
#' date: "2025-01-21"
#' output: html_document
#' ---
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
g_er <- sample_gnm(vcount(g_l_s), ecount(g_l_s), directed = TRUE)
g_er_sub <- sample_gnm(vcount(g_wire), ecount(g_wire), directed = TRUE)
transitivity(g_er, type = "average")
mean_distance(g_er, directed = TRUE)
edge_density(g_er_sub)
mean(degree(g_er))

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
swg <- sample_smallworld(dim = 1, size = vcount(g_l_s), nei = 4, p = 0.5, loops = FALSE, multiple = FALSE)
swg_sub <- sample_smallworld(dim = 1, size = vcount(g_wire), nei = 6, p = 0.5, loops = FALSE, multiple = FALSE)
mean(degree(swg, mode = "all"))
transitivity(swg, type = "average")
mean_distance(swg, directed = TRUE)
mean_distance(swg_sub, directed = TRUE)
ecount(swg)
ecount(swg_sub)
edge_density(swg_sub)

## -----------------------------------------------------------------------------------------------------------------------------------------
swg <- as_directed(sample_smallworld(1,size = vcount(g_l_s), 
                                     nei = round(ecount(g_l_s) / vcount(g_l_s)),p = 0.5),
                   mode = "random")
swg_sub <- as_directed(sample_smallworld(1, size = vcount(g_wire),
                                         nei = round(ecount(g_wire)/vcount(g_wire)), p = 0.5),
                       mode = "random")

#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
g_pa <- sample_pa(n=vcount(g_l_s), m=4, directed=TRUE, algorithm = c("bag"))
g_pa_sub <- sample_pa(n=vcount(g_wire), m=7, directed=TRUE, algorithm = c("bag"))
mean(degree(g_pa_sub, mode = "in"))
count_components(g_pa)
transitivity(g_pa, type = "average")
mean_distance(g_pa, directed = TRUE, weights = NA)
ecount(g_pa_sub)
edge_density(g_pa_sub)

#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
# Degree distribution
dd_g_bg <- degree_distribution(g_er, cumulative = FALSE)
cumdd_g_bg <- degree_distribution(g_er, cumulative = TRUE)

#' 
#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
# 2. Plot separate histograms (base R)
par(mfrow=c(1,2))  # 2 plots side-by-side for convenience

plot(dd_g_bg, type="s", col="blue", xaxt="n", xlab="Degree", ylab="Relative Frequency",
     main = "Degree Distribution")
axis(1, at=NULL, labels=TRUE)
plot(cumdd_g_bg, type="s", col="blue", xaxt="n", xlab="Degree", ylab="Cumulative Frequency",
     main = "Cumulative Degree Distribution")
axis(1, at=NULL, labels=TRUE)



source(here::here("R","ensure.R"))
cache_write(
  list(g_er = g_er,   # << replace with your names
       g_er_sub     = g_er_sub,
       swg      = swg,
       swg_sub = swg_sub,
       g_pa = g_pa,
       g_pa_sub = g_pa_sub),
  here::here("data/derived/graphs","baseline_graphs.rds")
)

