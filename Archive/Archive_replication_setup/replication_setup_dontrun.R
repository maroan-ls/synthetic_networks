#=== Robust helpers for CodeDepends (inputs/outputs) ===
  get_cd_out <- function(info){
    ns <- asNamespace("CodeDepends")
    if (exists("getOutputs", envir = ns, inherits = FALSE))      get("getOutputs", ns)(info)
    else if (exists("outputs", envir = ns, inherits = FALSE))    get("outputs", ns)(info)
    else if ("outputs" %in% slotNames(info))                     info@outputs
    else character(0)
  }
get_cd_in <- function(info){
  ns <- asNamespace("CodeDepends")
  if (exists("getInputs", envir = ns, inherits = FALSE))       get("getInputs", ns)(info)
  else if (exists("inputs", envir = ns, inherits = FALSE))     get("inputs", ns)(info)
  else if ("inputs" %in% slotNames(info))                      info@inputs
  else character(0)
}

suppressPackageStartupMessages({
  library(CodeDepends); library(igraph); library(purrr)
  library(fs); library(stringr); library(knitr); library(here); library(readr)
})

# 1) collect files (exclude renv/.git)
R_files  <- fs::dir_ls(here(), glob = "**/*.R", recurse = TRUE)
R_files  <- R_files[!grepl("^renv/|^\\.git/", fs::path_rel(R_files, here()))]

# Rmds     <- fs::dir_ls(here(), glob = "**/*.Rmd", recurse = TRUE)
# Rmds     <- Rmds[!grepl("^renv/|^\\.git/", fs::path_rel(Rmds, here()))]
# 
# # also extract code from any remaining Rmds (if you kept some on purpose)
# purls <- character(0)
# if (length(Rmds)) {
#   purls <- map_chr(Rmds, ~{
#     out <- fs::file_temp(ext = "R")
#     knitr::purl(.x, output = out, quiet = TRUE)
#     out
#   })
# }

#files <- unique(c(R_files))#, purls))

# 2) variable-level deps
# 2) Try CodeDepends first
file_edges <- NULL
file_cd_ok <- TRUE
try({
  file_infos <- setNames(lapply(R_files, CodeDepends::readScript), R_files)
  file_outs  <- lapply(file_infos, get_cd_out)
  file_ins   <- lapply(file_infos, get_cd_in)
  
  file_edges <- map_dfr(names(file_infos), function(a){
    oa <- file_outs[[a]]; if (!length(oa)) return(NULL)
    map_dfr(setdiff(names(file_infos), a), function(b){
      ib <- file_ins[[b]]
      vars <- intersect(oa, ib)
      if (length(vars) > 0)
        tibble(from = a, to = b, var = vars)
    })
  })
}, silent = TRUE)

# 3) Fallback "lite" analysis if CodeDepends failed to produce edges
if (is.null(file_edges) || !nrow(file_edges)) {
  file_cd_ok <- FALSE
  message("CodeDepends edges unavailable; using regex-based lite analysis.")
  parse_io <- function(path){
    txt <- readLines(path, warn = FALSE)
    txt <- txt[!grepl("^\\s*#", txt)]
    txt <- paste(txt, collapse = "\n")
    outs <- unique(str_match_all(txt, "(?m)^\\s*([.A-Za-z][\\w.]*)\\s*(?:<-|=)\\s*(?!function\\s*\\()")[[1]][,2])
    syms <- unique(str_match_all(txt, "\\b([.A-Za-z][\\w.]*)\\b(?!\\s*\\()")[[1]][,2])
    consts <- c("NA","NaN","Inf","TRUE","FALSE","T","F")
    inputs <- setdiff(syms, c(outs, consts))
    list(outs = outs, ins = inputs)
  }
  file_io <- lapply(R_files, parse_io); names(file_io) <- R_files
  file_edges <- map_dfr(names(file_io), function(a){
    oa <- file_io[[a]]$outs; if (!length(oa)) return(NULL)
    map_dfr(setdiff(names(file_io), a), function(b){
      ib <- file_io[[b]]$ins
      if (length(intersect(oa, ib)) > 0)
        data.frame(from = a, to = b, stringsAsFactors = FALSE)
    })
  })
}

# 4) Build graph + summaries
file_dag_g <- if (nrow(file_edges)) graph_from_data_frame(file_edges, directed = TRUE) else make_empty_graph()
is_dag_now <- gorder(file_dag_g) > 0 && igraph::is_dag(file_dag_g)

cat("\n=== SCRIPT-LEVEL RUN ORDER ===\n")
if (is_dag_now) {
  run_order <- as_ids(topo_sort(file_dag_g, mode = "out"))
  print(run_order)
} else if (gorder(file_dag_g) > 0) {
  cat("Graph has cycles; strongly-connected groups (break at least one edge in each):\n")
  scc <- components(file_dag_g, mode = "strong")
  cyc_groups <- split(names(scc$membership), scc$membership)
  cyc_groups <- Filter(function(x) length(x) > 1, cyc_groups)
  print(cyc_groups)
} else {
  cat("(No dependencies detected.)\n")
}

# 5) Simple file I/O scan
file_grab <- function(path) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  reads  <- str_match_all(txt, "(?:read_|vroom|fread|load|readxl::read_|qs::qread)\\s*\\(\\s*['\"]([^'\"]+)['\"]")[[1]]
  writes <- str_match_all(txt, "(?:write_|saveRDS|save|fwrite|qs::qsave|qs::qwrite)\\s*\\(\\s*['\"]([^'\"]+)['\"]")[[1]]
  list(reads = if (nrow(reads)) reads[,2] else character(0),
       writes = if (nrow(writes)) writes[,2] else character(0))
}
file_io <- setNames(lapply(R_files, file_grab), R_files)
produced <- unique(unlist(lapply(file_io, `[[`, "writes")))
consumed <- unique(unlist(lapply(file_io, `[[`, "reads")))
orphans_produced <- setdiff(produced, consumed)
missing_inputs   <- setdiff(consumed, c(produced, fs::dir_exists(here("data")) %>% { if (.) fs::dir_ls(here("data"), recurse=TRUE) else character(0) }))

# 6) Save artifacts
fs::dir_create(here("data/derived/audit"), recurse = TRUE)
readr::write_csv(file_edges, here("data/derived/audit/script_edges.csv"))
if (is_dag_now) readr::write_csv(data.frame(order = run_order), here("data/derived/audit/run_order.csv"))
readr::write_lines(orphans_produced, here("data/derived/audit/orphan_outputs.txt"))
readr::write_lines(missing_inputs,   here("data/derived/audit/missing_inputs.txt"))

cat("\nAudit written to data/derived/audit/ .\n",
    if (file_cd_ok) "Used CodeDepends.\n" else "Used lite regex analysis.\n", sep = "")

############################################################################


# --- Cycle plan (robust; only prints when files actually written) ---
scc <- igraph::components(file_dag_g, mode = "strong")
groups <- split(names(scc$membership), scc$membership)
cycle_groups <- Filter(function(x) length(x) > 1, groups)

plan_dir <- here::here("data/derived/audit/cycles")
fs::dir_create(plan_dir, recursive = TRUE)

plans_written <- 0L

# Ensure per-file inputs/outputs are available (from CodeDepends or fallback regex)
if (!exists("file_outs") || !length(file_outs)) {
  if (exists("file_infos")) {
    file_outs <- lapply(file_infos, get_cd_out)
    file_ins  <- lapply(file_infos, get_cd_in)
  } else if (exists("file_io")) {
    file_outs <- lapply(file_io, `[[`, "outs")
    file_ins  <- lapply(file_io, `[[`, "ins")
  } else {
    file_outs <- setNames(vector("list", length(R_files)), R_files)
    file_ins  <- file_outs
  }
}

build_eg <- function(grp) {
  if ("var" %in% names(file_edges)) {
    dplyr::filter(file_edges, from %in% grp, to %in% grp)
  } else {
    pairs <- expand.grid(from = grp, to = grp, stringsAsFactors = FALSE)
    pairs <- pairs[pairs$from != pairs$to, , drop = FALSE]
    purrr::pmap_dfr(pairs, function(from, to) {
      v <- intersect(file_outs[[from]], file_ins[[to]])
      if (length(v)) tibble::tibble(from = from, to = to, var = v) else NULL
    })
  }
}

for (i in seq_along(cycle_groups)) {
  grp <- cycle_groups[[i]]
  eg  <- build_eg(grp)
  
  # Choose producer + variables
  if (!nrow(eg)) {
    # Fallback: pick file with the most top-level assignments, take up to 5 of them
    outs_counts  <- vapply(grp, function(f) length(file_outs[[f]]), integer(1))
    rec_producer <- names(sort(outs_counts, decreasing = TRUE))[1]
    top_vars     <- utils::head(file_outs[[rec_producer]], 5)
  } else {
    var_table <- eg |>
      dplyr::group_by(var) |>
      dplyr::summarise(
        producer  = dplyr::first(from),
        consumers = paste(unique(to), collapse = "; "),
        uses      = dplyr::n(),
        .groups   = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(uses))
    top_vars <- utils::head(var_table$var, 5)
    prod_scores  <- table(var_table$producer[var_table$var %in% top_vars])
    rec_producer <- names(sort(prod_scores, decreasing = TRUE))[1]
    if (!length(rec_producer) || is.na(rec_producer)) rec_producer <- grp[1]
  }
  
  if (length(top_vars)) {
    plans_written <- plans_written + 1L
    md <- c(
      sprintf("# Cycle group %02d", i),
      "## Scripts in this group:",
      paste0("- ", fs::path_rel(grp, here::here())),
      "",
      "## Recommended cut:",
      sprintf("- **Save in**: `%s`", fs::path_rel(rec_producer, here::here())),
      sprintf("- **Persist variables**: %s", paste(top_vars, collapse = ", ")),
      "",
      "### Save snippet (append to end of producer):",
      "```r",
      "dir.create(here::here(\"data/derived/checkpoints\"), showWarnings = FALSE, recursive = TRUE)",
      sprintf(
        "saveRDS(list(%s), here::here(\"data/derived/checkpoints\",\"scc%02d_checkpoint.rds\"))",
        paste(sprintf("%s=%s", top_vars, top_vars), collapse = ", "),
        i
      ),
      "```",
      "",
      "### Load snippet (near top of each consumer):",
      "```r",
      sprintf("ckp <- readRDS(here::here(\"data/derived/checkpoints\",\"scc%02d_checkpoint.rds\"))", i),
      paste0("list2env(ckp[intersect(names(ckp), c(", paste(sprintf('\"%s\"', top_vars), collapse = ", "), "))], envir = environment())"),
      "```",
      ""
    )
    out_md <- here::here(plan_dir, sprintf("scc%02d_plan.md", i))
    writeLines(md, out_md)
  }
}

if (plans_written > 0L) {
  cat("Wrote ", plans_written, " cycle plan(s) to ", fs::path_rel(plan_dir, here::here()), "\n", sep = "")
} else {
  cat("Found cycle groups but couldn't infer variables. Pick one file in a group and persist 3–5 top-level assignments at its end with saveRDS(); then re-run the audit.\n")
}









grp_ids <- split(names(scc$membership), scc$membership)
cycle_groups <- Filter(function(x) length(x) > 1, grp_ids)

cat("# Cycle groups:\n")
for (i in seq_along(cycle_groups)) {
  cat(sprintf("\n## Group %02d (%d files)\n", i, length(cycle_groups[[i]])))
  cat(paste0(" - ", fs::path_rel(cycle_groups[[i]], here::here())), sep = "\n")
}




dump_vars(c("g","g_l_s","g_wire", "g_sib", "net_l_s", "net_wire","net_sib"),
          here("data/derived/graphs", "main_graphs.rds"))

# baseline/null graphs
dump_vars(c("g_er","g_er_sub","swg","swg_sub","g_pa", "g_pa_sub"),       # add/remove keys to match yours
          here("data/derived/graphs", "baseline_graphs.rds"))


# ERGM fit (no recompute)
dump_vars(c("fit_wire2"),
          here("data/derived/models", "ergm_fit.rds"))

dump_vars(c("sims"),
          here("data/derived/models", "ergm_sims.rds"))


