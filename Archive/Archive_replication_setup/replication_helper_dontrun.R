tokens <- c("g_l_s","g_wire", "g_sib", "net_l_s", "net_wire","net_sib", 
            "g_er","g_er_sub","swg","swg_sub","g_pa", "g_pa_sub",
            "fit_wire2", "sims")
files  <- fs::dir_ls(here(), glob = "**/*.R", recurse = TRUE)
files  <- files[!grepl("^(renv/|\\.git/)", fs::path_rel(files, here()))]

needs <- lapply(files, function(f){
  txt <- readLines(f, warn = FALSE)
  found <- tokens[vapply(tokens, function(t) any(grepl(paste0("\\b",t,"\\b"), txt)), logical(1))]
  has_ensure <- any(grepl("R/ensure\\.R", txt)) && any(grepl("ensure_", txt))
  list(found = found, has_ensure = has_ensure)
})
for (i in seq_along(files)) {
  if (length(needs[[i]]$found) > 0 && !needs[[i]]$has_ensure) {
    cat(fs::path_rel(files[i], here()), ": ",
        paste(needs[[i]]$found, collapse = ", "), "\n", sep = "")
  }
}
