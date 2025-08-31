repos <- c(CRAN = "https://packagemanager.posit.co/cran/latest")
options(repos = repos)
if (.Platform$OS.type == "windows") options(pkgType = "win.binary")

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

message("Restoring packages with renv…")
tryCatch({
  renv::restore(prompt = FALSE)
  message("✅ renv restore complete.")
}, error = function(e) {
  message("⚠️ renv restore failed: ", conditionMessage(e))
  if (.Platform$OS.type == "windows") {
    message("Tip: install Rtools for your R version (only if a package truly needs compiling):")
    message("https://cran.r-project.org/bin/windows/Rtools/")
  }
  stop(e)
})