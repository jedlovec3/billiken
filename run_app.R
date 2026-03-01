lib <- normalizePath("r-packages", mustWork = FALSE)
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))

# Write Makevars with zlib paths using pkg-config so compiled packages find headers
makevars_path <- file.path(lib, ".Makevars")
tryCatch({
  zlib_cflags <- system2("pkg-config", c("--cflags", "zlib"), stdout = TRUE, stderr = FALSE)
  zlib_libs   <- system2("pkg-config", c("--libs-only-L", "zlib"), stdout = TRUE, stderr = FALSE)
  if (length(zlib_cflags) > 0) {
    writeLines(c(
      paste("CPPFLAGS +=", zlib_cflags),
      paste("LDFLAGS +=",  zlib_libs)
    ), makevars_path)
    Sys.setenv(R_MAKEVARS_USER = makevars_path)
  }
}, error = function(e) NULL)

# Remove any stale lock directories from interrupted prior installs
locks <- list.dirs(lib, recursive = FALSE, full.names = TRUE)
locks <- locks[grepl("00LOCK", basename(locks))]
if (length(locks) > 0) {
  cat("Removing stale lock directories:", paste(basename(locks), collapse = ", "), "\n")
  unlink(locks, recursive = TRUE)
}

required_pkgs <- c("shiny", "DT")
missing_pkgs  <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  cat("Installing missing packages:", paste(missing_pkgs, collapse = ", "), "\n")
  options(install.lock = FALSE)
  install.packages(
    missing_pkgs,
    lib   = lib,
    repos = "https://packagemanager.posit.co/cran/__linux__/noble/latest",
    Ncpus = 1
  )
  cat("Package installation complete.\n")
}

library(shiny)

shiny::runApp(
  appDir         = "TradeScenarios",
  host           = "0.0.0.0",
  port           = 5000,
  launch.browser = FALSE
)
