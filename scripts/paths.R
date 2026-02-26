# scripts/paths.R
# Helpers for finding the Billiken project root and constructing paths.

#' Find the project root directory by walking up from a starting directory.
#'
#' We treat the directory containing `billiken.Rproj` as the root.
#' This avoids fragile relative paths like "../data/..." when scripts are run
#' from different working directories.
#'
#' @param start_dir Directory to start searching from (default: getwd()).
#' @param max_up Maximum number of parent directories to search.
#' @return Absolute normalized path to the project root.
find_project_root <- function(start_dir = getwd(), max_up = 10) {
  cur <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)

  for (i in 0:max_up) {
    marker <- file.path(cur, "billiken.Rproj")
    if (file.exists(marker)) {
      return(cur)
    }

    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }

  stop(
    sprintf(
      "Could not find project root (billiken.Rproj) starting from: %s",
      normalizePath(start_dir, winslash = "/", mustWork = FALSE)
    ),
    call. = FALSE
  )
}

#' Build a path relative to project root.
#' @param ... path components under the repo root.
#' @return Absolute path.
root_path <- function(...) {
  file.path(find_project_root(), ...)
}
