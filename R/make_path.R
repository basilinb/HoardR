#' Build a consistent file path
#'
#' The single source of truth for file naming across your project. Always
#' produces paths of the form `dir/project_analysis.ext`.
#'
#' When `ext` is a vector, one path is returned per extension, named by
#' extension. This makes it easy to pass multiple formats to [cache()],
#' [save_figures()], or [cache_with_figures()] in one call.
#'
#' @param project Character. Project identifier (e.g. `"P01"` or
#'   `projectNumber_swab`).
#' @param analysis Character. Descriptive analysis label
#'   (e.g. `"voom_clean"`, `"pca"`).
#' @param ext Character. One or more extensions without a leading dot
#'   (e.g. `"rds"`, `c("csv","rds")`, `c("pdf","png")`).
#' @param dir Character. Target directory. Created if it does not exist.
#'
#' @return A named character vector of normalised paths, one per extension.
#'   If only one extension is supplied the vector is unnamed.
#'
#' @examples
#' \dontrun{
#' # Single path
#' make_path("P01", "voom_clean", "rds", "DataClean")
#'
#' # Multiple extensions at once
#' make_path("P01", "results", c("csv", "rds"), "Results")
#' }
#'
#' @export
make_path <- function(project, analysis, ext, dir) {
  project <- as.character(project)
  analysis <- as.character(analysis)
  dir <- as.character(dir)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  base <- paste0(project, "_", analysis)
  paths <- normalizePath(
    file.path(dir, paste0(base, ".", ext)),
    winslash = "/",
    mustWork = FALSE
  )

  # Name by extension when multiple are given; leave unnamed for a single ext
  if (length(ext) > 1L) stats::setNames(paths, ext) else paths
}
