#' Build a collision-safe file path
#'
#' Creates the parent directory if needed and returns a path that will not
#' silently overwrite an existing file (unless `overwrite = TRUE`).
#'
#' @param dir Character. Target directory.
#' @param basename Character. Base name; any extension is stripped and
#'   replaced by `ext`.
#' @param ext Character. Extension without leading dot (e.g. `"rds"`).
#' @param overwrite Logical. If TRUE return the plain path even if it exists.
#' @param version Character. Conflict strategy:
#'   * `"none"`: Append a timestamp only when a conflict is detected.
#'   * `"timestamp"`: Always append a timestamp.
#'   * `"increment"`: Append `_v1`, `_v2`, ... on each conflict.
#'
#' @return Character. Normalised path (the file is NOT created by this function).
#'
#' @export
safe_filename <- function(
    dir,
    basename,
    ext,
    overwrite = FALSE,
    version = c("none", "timestamp", "increment")) {
  version <- match.arg(version)
  dir <- as.character(dir)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  base <- sub("\\.[A-Za-z0-9]+$", "", as.character(basename))
  path <- file.path(dir, paste0(base, ".", ext))

  if (!overwrite && file.exists(path)) {
    if (version == "none") {
      path <- file.path(
        dir,
        paste0(base, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
      )
    } else if (version == "timestamp") {
      path <- file.path(
        dir,
        paste0(base, "_", format(Sys.time(), "%Y%m%dT%H%M%S"), ".", ext)
      )
    } else {
      # increment
      esc <- gsub("([.\\^$*+?()[{\\\\|])", "\\\\\\1", base)
      pat <- paste0("^", esc, "_v(\\d+)\\.", ext, "$")
      hits <- list.files(dir, pattern = pat)
      nv <- if (length(hits)) {
        max(
          as.integer(
            sub(paste0("^", esc, "_v(\\d+)\\.", ext, "$"), "\\1", hits)
          ),
          na.rm = TRUE
        ) + 1L
      } else {
        1L
      }
      path <- file.path(dir, paste0(base, "_v", nv, ".", ext))
    }
  }

  normalizePath(path, winslash = "/", mustWork = FALSE)
}
