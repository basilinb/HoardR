#' Save a ggplot / patchwork object to one or more files
#'
#' When `overwrite = FALSE` (the default), any output file that already
#' exists on disk is skipped - [ggplot2::ggsave()] is not called for that path.
#' Set `overwrite = TRUE` to force re-rendering regardless.
#'
#' Supported extensions: `pdf`, `png`, `svg`, `eps`, `tiff`, `jpeg` / `jpg`.
#'
#' @param plot A ggplot or patchwork object.
#' @param paths Character vector. One or more output paths. Extension sets
#'   the device. Mix formats freely: `c("Figs/pca.pdf", "Figs/pca.png")`
#' @param width Numeric. Plot width.
#' @param height Numeric. Plot height.
#' @param units Character. `"in"` (default), `"cm"`, or `"mm"`.
#' @param dpi Numeric. Resolution for raster formats (default 300).
#' @param overwrite Logical. If FALSE, skip paths whose file already exists.
#' @param verbose Logical. Emit a message for each file saved or skipped.
#' @param ... Additional arguments forwarded to [ggplot2::ggsave()].
#'
#' @return Named list of output paths (invisibly), keyed by extension.
#'
#' @examples
#' \dontrun{
#' # Single format
#' save_figures(p_pca, make_path("P01", "pca", "pdf", "Figs"), width = 8, height = 6)
#'
#' # Multiple formats in one call
#' save_figures(
#'   p_pca,
#'   paths = make_path("P01", "pca", c("pdf", "png"), "Figs"),
#'   width = 8, height = 6
#' )
#' }
#'
#' @importFrom ggplot2 ggsave
#' @export
save_figures <- function(
    plot,
    paths,
    width = NA,
    height = NA,
    units = "in",
    dpi = 300,
    overwrite = FALSE,
    verbose = TRUE,
    ...) {
  if (is.null(plot)) {
    stop("`plot` is NULL.", call. = FALSE)
  }
  paths <- as.character(paths)

  exts <- .ext(paths)
  bad <- setdiff(exts, .figure_exts)
  if (length(bad)) {
    stop(
      "Unsupported figure extension(s): ",
      paste0(".", bad, collapse = ", "),
      "\nSupported: ",
      paste0(".", .figure_exts, collapse = ", "),
      call. = FALSE
    )
  }

  saved <- vector("list", length(paths))

  for (i in seq_along(paths)) {
    p <- paths[i]

    # ── Cache hit: skip ───────────────────────────────────────────────────────
    if (!overwrite && file.exists(p)) {
      if (verbose) {
        message("  Figure exists (skipping): ", p)
      }
      saved[[i]] <- p
      next
    }

    # ── Cache miss: render ────────────────────────────────────────────────────
    .ensure_dir(p)
    ggplot2::ggsave(
      filename = p,
      plot = plot,
      device = exts[i],
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      ...
    )
    if (verbose) {
      message("  Figure saved: ", p)
    }
    saved[[i]] <- p
  }

  invisible(stats::setNames(saved, exts))
}
