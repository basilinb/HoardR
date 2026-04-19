#' Cache data AND save figures in one call
#'
#' On a cache miss the compute block is evaluated once. The block must return
#' a named list:
#' * `$data`: The object to cache (required).
#' * `$plot`: A single ggplot / patchwork saved to all `figure_paths`
#'   (when `figure_paths` is a plain character vector).
#' * `$plots`: A *named* list of plots (when `figure_paths` is a named list
#'   keyed by plot name).
#'
#' Figure caching follows [save_figures()] - existing figure files are
#' skipped when `overwrite = FALSE`.
#'
#' On a cache hit only the data is loaded. If you want to regenerate figures
#' on every run regardless, call [save_figures()] separately with
#' `overwrite = TRUE`.
#'
#' The compute block accepts both a bare `{ }` expression and a
#' `function()` / `\()` lambda.
#'
#' @param data_paths Character vector. Paths for the data cache.
#' @param figure_paths Character vector *or* named list.
#'   * Plain vector: `result$plot` saved to all paths.
#'   * Named list: each name matches a key in `result$plots`; value is the
#'     character vector of paths for that plot.
#' @param expr Unevaluated `{ block }` or `function()` returning
#'   `list(data = ..., plot = ...)`.
#' @param overwrite Logical. If TRUE recompute data and overwrite all files.
#'   Figures are also re-rendered when TRUE.
#' @param verbose Logical.
#' @param width,height,units,dpi Forwarded to [save_figures()].
#' @param csv_opts,csv_read_args Forwarded to internal read/write helpers.
#' @param ... Extra args forwarded to [ggplot2::ggsave()].
#'
#' @return The cached `$data` object (invisibly).
#'
#' @examples
#' \dontrun{
#' # Single plot
#' dat <- cache_with_figures(
#'   data_paths   = make_path("P01", "voom_clean", "rds", "DataClean"),
#'   figure_paths = make_path("P01", "pca", c("pdf", "png"), "Figs"),
#'   width = 8, height = 6,
#'   {
#'     obj <- build_voom(dat.raw)
#'     list(data = obj, plot = plot_pca(obj))
#'   }
#' )
#'
#' # Multiple plots (named list of figure_paths)
#' dat <- cache_with_figures(
#'   data_paths   = make_path("P01", "voom_clean", "rds", "DataClean"),
#'   figure_paths = list(
#'     pca     = make_path("P01", "pca", c("pdf", "png"), "Figs"),
#'     heatmap = make_path("P01", "heatmap", "pdf", "Figs")
#'   ),
#'   width = 8, height = 6,
#'   {
#'     obj <- build_voom(dat.raw)
#'     list(
#'       data  = obj,
#'       plots = list(pca = plot_pca(obj), heatmap = plot_heatmap(obj))
#'     )
#'   }
#' )
#' }
#'
#' @export
cache_with_figures <- function(
  data_paths,
  figure_paths = NULL,
  expr,
  overwrite = FALSE,
  verbose = TRUE,
  width = NA,
  height = NA,
  units = "in",
  dpi = 300,
  csv_opts = list(),
  csv_read_args = list(),
  ...
) {
  # ── Capture FIRST, before expr is touched ──────────────────────────────────
  expr_quo <- rlang::enquo(expr)

  data_paths <- as.character(data_paths)

  # ── Cache hit: load data and return early ───────────────────────────────────
  if (!overwrite) {
    exts <- .ext(data_paths)
    rds_hit <- data_paths[exts %in% .rds_exts & file.exists(data_paths)]
    any_hit <- data_paths[file.exists(data_paths)]
    load_from <- if (length(rds_hit)) {
      rds_hit[1L]
    } else if (length(any_hit)) {
      any_hit[1L]
    } else {
      NULL
    }
    if (!is.null(load_from)) {
      if (verbose) {
        message("Loading cached: ", load_from)
      }
      return(invisible(.read_one(load_from, csv_read_args)))
    }
  }

  # ── Cache miss: compute ─────────────────────────────────────────────────────
  if (verbose) {
    message("Cache miss - computing ...")
  }

  expr_inner <- rlang::quo_get_expr(expr_quo)
  result <- if (is.call(expr_inner) && expr_inner[[1]] == as.name("function")) {
    fn <- rlang::eval_tidy(expr_quo)
    fn()
  } else {
    rlang::eval_tidy(expr_quo)
  }

  # ── Validate result structure ───────────────────────────────────────────────
  if (!is.list(result) || !"data" %in% names(result)) {
    stop(
      "The expr block must return a named list with at least a `$data` element.\n",
      "  Bare block : list(data = my_obj, plot = my_plot)\n",
      "  Multi-plot : list(data = my_obj, plots = list(pca = p1, heatmap = p2))",
      call. = FALSE
    )
  }

  obj <- result$data

  # ── Save data ───────────────────────────────────────────────────────────────
  for (p in data_paths) {
    .write_one(obj, p, csv_opts)
  }

  # ── Save figures ────────────────────────────────────────────────────────────
  if (!is.null(figure_paths)) {
    if (is.list(figure_paths)) {
      if (!"plots" %in% names(result)) {
        stop(
          "figure_paths is a named list but the expr block returned no `$plots` element.\n",
          "  Return: list(data = ..., plots = list(pca = p1, heatmap = p2))",
          call. = FALSE
        )
      }
      for (nm in names(figure_paths)) {
        if (!nm %in% names(result$plots)) {
          stop(
            "Plot '",
            nm,
            "' not found in result$plots. ",
            "Available: ",
            paste(names(result$plots), collapse = ", "),
            call. = FALSE
          )
        }
        save_figures(
          result$plots[[nm]],
          paths = as.character(figure_paths[[nm]]),
          width = width,
          height = height,
          units = units,
          dpi = dpi,
          overwrite = overwrite,
          verbose = verbose,
          ...
        )
      }
    } else {
      plt <- result$plot %||% result$plots[[1L]]
      if (is.null(plt)) {
        stop(
          "figure_paths was supplied but the expr block returned neither ",
          "`$plot` nor `$plots`.\n",
          "  Return: list(data = ..., plot = my_plot)",
          call. = FALSE
        )
      }
      save_figures(
        plt,
        paths = as.character(figure_paths),
        width = width,
        height = height,
        units = units,
        dpi = dpi,
        overwrite = overwrite,
        verbose = verbose,
        ...
      )
    }
  }

  invisible(obj)
}
