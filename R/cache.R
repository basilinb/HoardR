#' Cache any R object
#'
#' Load if cached, otherwise compute and save. The extension of each path
#' determines the storage format:
#'
#' * `.rds`: Works for *any* R object - DGEList, EList, list-of-lists,
#'   model fits, data.frames, etc.
#' * `.csv` / `.tsv`: Flat data.frames / tibbles *only*. Passing a complex
#'   object errors immediately with a clear message.
#'
#' Multiple paths can be supplied; on a cache miss the object is written to
#' every format. On a cache hit the first existing file is read (preferring
#' `.rds` for round-trip fidelity).
#'
#' The compute block is only evaluated on a cache miss and can be supplied as:
#' * A bare `{ }` expression: `cache(path, { ... })`
#' * A lambda / anonymous function: `cache(path, function() { ... })` or
#'   `cache(path, \(){ ... })` (R >= 4.1)
#'
#' @param paths Character vector. One or more output paths.
#' @param expr Unevaluated `{ block }` or `function()` whose last expression
#'   is the object to cache.
#' @param overwrite Logical. If TRUE always recompute and overwrite all paths.
#' @param verbose Logical. Emit messages on load / save.
#' @param csv_opts List. Extra args forwarded to [readr::write_csv()] /
#'   [readr::write_tsv()].
#' @param csv_read_args List. Extra args forwarded to [readr::read_csv()] /
#'   [readr::read_tsv()].
#'
#' @return The cached or freshly-computed object (invisibly).
#'
#' @importFrom rlang enquo quo_get_expr eval_tidy
#' @examples
#' \dontrun{
#' # Cache a complex object
#' dat <- cache(
#'   make_path("P01", "voom_clean", "rds", "DataClean"),
#'   {
#'     # ... computation ...
#'     result
#'   }
#' )
#'
#' # Flat table to csv + rds simultaneously
#' results_tbl <- cache(
#'   make_path("P01", "results", c("csv", "rds"), "Results"),
#'   { compute_results(dat) }
#' )
#' }
#'
#' @export
cache <- function(
  paths,
  expr,
  overwrite = FALSE,
  verbose = TRUE,
  csv_opts = list(),
  csv_read_args = list()
) {
  # ── Capture FIRST, before expr is touched ──────────────────────────────────
  expr_quo <- rlang::enquo(expr)

  paths <- as.character(paths)
  if (!length(paths)) {
    stop("`paths` must contain at least one file path.", call. = FALSE)
  }
  exts <- .ext(paths)
  bad <- setdiff(exts, c(.rds_exts, .table_exts))
  if (length(bad)) {
    stop(
      "Unsupported extension(s) for cache(): ",
      paste0(".", bad, collapse = ", "),
      "\nFor figures use save_figures().",
      call. = FALSE
    )
  }

  # ── Load (cache hit) ────────────────────────────────────────────────────────
  if (!overwrite) {
    rds_hit <- paths[exts %in% .rds_exts & file.exists(paths)]
    any_hit <- paths[file.exists(paths)]
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

  # ── Compute (cache miss) ────────────────────────────────────────────────────
  if (verbose) {
    message("Cache miss - computing ...")
  }

  # Check the captured quosure expression, not `expr` itself
  expr_inner <- rlang::quo_get_expr(expr_quo)
  obj <- if (is.call(expr_inner) && expr_inner[[1]] == as.name("function")) {
    # Lambda / anonymous function: e.g. \() { ... } or function() { ... }
    fn <- rlang::eval_tidy(expr_quo)
    fn()
  } else {
    rlang::eval_tidy(expr_quo)
  }

  if (is.null(obj)) {
    warning(
      "cache() expression returned NULL - nothing will be saved.",
      call. = FALSE
    )
  }

  # ── Save to every requested path ────────────────────────────────────────────
  for (p in paths) {
    .write_one(obj, p, csv_opts)
  }
  invisible(obj)
}
