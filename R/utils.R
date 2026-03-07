# =============================================================================
# utils.R
#
# Internal constants and helper functions for HoardR
# =============================================================================

# ── internal constants ────────────────────────────────────────────────────────

.figure_exts <- c("pdf", "png", "svg", "eps", "tiff", "jpeg", "jpg")
.table_exts <- c("csv", "tsv")
.rds_exts <- "rds"
.all_exts <- c(.rds_exts, .table_exts, .figure_exts)


# ── internal helper functions ─────────────────────────────────────────────────

.ext <- function(path) tolower(tools::file_ext(path))

.ensure_dir <- function(path) {
  d <- dirname(path)
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(d)
}

.is_flat <- function(x) is.data.frame(x) || isTRUE(inherits(x, "tbl"))

`%||%` <- function(a, b) if (!is.null(a)) a else b


# ── internal read / write ─────────────────────────────────────────────────────

.read_one <- function(path, csv_read_args = list()) {
  ext <- .ext(path)
  switch(
    ext,
    rds = readRDS(path),
    csv = do.call(
      readr::read_csv,
      c(list(file = path, show_col_types = FALSE), csv_read_args)
    ),
    tsv = do.call(
      readr::read_tsv,
      c(list(file = path, show_col_types = FALSE), csv_read_args)
    ),
    stop("Cannot read format '.", ext, "'. Supported: rds, csv, tsv.", call. = FALSE)
  )
}

.write_one <- function(obj, path, csv_opts = list()) {
  ext <- .ext(path)
  .ensure_dir(path)
  
  # 1. Create a hidden temp path in the SAME directory
  tmp_path <- tempfile(
    pattern = paste0(".tmp_", basename(path), "_"), 
    tmpdir  = dirname(path)
  )

  # 2. Use tryCatch to ensure we clean up the temp file if things crash
  tryCatch({
    switch(
      ext,
      rds = saveRDS(obj, file = tmp_path),
      csv = {
        if (!.is_flat(obj)) {
          stop("Cannot save a <", class(obj)[1L], "> to CSV.", call. = FALSE)
        }
        do.call(readr::write_csv, c(list(x = obj, file = tmp_path), csv_opts))
      },
      tsv = {
        if (!.is_flat(obj)) {
          stop("Cannot save a <", class(obj)[1L], "> to TSV.", call. = FALSE)
        }
        do.call(readr::write_tsv, c(list(x = obj, file = tmp_path), csv_opts))
      },
      stop("Unsupported format: .", ext, call. = FALSE)
    )

    # 3. Final Step: Atomic Swap
    # If the code gets here, the file is fully written.
    file.rename(tmp_path, path)
    message(" Saved : ", path)
    
  }, error = function(e) {
    # Clean up the partial/broken temp file if it exists
    if (file.exists(tmp_path)) unlink(tmp_path)
    stop(e$message, call. = FALSE)
  })
}

