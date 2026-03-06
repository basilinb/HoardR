# Tests for HoardR caching functions

# ── make_path() ───────────────────────────────────────────────────────────────

test_that("make_path creates single path correctly", {
  tmp_dir <- tempdir()
  path <- make_path("P01", "analysis", "rds", tmp_dir)

  expect_true(grepl("P01_analysis\\.rds$", path))
  expect_true(grepl(basename(tmp_dir), path))
})

test_that("make_path creates multiple paths with named vector", {
  tmp_dir <- tempdir()
  paths <- make_path("P01", "results", c("csv", "rds"), tmp_dir)

  expect_length(paths, 2)
  expect_named(paths, c("csv", "rds"))
  expect_true(grepl("\\.csv$", paths["csv"]))
  expect_true(grepl("\\.rds$", paths["rds"]))
})

test_that("make_path creates directory if it doesn't exist", {
  tmp_dir <- file.path(tempdir(), paste0("test_dir_", Sys.getpid()))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  path <- make_path("P01", "test", "rds", tmp_dir)

  expect_true(dir.exists(tmp_dir))
})

# ── cache() ───────────────────────────────────────────────────────────────────

test_that("cache saves and loads RDS correctly", {
  tmp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp_file), add = TRUE)

  # First call: compute and save
  result1 <- cache(
    tmp_file,
    {
      list(a = 1, b = 2)
    },
    verbose = FALSE
  )

  expect_true(file.exists(tmp_file))
  expect_equal(result1, list(a = 1, b = 2))

  # Second call: load from cache
  result2 <- cache(
    tmp_file,
    {
      stop("Should not run")
    },
    verbose = FALSE
  )

  expect_equal(result2, list(a = 1, b = 2))
})

test_that("cache saves and loads CSV correctly", {
  tmp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_file), add = TRUE)

  df <- data.frame(x = 1:3, y = c("a", "b", "c"))

  result1 <- cache(
    tmp_file,
    {
      df
    },
    verbose = FALSE
  )

  expect_true(file.exists(tmp_file))

  result2 <- cache(
    tmp_file,
    {
      stop("Should not run")
    },
    verbose = FALSE
  )

  expect_equal(result2$x, df$x)
  expect_equal(result2$y, df$y)
})

test_that("cache errors when saving non-flat object to CSV", {
  tmp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_file), add = TRUE)

  expect_error(
    cache(
      tmp_file,
      {
        list(a = 1, b = list(c = 2))
      },
      verbose = FALSE
    ),
    "Cannot save"
  )
})

test_that("cache supports lambda syntax", {
  tmp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp_file), add = TRUE)

  result <- cache(
    tmp_file,
    function() {
      42
    },
    verbose = FALSE
  )

  expect_equal(result, 42)
})

test_that("cache overwrites when overwrite = TRUE", {
  tmp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp_file), add = TRUE)

  cache(
    tmp_file,
    {
      "first"
    },
    verbose = FALSE
  )
  result <- cache(
    tmp_file,
    {
      "second"
    },
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_equal(result, "second")
})

test_that("cache saves to multiple formats", {
  tmp_csv <- tempfile(fileext = ".csv")
  tmp_rds <- tempfile(fileext = ".rds")
  on.exit(unlink(c(tmp_csv, tmp_rds)), add = TRUE)

  df <- data.frame(x = 1:3)
  result <- cache(
    c(tmp_csv, tmp_rds),
    {
      df
    },
    verbose = FALSE
  )

  expect_true(file.exists(tmp_csv))
  expect_true(file.exists(tmp_rds))
})

test_that("cache errors on unsupported extension", {
  tmp_file <- tempfile(fileext = ".pdf")

  expect_error(
    cache(
      tmp_file,
      {
        1
      },
      verbose = FALSE
    ),
    "Unsupported extension"
  )
})

# ── safe_filename() ───────────────────────────────────────────────────────────

test_that("safe_filename creates path in correct format", {
  tmp_dir <- tempdir()
  path <- safe_filename(tmp_dir, "test", "rds")

  expect_true(grepl("test\\.rds$", path))
})

test_that("safe_filename handles existing file with increment", {
  tmp_dir <- file.path(tempdir(), paste0("safe_test_", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create initial file
  file.create(file.path(tmp_dir, "test.rds"))

  path <- safe_filename(
    tmp_dir,
    "test",
    "rds",
    overwrite = FALSE,
    version = "increment"
  )

  expect_true(grepl("test_v1\\.rds$", path))
})

test_that("safe_filename returns same path when overwrite = TRUE", {
  tmp_dir <- file.path(tempdir(), paste0("safe_test2_", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(tmp_dir, "test.rds"))

  path <- safe_filename(tmp_dir, "test", "rds", overwrite = TRUE)

  expect_true(grepl("test\\.rds$", path))
  expect_false(grepl("_v\\d", path))
})
