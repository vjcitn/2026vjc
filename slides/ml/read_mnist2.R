#' Download (once), cache, and read the MNIST dataset
#'
#' Drop-in replacement for dslabs::read_mnist(). Returns the same structure:
#' list(train = list(images, labels), test = list(images, labels)), where
#' `images` is an n x 784 integer matrix (one row per image, row-major 28x28,
#' values 0-255) and `labels` is an integer vector of digits 0-9.
#'
#' Files are fetched from the first working mirror, checked against known MD5
#' sums, and kept in a per-user cache directory so later calls read from disk.
#'
#' @param path Directory already holding the four *-ubyte.gz files. If given,
#'   nothing is downloaded and the cache is not used.
#' @param cache_dir Where to keep downloaded files. Defaults to
#'   option "mnist.cache_dir", then env var MNIST_CACHE_DIR, then
#'   tools::R_user_dir("mnist", "cache"). Use NULL for no persistent cache
#'   (files go to a temporary directory that is removed afterwards).
#' @param mirrors Base URLs (with trailing slash) tried in order.
#' @param verify Check MD5 sums of cached and downloaded files.
#' @param refresh Ignore any cached copies and download again.
#' @param quiet Suppress progress messages.
read_mnist2 <- function(path = NULL,
                       cache_dir = getOption("mnist.cache_dir",
                                             Sys.getenv("MNIST_CACHE_DIR",
                                                        tools::R_user_dir("mnist", "cache"))),
                       mirrors = c("https://ossci-datasets.s3.amazonaws.com/mnist/",
                                   "https://storage.googleapis.com/cvdf-datasets/mnist/"),
                       verify = TRUE,
                       refresh = FALSE,
                       quiet = FALSE) {

  md5 <- c("train-images-idx3-ubyte.gz" = "f68b3c2dcbeaaa9fbdd348bbdeb94873",
           "train-labels-idx1-ubyte.gz" = "d53e105ee54ea40749a09fcbcd1e9432",
           "t10k-images-idx3-ubyte.gz"  = "9fb629c4189551a2d022fa330f9573f3",
           "t10k-labels-idx1-ubyte.gz"  = "ec29112dd5afa0611ce80d1b7f02629c")
  files <- names(md5)
  say <- function(...) if (!quiet) message(...)
  md5_ok <- function(f, fn = basename(f)) !verify || unname(tools::md5sum(f)) == md5[[fn]]

  ## ---- locate or fetch the four files ------------------------------------
  if (!is.null(path)) {
    dir <- path.expand(path)
    missing <- files[!file.exists(file.path(dir, files))]
    if (length(missing))
      stop("Not found in ", dir, ": ", paste(missing, collapse = ", "), call. = FALSE)
  } else {
    if (is.null(cache_dir) || !nzchar(cache_dir)) {
      dir <- tempfile("mnist_")
      on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    } else {
      dir <- path.expand(cache_dir)
    }
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)

    old_timeout <- options(timeout = max(600, getOption("timeout")))
    on.exit(options(old_timeout), add = TRUE)

    fetched <- FALSE
    for (fn in files) {
      dest <- file.path(dir, fn)
      if (!refresh && file.exists(dest)) {
        if (md5_ok(dest)) next
        say("Cached ", fn, " failed MD5 check; downloading again.")
      }
      got <- FALSE
      for (base in mirrors) {
        tmp <- tempfile(fileext = ".gz", tmpdir = dir)   # same filesystem -> atomic rename
        ok <- tryCatch({
          say("Downloading ", fn, " from ", base)
          download.file(paste0(base, fn), tmp, mode = "wb", quiet = quiet) == 0
        }, error = function(e) { say("  failed: ", conditionMessage(e)); FALSE },
           warning = function(w) { say("  failed: ", conditionMessage(w)); FALSE })
        if (ok && md5_ok(tmp, fn)) {
          file.rename(tmp, dest)
          got <- fetched <- TRUE
          break
        }
        if (ok) say("  MD5 mismatch for ", fn, " from ", base)
        unlink(tmp)
      }
      if (!got)
        stop("Could not obtain ", fn, " from any mirror:\n  ",
             paste(mirrors, collapse = "\n  "), call. = FALSE)
    }
    if (fetched && !is.null(cache_dir) && nzchar(cache_dir)) say("MNIST files cached in ", dir)
  }

  ## ---- IDX readers ---------------------------------------------------------
  read_idx <- function(f, expect_magic) {
    con <- gzfile(f, "rb")
    on.exit(close(con))
    magic <- readBin(con, "integer", n = 1, size = 4, endian = "big")
    if (magic != expect_magic)
      stop(basename(f), ": bad magic number ", magic, " (expected ", expect_magic, ")",
           call. = FALSE)
    ndim <- bitwAnd(magic, 255L)
    dims <- readBin(con, "integer", n = ndim, size = 4, endian = "big")
    n <- prod(dims)
    x <- readBin(con, "raw", n = n)
    if (length(x) != n)
      stop(basename(f), ": truncated (", length(x), " of ", n, " bytes)", call. = FALSE)
    list(dims = dims, data = as.integer(x))
  }
  read_images <- function(f) {
    r <- read_idx(f, 2051L)
    matrix(r$data, nrow = r$dims[1], ncol = prod(r$dims[-1]), byrow = TRUE)
  }
  read_labels <- function(f) read_idx(f, 2049L)$data

  list(train = list(images = read_images(file.path(dir, "train-images-idx3-ubyte.gz")),
                    labels = read_labels(file.path(dir, "train-labels-idx1-ubyte.gz"))),
       test  = list(images = read_images(file.path(dir, "t10k-images-idx3-ubyte.gz")),
                    labels = read_labels(file.path(dir, "t10k-labels-idx1-ubyte.gz"))))
}

