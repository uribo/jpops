#' Remove cache files
#'
#' @description Remove all processed and raw package cache files.
#' @rdname reset_cache
#' @export
reset_cache <- function() {
  unlink(jpops_cache_dir(), recursive = TRUE)
}

jpops_cache_dir <- function(create = FALSE) {
  cache_dir <- rappdirs::user_cache_dir("jpops")
  if (create && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  cache_dir
}
