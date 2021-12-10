#' Remove cache files
#'
#' @description Remove all package cache files.
#' @rdname reset_cache
#' @export
reset_cache <- function() {
  unlink(rappdirs::user_cache_dir("jpops"),
         recursive = TRUE)
}
