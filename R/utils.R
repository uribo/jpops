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

jpops_processed_cache_file <- function(year, table_kind, create = FALSE) {
  table_kind <- rlang::arg_match(table_kind, c("total", "age"))
  cache_version <- c(total = 1L, age = 4L)[[table_kind]]
  prefix <- if (table_kind == "total") "jinkou_" else "jinkou_age_"
  file_name <- paste0(prefix, year, "_v", cache_version, ".rds")

  file.path(jpops_cache_dir(create = create), file_name)
}

jpops_estat_meta_cache_file <- function(
  stats_data_id,
  stage = c("raw", "area"),
  create = FALSE
) {
  stage <- rlang::arg_match(stage)
  area_cache_version <- 1L
  file_name <- if (stage == "raw") {
    paste0("estat_meta_", stats_data_id, "_raw.rds")
  } else {
    paste0(
      "estat_area_meta_",
      stats_data_id,
      "_v",
      area_cache_version,
      ".rds"
    )
  }

  file.path(jpops_cache_dir(create = create), file_name)
}
