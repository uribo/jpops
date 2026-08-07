fetch_estat_table <- function(stats_data_id, appid, ...) {
  estatapi::estat_getStatsData(
    appId = appid,
    statsDataId = stats_data_id,
    ...
  )
}

fetch_estat_meta <- function(stats_data_id, appid, ...) {
  estatapi::estat_getMetaInfo(
    appId = appid,
    statsDataId = stats_data_id,
    ...
  )
}

assert_cached_area_meta <- function(area_meta, file_loc) {
  required <- c(
    "area_code",
    "area_name",
    "area_level",
    "parent_area_code",
    "parent_area_code_raw",
    "parent_repaired",
    "repair_rule"
  )
  ok <- is.data.frame(area_meta) &&
    all(required %in% names(area_meta)) &&
    isTRUE(attr(area_meta, "jpops_area_meta_validated", exact = TRUE)) &&
    is.data.frame(attr(area_meta, "repair_log", exact = TRUE))
  if (!ok) {
    abort_area_meta(
      paste0(
        "The cached area metadata is not a validated area table: ",
        file_loc,
        ". Remove it or call `reset_cache()`."
      ),
      "jpops_area_meta_cache_error",
      file_loc = file_loc
    )
  }
  area_meta
}

collect_estat_area_meta <- function(stats_data_id, appid, cache = TRUE) {
  if (!cache) {
    assert_estat_meta_appid(appid)
    meta <- fetch_estat_meta(stats_data_id = stats_data_id, appid = appid)
    return(normalize_estat_area_meta(meta, stats_data_id = stats_data_id))
  }

  area_file <- jpops_estat_meta_cache_file(stats_data_id, "area")
  if (file.exists(area_file)) {
    return(assert_cached_area_meta(readRDS(area_file), area_file))
  }

  raw_file <- jpops_estat_meta_cache_file(stats_data_id, "raw")
  if (file.exists(raw_file)) {
    meta <- readRDS(raw_file)
  } else {
    assert_estat_meta_appid(appid)
    meta <- fetch_estat_meta(stats_data_id = stats_data_id, appid = appid)
    raw_file <- jpops_estat_meta_cache_file(
      stats_data_id,
      "raw",
      create = TRUE
    )
    saveRDS(meta, raw_file)
  }

  area_meta <- normalize_estat_area_meta(meta, stats_data_id = stats_data_id)
  area_file <- jpops_estat_meta_cache_file(
    stats_data_id,
    "area",
    create = TRUE
  )
  saveRDS(area_meta, area_file)
  area_meta
}

assert_estat_meta_appid <- function(appid) {
  missing <- is.null(appid) ||
    length(appid) != 1L ||
    is.na(appid) ||
    appid == ""
  if (!missing) {
    return(invisible(appid))
  }

  rlang::abort(
    paste0(
      "`appid` is required to fetch e-Stat area metadata. ",
      "`.area = \"municipality\"`, `\"ward\"`, and the deprecated ",
      "`\"city\"` alias need it on first use when no metadata cache exists; ",
      "with `cache = FALSE`, they always need it. `.area = \"all\"` and ",
      "`\"prefecture\"` do not use area metadata."
    ),
    class = "jpops_estat_meta_appid_error"
  )
}
