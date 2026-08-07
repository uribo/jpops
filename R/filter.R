area_filter <- function(df, area, area_meta = NULL) {
  area_code <- NULL
  area <- match_jinkou_area(area)
  if (area == "all") {
    return(df)
  }

  prefecture_codes <- sprintf("%02d000", seq_len(47L))
  if (area == "prefecture") {
    return(dplyr::filter(df, area_code %in% prefecture_codes))
  }

  if (is.null(area_meta)) {
    rlang::abort(
      paste0(
        "`area_meta` is required when `area` is \"",
        area,
        "\"."
      ),
      class = "jpops_area_meta_required_error"
    )
  }
  assert_population_area_codes(df$area_code, area_meta)
  selected_codes <- classify_area_codes(area_meta, area)
  dplyr::filter(df, area_code %in% selected_codes)
}

match_jinkou_area <- function(area) {
  area <- rlang::arg_match(
    area,
    c("all", "prefecture", "municipality", "ward", "city")
  )
  if (area == "city") {
    lifecycle::deprecate_warn(
      "0.0.0.9000",
      I("`.area = \"city\"`"),
      I("`.area = \"municipality\"`"),
      details = paste0(
        "The meaning changed: `.area = \"city\"` previously returned ",
        "overlapping parent and child areas; it now returns the ",
        "non-overlapping `\"municipality\"` frontier. Results and totals ",
        "will change. Use `.area = \"all\"` and filter explicitly only ",
        "if you need the former result set."
      ),
      # Not a rename: every call silently returns a different result set, so
      # the warning must not be throttled away after the first one.
      always = TRUE
    )
    area <- "municipality"
  }
  area
}

assert_population_area_codes <- function(area_codes, area_meta) {
  missing <- unique(area_codes[
    is.na(area_codes) | !area_codes %in% area_meta$area_code
  ])
  if (length(missing) == 0L) {
    return(invisible(area_meta))
  }

  labels <- ifelse(is.na(missing), "<NA>", missing)
  abort_area_meta(
    paste0(
      "Population area codes are missing from metadata: ",
      paste(labels, collapse = ", "),
      "."
    ),
    "jpops_area_meta_population_code_error",
    area_codes = missing
  )
}
