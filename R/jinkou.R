#' Obtain data for the Population Census
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' @param year year
#' @param appid e-Stat application ID. Area filtering for `"all"` and
#'   `"prefecture"` never needs an application ID or area metadata. Fetching
#'   uncached population data from e-Stat still needs an application ID.
#'   In addition, `"municipality"`, `"ward"`, and the deprecated `"city"`
#'   alias need one on first use if no area metadata cache exists for the
#'   selected statistical table. Once that metadata cache exists, it can be
#'   reused with `appid = NULL` when the population data is also available
#'   locally. With `cache = FALSE`, these three values always need an
#'   application ID.
#' @param cache Whether to read and write package caches.
#' @param .area Area detail to return. The canonical values are `"all"`,
#'   `"prefecture"`, `"municipality"`, and `"ward"`.
#'
#'   `"all"` contains overlapping parent and child areas and must not be
#'   summed. `"prefecture"` returns the 47 prefectures. `"municipality"`
#'   returns ordinary cities, designated cities as single units, Tokyo's
#'   special-ward area as one unit, and towns and villages. It excludes the 23
#'   individual Tokyo special wards, wards of designated cities, and historical
#'   municipalities based on year-2000 boundaries. It is therefore coarser than
#'   the everyday sense of municipalities in Tokyo. `"ward"` uses the same
#'   frontier but splits designated cities into wards and Tokyo's special-ward
#'   area into the 23 individual special wards.
#'
#'   `"city"` is deprecated for one release and is an alias for
#'   `"municipality"`. Its meaning changed: it formerly returned overlapping
#'   parent and child areas, so results and totals will change.
#' @rdname jinkou
#' @export
get_jinkou <- function(year, appid = NULL, cache = TRUE, .area = "all") {
  year <- match_survey_year(year, "total")
  .area <- match_jinkou_area(.area)
  if (cache) {
    file_loc <- jpops_processed_cache_file(year, "total", create = TRUE)
    if (file.exists(file_loc)) {
      out <- readRDS(file_loc)
    } else {
      out <-
        collect_jinkou_raw(year = year, appid = appid)
      saveRDS(out, file = file_loc)
    }
  } else {
    out <-
      collect_jinkou_raw(year = year, appid = appid)
  }
  filter_jinkou_area(out, year, "total", appid, cache, .area)
}

#' @rdname jinkou
#' @export
get_jinkou_age <- function(year, appid = NULL, cache = TRUE, .area = "all") {
  year <- match_survey_year(year, "age")
  .area <- match_jinkou_area(.area)
  if (cache) {
    file_loc <- jpops_processed_cache_file(year, "age", create = TRUE)
    if (file.exists(file_loc)) {
      out <-
        readRDS(file_loc)
    } else {
      out <-
        collect_jinkou_age_raw(year, appid, cache = TRUE)
      saveRDS(out, file_loc)
    }
  } else {
    out <-
      collect_jinkou_age_raw(year, appid, cache = FALSE)
  }
  filter_jinkou_area(out, year, "age", appid, cache, .area)
}

filter_jinkou_area <- function(df, year, table_kind, appid, cache, area) {
  if (!area %in% c("municipality", "ward")) {
    return(area_filter(df, area))
  }

  stats_data_id <- unname(survey_year_dataid[[table_kind]][[year]])
  area_meta <- collect_estat_area_meta(stats_data_id, appid, cache)
  area_filter(df, area, area_meta)
}

survey_year_dataid <- list(
  `total` = c(
    `2020` = "0003445078",
    `2015` = "0003149040",
    `2010` = "0003038587",
    `2005` = "0000033784"
  ),
  `age` = c(
    # 2-5-1
    `2020` = "0003445139",
    # 00310 gun-kei
    # 00320
    `2015` = "0003148521",
    # 00320
    `2010` = "0003041389",
    # 00401 0000033697 gun-kei
    # todouhuken
    `2005` = "0000033787"
  )
)

match_survey_year <- function(year, table_kind) {
  year <- as.character(year)
  rlang::arg_match(year, names(survey_year_dataid[[table_kind]]))
}

select_jinkou_cols <- function(df) {
  area_code <- area <- gender <- NULL
  value <- NULL
  dplyr::select(df, gender = 4, area_code, area = 6, value)
}

select_jinkou_age_output <- function(df) {
  area_code <- area <- gender <- age <- value <- NULL
  dplyr::select(df, gender, area_code, area, age, value)
}

collect_jinkou_raw <- function(year, appid) {
  cat01_code <- cat02_code <- area_code <- area <- gender <- NULL
  value <- NULL
  year <- match_survey_year(year, "total")
  df_raw <- fetch_estat_table(
    stats_data_id = survey_year_dataid$total[year],
    appid = appid
  )
  if (year == "2020") {
    df_raw |>
      select_jinkou_cols()
  } else if (year == "2015") {
    df_raw |>
      dplyr::filter(
        cat01_code == "00710",
        cat02_code %in% c("010", "020", "030")
      ) |>
      select_jinkou_cols() |>
      dplyr::mutate(gender = conv_gender_vars(gender))
  } else if (year == "2010") {
    df_raw |>
      dplyr::filter(
        cat01_code == "00710",
        cat02_code %in% c("000", "001", "002")
      ) |>
      select_jinkou_cols() |>
      dplyr::mutate(gender = conv_gender_vars(gender)) |>
      dplyr::mutate(
        area = dplyr::if_else(
          area_code == "12229" &
            area == JPOPS_AREA_SODEGAURA_SMALL_KE,
          JPOPS_AREA_SODEGAURA_KE,
          area
        )
      )
  } else if (year == "2005") {
    df_raw |>
      dplyr::filter(cat01_code == "00700") |>
      select_jinkou_cols() |>
      dplyr::mutate(gender = conv_gender_vars(gender))
  } else {
    rlang::abort("No formatter is available for the selected survey year.")
  }
}

collect_jinkou_age_raw <- function(year, appid, cache = TRUE) {
  tab_code <- cat01_code <- cat02_code <- cat03_code <- cat04_code <- NULL
  unit <- gender <- age <- NULL
  year <- match_survey_year(year, "age")
  cache_dir <- jpops_cache_dir(create = cache)
  file_loc <- file.path(cache_dir, paste0("jinkou_age_", year, "_raw.rds"))
  if (cache && file.exists(file_loc)) {
    df_raw <-
      readRDS(file_loc)
  } else {
    df_raw <- fetch_estat_table(
      stats_data_id = survey_year_dataid$age[year],
      appid = appid
    )
    if (cache) {
      saveRDS(df_raw, file_loc)
    }
  }
  if (year == "2020") {
    df_raw |>
      dplyr::filter(cat01_code == "0") |>
      dplyr::select(5:10, 14) |>
      dplyr::rename(gender = 2, age = 4, area = 6) |>
      dplyr::mutate(age = conv_age_vars(age)) |>
      select_jinkou_age_output()
  } else if (year == "2015") {
    df_raw |>
      dplyr::filter(
        cat01_code == "00710",
        cat04_code == "0000",
        unit == JPOPS_UNIT_PERSON
      ) |>
      dplyr::select(5:8, 11:12, 16) |>
      dplyr::rename(gender = 4, age = 2, area = 6) |>
      dplyr::mutate(
        gender = conv_gender_vars(gender),
        age = conv_age_vars(age)
      ) |>
      select_jinkou_age_output()
  } else if (year == "2010") {
    df_raw |>
      dplyr::filter(
        cat01_code == "00710",
        unit == JPOPS_UNIT_PERSON,
        cat03_code == "000"
      ) |>
      dplyr::select(5:6, 9:10, 11:12, 16) |>
      dplyr::rename(gender = 2, age = 4, area = 6) |>
      dplyr::mutate(
        gender = conv_gender_vars(gender),
        age = conv_age_vars(age)
      ) |>
      select_jinkou_age_output()
  } else if (year == "2005") {
    df_raw |>
      dplyr::filter(cat01_code == "00700") |>
      dplyr::select(3:8, 12) |>
      dplyr::rename(gender = 4, age = 2, area = 6) |>
      dplyr::mutate(
        gender = conv_gender_vars(gender),
        age = conv_age_vars(age)
      ) |>
      select_jinkou_age_output()
  } else {
    rlang::abort("No formatter is available for the selected survey year.")
  }
}
