#' Obtain data for the Population Census
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' @param year year
#' @param appid application id
#' @param cache save to cache
#' @rdname jinkou
#' @export
get_jinkou <- function(year, appid, cache = TRUE) {
  if (cache) {
    cache_dir <- rappdirs::user_cache_dir("jpops")
    if (!file.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
    rds = paste0("jinkou_", year, ".rds")
    file_loc <- file.path(cache_dir, rds)
    if (file.exists(file_loc)) {
      out <- readRDS(file_loc)
    } else {
      out <-
        collect_jinkou_raw(year = year,
                           appid = appid)
      saveRDS(out, file = file_loc)
    }
    out
  } else {
    collect_jinkou_raw(year = year,
                       appid = appid)
  }
}

survey_year_dataid <- c(
  `2020` = "0003445078",
  `2015` = "0003149040",
  `2010` = "0003038587",
  `2005` = "0000033784")

select_jinkou_cols <- function(df) {
  cat01_code <- cat02_code <- area_code <- area <- gender <- NULL
  value <- NULL
  dplyr::select(df,
                cat01_code,
                gender = 4,
                area_code,
                area = 6,
                value)
}

collect_jinkou_raw <- function(year, appid) {
  cat01_code <- cat02_code <- area_code <- area <- gender <- NULL
  value <- NULL
  year <- as.character(year)
  year <- rlang::arg_match(year,
                           as.character(seq.int(2000, 2020, by = 5)))
  df_raw <-
    estatapi::estat_getStatsData(appId = appid,
                                 statsDataId = survey_year_dataid[year])
  if (year == "2020") {
    df_raw %>%
      select_jinkou_cols()
  } else if (year == "2015") {
    df_raw %>%
      dplyr::filter(cat01_code == "00710",
                    cat02_code %in% c("010", "020", "030")) %>%
      select_jinkou_cols() %>%
      dplyr::mutate(gender = conv_gender_vars(gender))
  } else if (year %in% c("2010", "2015")) {
    df_raw %>%
      dplyr::filter(cat01_code == "00710",
                    cat02_code %in% c("000", "001", "002")) %>%
      select_jinkou_cols() %>%
      dplyr::mutate(gender = conv_gender_vars(gender)) %>%
      dplyr::mutate(area = dplyr::if_else(
        area_code == "12229" & area == intToUtf8(c(34966, 12534, 28006, 24066)),
        intToUtf8(c(34966, 12465, 28006, 24066)),
        area))
  }
}
