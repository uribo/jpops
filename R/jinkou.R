#' Obtain data for the Population Census
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' @param year year
#' @param appid application id
#' @param cache save to cache
#' @rdname jinkou
#' @export
get_jinkou <- function(year, appid = NULL, cache = TRUE) {
  if (cache) {
    cache_dir <- rappdirs::user_cache_dir("jpops")
    if (!file.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
    file_loc <- file.path(cache_dir,
                          paste0("jinkou_", year, ".rds"))
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

#' @rdname jinkou
#' @export
get_jinkou_age <- function(year, appid = NULL, cache = TRUE) {
  if (cache) {
    cache_dir <- rappdirs::user_cache_dir("jpops")
    file_loc <- file.path(cache_dir,
                          paste0("jinkou_age_", year, ".rds"))
    if (file.exists(file_loc)) {
      out <-
        readRDS(file_loc)
    } else {
      out <-
        collect_jinkou_age_raw(year, appid)
      saveRDS(out, file_loc)
    }
    out
  } else {
    collect_jinkou_age_raw(year, appid)
  }
}

survey_year_dataid <- list(
  `total` = c(
    `2020` = "0003445078",
    `2015` = "0003149040",
    `2010` = "0003038587",
    `2005` = "0000033784"),
  `age` = c(# 2-5-1
    `2020` = "0003445139",
    # 00310 gun-kei
    # 00320
    `2015` = "0003148521",
    # 00320
    `2010` = "0003041389",
    # 00401 0000033697 gun-kei
    # todouhuken
    `2005` = "0000033787"))

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
                                 statsDataId = survey_year_dataid$total[year])
  if (year == "2020") {
    df_raw %>%
      select_jinkou_cols()
  } else if (year == "2015") {
    df_raw %>%
      dplyr::filter(cat01_code == "00710",
                    cat02_code %in% c("010", "020", "030")) %>%
      select_jinkou_cols() %>%
      dplyr::mutate(gender = conv_gender_vars(gender))
  } else if (year == "2010") {
    df_raw %>%
      dplyr::filter(cat01_code == "00710",
                    cat02_code %in% c("000", "001", "002")) %>%
      select_jinkou_cols() %>%
      dplyr::mutate(gender = conv_gender_vars(gender)) %>%
      dplyr::mutate(area = dplyr::if_else(
        area_code == "12229" & area == intToUtf8(c(34966, 12534, 28006, 24066)),
        intToUtf8(c(34966, 12465, 28006, 24066)),
        area))
  } else if (year == "2005") {
    df_raw %>%
      dplyr::filter(cat01_code == "00700") %>%
      select_jinkou_cols()
  }
}

collect_jinkou_age_raw <- function(year, appid) {
  tab_code <- cat01_code <- cat02_code <- cat03_code <- cat04_code <- NULL
  unit <- gender <- age <- NULL
  year <- as.character(year)
  year <- rlang::arg_match(year,
                           as.character(seq.int(2000, 2020, by = 5)))
  cache_dir <- rappdirs::user_cache_dir("jpops")
  file_loc <- file.path(cache_dir,
                        paste0("jinkou_age_", year, "_raw.rds"))
  if (file.exists(file_loc)) {
    df_raw <-
      readRDS(file_loc)
  } else {
    df_raw <-
      estatapi::estat_getStatsData(appId = appid,
                                   statsDataId = survey_year_dataid$age[year])
    saveRDS(df_raw, file_loc)
  }
  if (year == "2020") {
    df_raw %>%
      dplyr::filter(cat01_code == "0") %>%
      dplyr::select(5:10, 14) %>%
      dplyr::rename(gender = 2,
                    age = 4,
                    area = 6)
  } else if (year == "2015") {
    df_raw %>%
      dplyr::filter(cat01_code == "00710",
                    cat04_code == "0000",
                    unit == intToUtf8(20154),
                    cat04_code == "0000") %>%
      dplyr::select(5:8, 11:12, 16) %>%
      dplyr::rename(gender = 4,
             age = 2,
             area = 6) %>%
      dplyr::mutate(gender = conv_gender_vars(gender),
                    age = dplyr::if_else(
                      stringr::str_detect(age,
                                          paste0("(",
                                                 intToUtf8(c(32207, 25968)),
                                                 "|",
                                                 intToUtf8(24180, 40802),
                                                 ")")),
                                          intToUtf8(c(32207, 25968)),
                      age))
  } else if (year == "2010") {
    df_raw %>%
      dplyr::filter(cat01_code == "00710",
                    unit == intToUtf8(20154),
                    cat03_code == "000") %>%
      dplyr::select(5:6, 9:10, 11:12, 16) %>%
      dplyr::rename(gender = 2,
                    age = 4,
                    area = 6) %>%
      dplyr::mutate(gender = conv_gender_vars(gender),
                    age = dplyr::if_else(
                      stringr::str_detect(age,
                                          paste0("(",
                                                 intToUtf8(c(32207, 25968)),
                                                 "|",
                                                 intToUtf8(24180, 40802),
                                                 ")")),
                      intToUtf8(c(32207, 25968)),
                      age))
  } else if (year == "2005") {
    df_raw %>%
      dplyr::filter(cat01_code == "00700") %>%
      dplyr::select(3:8, 12) %>%
      dplyr::rename(gender = 4,
                    age = 2,
                    area = 6)
  }
}
