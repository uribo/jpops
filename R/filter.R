area_filter <- function(df, area) {
  area_code <- NULL
  if (area == "prefecture") {
    df %>%
      dplyr::filter(stringr::str_detect(area_code,
                                        paste0(stringr::str_pad(seq.int(1, 47),
                                                                width = 2,
                                                                pad = "0"),
                                               "000",
                                               collapse = "|")),
                    area_code != "00000")
  } else if (area == "city") {
    df %>%
      dplyr::filter(stringr::str_detect(area_code,
                                      paste0(stringr::str_pad(seq.int(1, 47),
                                                              width = 2,
                                                              pad = "0"),
                                             "000",
                                             collapse = "|"),
                                      negate = TRUE),
                  area_code != "00000")
  }
}
