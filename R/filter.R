area_filter <- function(df, area) {
  area_code <- NULL
  area <- rlang::arg_match(area, c("all", "prefecture", "city"))
  if (area == "all") {
    return(df)
  }

  prefecture_codes <- sprintf("%02d000", seq_len(47L))
  if (area == "prefecture") {
    return(dplyr::filter(df, area_code %in% prefecture_codes))
  }

  dplyr::filter(
    df,
    !area_code %in% c("00000", prefecture_codes)
  )
}
