conv_gender_vars <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, intToUtf8(c(32207, 25968))) ~ intToUtf8(c(32207, 25968)),
    stringr::str_detect(x, intToUtf8(30007)) ~ intToUtf8(30007),
    stringr::str_detect(x, intToUtf8(22899)) ~ intToUtf8(22899)
  )
}
