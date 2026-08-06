#' Convert e-Stat's record value
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' @param x variable
#' @param lang Specify the language to convert
#' @export
conv_gender_vars <- function(x, lang = "ja") {
  lang <- rlang::arg_match(lang, c("ja", "en"))
  if (lang == "ja") {
    dplyr::case_when(
      stringr::str_detect(x, intToUtf8(c(32207, 25968))) ~ intToUtf8(c(
        32207,
        25968
      )),
      stringr::str_detect(x, intToUtf8(30007)) ~ intToUtf8(30007),
      stringr::str_detect(x, intToUtf8(22899)) ~ intToUtf8(22899)
    )
  } else if (lang == "en") {
    dplyr::case_when(
      stringr::str_detect(x, intToUtf8(c(32207, 25968))) ~ "total",
      stringr::str_detect(x, intToUtf8(30007)) ~ "male",
      stringr::str_detect(x, intToUtf8(22899)) ~ "female"
    )
  }
}

#' Convert e-Stat's age labels
#'
#' @description
#' Normalize age labels that differ between Population Census tables.
#' @param x variable
#' @export
conv_age_vars <- function(x) {
  total <- intToUtf8(c(32207, 25968))
  total_age <- intToUtf8(c(32207, 25968, 65288, 24180, 40802, 65289))
  unknown <- intToUtf8(c(19981, 35443))
  age_unknown <- intToUtf8(c(24180, 40802, 12300, 19981, 35443, 12301))

  dplyr::case_when(
    x == total_age ~ total,
    x == age_unknown ~ unknown,
    TRUE ~ x
  )
}
