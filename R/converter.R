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
      stringr::str_detect(x, JPOPS_LABEL_TOTAL) ~ JPOPS_LABEL_TOTAL,
      stringr::str_detect(x, JPOPS_LABEL_MALE) ~ JPOPS_LABEL_MALE,
      stringr::str_detect(x, JPOPS_LABEL_FEMALE) ~ JPOPS_LABEL_FEMALE
    )
  } else if (lang == "en") {
    dplyr::case_when(
      stringr::str_detect(x, JPOPS_LABEL_TOTAL) ~ "total",
      stringr::str_detect(x, JPOPS_LABEL_MALE) ~ "male",
      stringr::str_detect(x, JPOPS_LABEL_FEMALE) ~ "female"
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
  dplyr::case_when(
    x == JPOPS_LABEL_TOTAL_AGE ~ JPOPS_LABEL_TOTAL,
    x == JPOPS_LABEL_AGE_UNKNOWN ~ JPOPS_LABEL_UNKNOWN,
    TRUE ~ x
  )
}
