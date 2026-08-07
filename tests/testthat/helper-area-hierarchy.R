make_area_meta_fixture <- function(
  kumamoto_defect = FALSE,
  include_national = TRUE
) {
  code <- c(
    "00000",
    "01000",
    "01300",
    "01100",
    "01101",
    "01200",
    "01299",
    "01400",
    "13000",
    "13100",
    "13101",
    "13102",
    "43000",
    "43100",
    "43101",
    "43102",
    "43103",
    "43104",
    "43105",
    "43200",
    "43400"
  )
  level <- c(
    1L,
    2L,
    3L,
    4L,
    5L,
    4L,
    7L,
    6L,
    2L,
    4L,
    4L,
    4L,
    2L,
    4L,
    5L,
    5L,
    5L,
    5L,
    5L,
    4L,
    6L
  )
  parent <- c(
    NA,
    "00000",
    "01000",
    "01000",
    "01100",
    "01000",
    "01200",
    "01300",
    "00000",
    "13000",
    "13100",
    "13100",
    "00000",
    "43000",
    rep(if (kumamoto_defect) "43000" else "43100", 5L),
    "43000",
    "43000"
  )
  if (!include_national) {
    keep <- code != "00000"
    code <- code[keep]
    level <- level[keep]
    parent <- parent[keep]
    parent[level == 2L] <- NA_character_
  }

  list(
    tab = data.frame(),
    area = data.frame(
      "@code" = code,
      "@name" = paste0("area-", code),
      "@level" = as.character(level),
      "@parentCode" = parent,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    .names = c("tab", "area")
  )
}

make_population_fixture <- function(area_meta) {
  values <- c(
    "00000" = 600,
    "01000" = 300,
    "01300" = 50,
    "01100" = 100,
    "01101" = 100,
    "01200" = 150,
    "01299" = 150,
    "01400" = 50,
    "13000" = 100,
    "13100" = 100,
    "13101" = 40,
    "13102" = 60,
    "43000" = 200,
    "43100" = 100,
    "43101" = 10,
    "43102" = 20,
    "43103" = 20,
    "43104" = 20,
    "43105" = 30,
    "43200" = 60,
    "43400" = 40
  )
  codes <- area_meta$area_code
  groups <- expand.grid(
    gender = c("\u7dcf\u6570", "\u7537"),
    age = c("0\u6b73", "1\u6b73"),
    area_code = codes,
    stringsAsFactors = FALSE
  )
  multiplier <- ifelse(groups$gender == "\u7dcf\u6570", 1, 0.4) *
    ifelse(groups$age == "0\u6b73", 1, 0.5)
  groups$value <- unname(values[groups$area_code]) * multiplier
  groups$value[
    groups$area_code == "01400" &
      groups$gender == "\u7537" &
      groups$age == "1\u6b73"
  ] <- NA_real_
  groups$value[
    groups$area_code == "01000" &
      groups$gender == "\u7537" &
      groups$age == "1\u6b73"
  ] <- 50
  groups
}
