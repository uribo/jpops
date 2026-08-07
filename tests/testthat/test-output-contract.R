make_total_fixture <- function(year) {
  gender <- rep("\u7dcf\u6570", 2L)
  area_code <- c("01000", "01100")
  area <- c("prefecture", "city")
  value <- c(100, 50)

  if (year == "2020") {
    data.frame(
      tab_code = "tab",
      label = "value",
      cat01_code = "0",
      gender = gender,
      area_code = area_code,
      area = area,
      time_code = "2020",
      time = "2020",
      unit = "person",
      value = value,
      annotation = NA_character_
    )
  } else {
    cat01_code <- if (year == "2005") "00700" else "00710"
    cat02_code <- if (year == "2010") "000" else "010"
    data.frame(
      cat01_code = cat01_code,
      domain = "all",
      cat02_code = cat02_code,
      gender = gender,
      area_code = area_code,
      area = area,
      time_code = year,
      time = year,
      unit = "person",
      value = value,
      annotation = NA_character_
    )
  }
}

make_age_fixture <- function(year) {
  gender <- rep("\u7dcf\u6570", 2L)
  age <- rep("0\u6b73", 2L)
  area_code <- c("01000", "01100")
  area <- c("prefecture", "city")
  unit <- rep("\u4eba", 2L)
  value <- c(10, 5)

  if (year == "2020") {
    data.frame(
      tab_code = "tab",
      label = "value",
      cat01_code = "0",
      nationality = "all",
      cat02_code = "000",
      gender = gender,
      cat03_code = "000",
      age = age,
      area_code = area_code,
      area = area,
      time_code = year,
      time = year,
      unit = unit,
      value = value,
      annotation = NA_character_
    )
  } else if (year == "2015") {
    data.frame(
      tab_code = "tab",
      label = "value",
      cat01_code = "00710",
      domain = "all",
      cat02_code = "000",
      age = age,
      cat03_code = "000",
      gender = gender,
      cat04_code = "0000",
      nationality = "all",
      area_code = area_code,
      area = area,
      time_code = year,
      time = year,
      unit = unit,
      value = value,
      annotation = NA_character_
    )
  } else if (year == "2010") {
    data.frame(
      tab_code = "tab",
      label = "value",
      cat01_code = "00710",
      domain = "all",
      cat02_code = "000",
      gender = gender,
      cat03_code = "000",
      nationality = "all",
      cat04_code = "000",
      age = age,
      area_code = area_code,
      area = area,
      time_code = year,
      time = year,
      unit = unit,
      value = value,
      annotation = NA_character_
    )
  } else {
    data.frame(
      cat01_code = "00700",
      domain = "all",
      cat02_code = "000",
      age = age,
      cat03_code = "000",
      gender = gender,
      area_code = area_code,
      area = area,
      time_code = year,
      time = year,
      unit = unit,
      value = value,
      annotation = NA_character_
    )
  }
}

mock_estat_fixtures <- function() {
  fixtures <- list()
  for (year in c("2020", "2015", "2010", "2005")) {
    fixtures[[unname(survey_year_dataid$total[[year]])]] <-
      make_total_fixture(year)
    fixtures[[unname(survey_year_dataid$age[[year]])]] <-
      make_age_fixture(year)
  }
  fixtures
}

test_that("all survey years follow the public output contracts", {
  fixtures <- mock_estat_fixtures()
  local_mocked_bindings(
    fetch_estat_table = function(stats_data_id, appid, ...) {
      fixtures[[unname(stats_data_id)]]
    },
    .package = "jpops"
  )

  years <- c(2020, 2015, 2010, 2005)
  total_data <- lapply(years, get_jinkou, cache = FALSE)
  age_data <- lapply(years, get_jinkou_age, cache = FALSE)
  total_columns <- c("gender", "area_code", "area", "value")
  age_columns <- c("gender", "area_code", "area", "age", "value")

  expect_true(all(vapply(
    total_data,
    function(df) identical(names(df), total_columns),
    logical(1)
  )))
  expect_true(all(vapply(
    age_data,
    function(df) identical(names(df), age_columns),
    logical(1)
  )))
  expect_false(any(grepl(
    "^cat[0-9]+_code$",
    unlist(lapply(
      c(total_data, age_data),
      names
    ))
  )))
})

test_that("binding survey years does not introduce schema-specific columns", {
  fixtures <- mock_estat_fixtures()
  local_mocked_bindings(
    fetch_estat_table = function(stats_data_id, appid, ...) {
      fixtures[[unname(stats_data_id)]]
    },
    .package = "jpops"
  )

  years <- c(2020, 2015, 2010, 2005)
  total_data <- lapply(years, get_jinkou, cache = FALSE)
  age_data <- lapply(years, get_jinkou_age, cache = FALSE)

  expect_identical(
    names(dplyr::bind_rows(total_data, .id = "year")),
    c("year", "gender", "area_code", "area", "value")
  )
  expect_identical(
    names(dplyr::bind_rows(age_data, .id = "year")),
    c("year", "gender", "area_code", "area", "age", "value")
  )
})

test_that("area filtering works with the public output columns", {
  fixtures <- mock_estat_fixtures()
  area_meta <- normalize_estat_area_meta(make_area_meta_fixture())
  local_mocked_bindings(
    fetch_estat_table = function(stats_data_id, appid, ...) {
      fixtures[[unname(stats_data_id)]]
    },
    collect_estat_area_meta = function(stats_data_id, appid, cache) area_meta,
    .package = "jpops"
  )

  prefecture <- get_jinkou(2020, cache = FALSE, .area = "prefecture")
  municipality <- get_jinkou_age(
    2020,
    cache = FALSE,
    .area = "municipality"
  )

  expect_identical(prefecture$area_code, "01000")
  expect_identical(municipality$area_code, "01100")
  expect_identical(names(prefecture), c("gender", "area_code", "area", "value"))
  expect_identical(
    names(municipality),
    c("gender", "area_code", "area", "age", "value")
  )
})
