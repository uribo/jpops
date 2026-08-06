test_that("Population Census tables retain normalized labels", {
  skip_if(Sys.getenv("ESTAT_TOKEN") == "")
  skip_if(Sys.getenv("JPOPS_RUN_API_TESTS") != "true")
  skip_on_ci()

  total <- intToUtf8(c(32207, 25968))
  unknown <- intToUtf8(c(19981, 35443))
  age_unknown <- intToUtf8(c(24180, 40802, 12300, 19981, 35443, 12301))
  age_zero <- intToUtf8(c(48, 27507))
  total_columns <- c("gender", "area_code", "area", "value")
  age_columns <- c("gender", "area_code", "area", "age", "value")

  for (year in c(2020, 2015, 2010, 2005)) {
    age_data <- get_jinkou_age(
      year,
      appid = Sys.getenv("ESTAT_TOKEN"),
      cache = FALSE
    )
    total_data <- get_jinkou(
      year,
      appid = Sys.getenv("ESTAT_TOKEN"),
      cache = FALSE
    )

    expect_true(all(c(total, unknown, age_zero) %in% unique(age_data$age)))
    expect_false(age_unknown %in% unique(age_data$age))
    expect_identical(names(age_data), age_columns)
    expect_identical(names(total_data), total_columns)
    expect_gt(nrow(total_data), 0L)
  }

  prefecture_data <- get_jinkou_age(
    2020,
    appid = Sys.getenv("ESTAT_TOKEN"),
    cache = FALSE,
    .area = "prefecture"
  )
  city_data <- get_jinkou(
    2020,
    appid = Sys.getenv("ESTAT_TOKEN"),
    cache = FALSE,
    .area = "city"
  )

  expect_gt(nrow(prefecture_data), 0L)
  expect_gt(nrow(city_data), 0L)
})
