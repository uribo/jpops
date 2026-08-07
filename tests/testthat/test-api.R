test_that("Population Census tables retain normalized labels", {
  skip_if(Sys.getenv("ESTAT_TOKEN") == "")
  skip_if(Sys.getenv("JPOPS_RUN_API_TESTS") != "true")
  skip_on_ci()

  total <- "\u7dcf\u6570"
  unknown <- "\u4e0d\u8a73"
  age_unknown <- "\u5e74\u9f62\u300c\u4e0d\u8a73\u300d"
  age_zero <- "0\u6b73"
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
  municipality_data <- get_jinkou(
    2020,
    appid = Sys.getenv("ESTAT_TOKEN"),
    cache = FALSE,
    .area = "municipality"
  )

  expect_gt(nrow(prefecture_data), 0L)
  expect_gt(nrow(municipality_data), 0L)
})
