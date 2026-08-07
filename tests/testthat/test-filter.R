test_that("area_filter uses exact prefecture-code matching", {
  df <- data.frame(
    area_code = c("00000", "01000", "01100", "010001"),
    value = seq_len(4)
  )

  expect_equal(area_filter(df, "prefecture")$area_code, "01000")
  expect_equal(area_filter(df, "city")$area_code, c("01100", "010001"))
})

test_that("area_filter passes all through and rejects invalid areas", {
  df <- data.frame(area_code = "01000")

  expect_identical(area_filter(df, "all"), df)
  expect_error(
    area_filter(df, "invalid"),
    class = "rlang_error"
  )
})

test_that("both public functions use the shared area-filter path", {
  calls <- character()
  local_mocked_bindings(
    collect_jinkou_raw = function(year, appid) {
      data.frame(area_code = c("01000", "01100"), value = c(10, 5))
    },
    collect_jinkou_age_raw = function(year, appid, cache) {
      data.frame(area_code = c("01000", "01100"), value = c(10, 5))
    },
    area_filter = function(df, area) {
      calls <<- c(calls, area)
      df
    },
    .package = "jpops"
  )

  get_jinkou(2020, cache = FALSE, .area = "prefecture")
  get_jinkou_age(2020, cache = FALSE, .area = "city")

  expect_identical(calls, c("prefecture", "city"))
})
