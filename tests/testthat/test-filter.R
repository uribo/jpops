test_that("area_filter preserves its current area-code matching behavior", {
  df <- data.frame(
    area_code = c("00000", "01000", "01100", "010001"),
    value = seq_len(4)
  )

  expect_equal(area_filter(df, "prefecture")$area_code, c("01000", "010001"))
  expect_equal(area_filter(df, "city")$area_code, "01100")
})

test_that("area_filter preserves its current invalid-area error", {
  df <- data.frame(area_code = "01000")

  expect_error(area_filter(df, "all"), "object 'out' not found", fixed = TRUE)
})
