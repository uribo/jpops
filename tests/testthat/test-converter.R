test_that("conv_gender_vars normalizes Japanese gender labels", {
  total <- "\u7dcf\u6570"
  male <- "\u7537"
  female <- "\u5973"

  expect_equal(
    conv_gender_vars(c(total, male, female)),
    c(total, male, female)
  )
  expect_equal(
    conv_gender_vars(c(total, male, female), lang = "en"),
    c("total", "male", "female")
  )
})

test_that("conv_age_vars normalizes only cross-year special labels", {
  total <- "\u7dcf\u6570"
  total_age <- "\u7dcf\u6570\uff08\u5e74\u9f62\uff09"
  unknown <- "\u4e0d\u8a73"
  age_unknown <- "\u5e74\u9f62\u300c\u4e0d\u8a73\u300d"
  age_zero <- "0\u6b73"

  expect_equal(
    conv_age_vars(c(total, total_age, unknown, age_unknown, age_zero, NA)),
    c(total, total, unknown, unknown, age_zero, NA)
  )
})
