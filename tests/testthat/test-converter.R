test_that("conv_gender_vars normalizes Japanese gender labels", {
  total <- intToUtf8(c(32207, 25968))
  male <- intToUtf8(30007)
  female <- intToUtf8(22899)

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
  total <- intToUtf8(c(32207, 25968))
  total_age <- intToUtf8(c(32207, 25968, 65288, 24180, 40802, 65289))
  unknown <- intToUtf8(c(19981, 35443))
  age_unknown <- intToUtf8(c(24180, 40802, 12300, 19981, 35443, 12301))
  age_zero <- intToUtf8(c(48, 27507))

  expect_equal(
    conv_age_vars(c(total, total_age, unknown, age_unknown, age_zero, NA)),
    c(total, total, unknown, unknown, age_zero, NA)
  )
})
