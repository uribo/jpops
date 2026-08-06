test_that("match_survey_year accepts registered years", {
  expect_identical(match_survey_year(2020, "total"), "2020")
  expect_identical(match_survey_year("2005", "age"), "2005")
})

test_that("match_survey_year rejects unregistered years", {
  expect_error(match_survey_year(2000, "total"))
  expect_error(match_survey_year(2021, "age"))
})
