test_that("get_jinkou_age uses the v3 processed cache", {
  cache_dir <- withr::local_tempdir()
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  calls$cache <- logical()

  local_mocked_bindings(
    jpops_cache_dir = function(create = FALSE) cache_dir,
    collect_jinkou_age_raw = function(year, appid, cache = TRUE) {
      calls$count <- calls$count + 1L
      calls$cache <- c(calls$cache, cache)
      data.frame(
        area_code = "01000",
        age = intToUtf8(c(19981, 35443)),
        value = calls$count
      )
    },
    .package = "jpops"
  )

  first <- get_jinkou_age(2020, cache = TRUE)
  second <- get_jinkou_age(2020, cache = TRUE)

  expect_equal(c(first$value, second$value), c(1L, 1L))
  expect_identical(calls$count, 1L)
  expect_identical(calls$cache, TRUE)
  expect_true(file.exists(file.path(cache_dir, "jinkou_age_2020_v3.rds")))
  expect_false(file.exists(file.path(cache_dir, "jinkou_age_2020_v2.rds")))
})

test_that("get_jinkou_age bypasses caches when cache is false", {
  cache_dir <- withr::local_tempdir()
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  calls$cache <- logical()

  local_mocked_bindings(
    jpops_cache_dir = function(create = FALSE) cache_dir,
    collect_jinkou_age_raw = function(year, appid, cache = TRUE) {
      calls$count <- calls$count + 1L
      calls$cache <- c(calls$cache, cache)
      data.frame(area_code = "01000", value = calls$count)
    },
    .package = "jpops"
  )

  first <- get_jinkou_age(2020, cache = FALSE)
  second <- get_jinkou_age(2020, cache = FALSE)

  expect_equal(c(first$value, second$value), c(1L, 2L))
  expect_identical(calls$count, 2L)
  expect_identical(calls$cache, c(FALSE, FALSE))
  expect_length(list.files(cache_dir), 0L)
})

test_that("get_jinkou uses and bypasses its processed cache", {
  cache_dir <- withr::local_tempdir()
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L

  local_mocked_bindings(
    jpops_cache_dir = function(create = FALSE) cache_dir,
    collect_jinkou_raw = function(year, appid) {
      calls$count <- calls$count + 1L
      data.frame(area_code = "01000", value = calls$count)
    },
    .package = "jpops"
  )

  cached_first <- get_jinkou(2020, cache = TRUE)
  cached_second <- get_jinkou(2020, cache = TRUE)
  uncached <- get_jinkou(2020, cache = FALSE)

  expect_equal(
    c(cached_first$value, cached_second$value, uncached$value),
    c(1L, 1L, 2L)
  )
  expect_identical(calls$count, 2L)
})
