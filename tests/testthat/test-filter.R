make_public_area_meta <- function() {
  normalize_estat_area_meta(make_area_meta_fixture(), stats_data_id = "fixture")
}

make_public_population <- function(area_meta, age = FALSE) {
  out <- data.frame(
    gender = rep("\u7dcf\u6570", nrow(area_meta)),
    area_code = area_meta$area_code,
    area = area_meta$area_name,
    value = seq_len(nrow(area_meta))
  )
  if (age) {
    out$age <- "0\u6b73"
    out <- out[c("gender", "area_code", "area", "age", "value")]
  }
  out
}

test_that("area_filter implements every canonical area", {
  area_meta <- make_public_area_meta()
  df <- make_public_population(area_meta)

  expect_identical(area_filter(df, "all"), df)
  expect_identical(
    area_filter(df, "prefecture")$area_code,
    c("01000", "13000", "43000")
  )
  expect_identical(
    area_filter(df, "municipality", area_meta)$area_code,
    classify_area_codes(area_meta, "municipality")
  )
  expect_identical(
    area_filter(df, "ward", area_meta)$area_code,
    classify_area_codes(area_meta, "ward")
  )
})

test_that("area_filter preserves row order and columns", {
  area_meta <- make_public_area_meta()
  df <- make_public_population(area_meta)
  df$extra <- rev(seq_len(nrow(df)))

  for (area in c("all", "prefecture", "municipality", "ward")) {
    result <- area_filter(df, area, area_meta)
    expect_identical(names(result), names(df))
    expect_identical(result$extra, df$extra[df$area_code %in% result$area_code])
  }
})

test_that("hierarchical areas require metadata and full code coverage", {
  area_meta <- make_public_area_meta()
  df <- make_public_population(area_meta)

  expect_error(
    area_filter(df, "municipality"),
    class = "jpops_area_meta_required_error"
  )
  df$area_code[1] <- "99999"
  expect_error(
    area_filter(df, "ward", area_meta),
    "99999",
    class = "jpops_area_meta_population_code_error"
  )
})

test_that("both public functions support every canonical area and schema", {
  area_meta <- make_public_area_meta()
  total <- make_public_population(area_meta)
  age <- make_public_population(area_meta, age = TRUE)
  local_mocked_bindings(
    collect_jinkou_raw = function(year, appid) total,
    collect_jinkou_age_raw = function(year, appid, cache) age,
    collect_estat_area_meta = function(stats_data_id, appid, cache) area_meta,
    .package = "jpops"
  )

  expected <- list(
    all = area_meta$area_code,
    prefecture = c("01000", "13000", "43000"),
    municipality = classify_area_codes(area_meta, "municipality"),
    ward = classify_area_codes(area_meta, "ward")
  )
  for (area in names(expected)) {
    total_result <- get_jinkou(2020, cache = FALSE, .area = area)
    age_result <- get_jinkou_age(2020, cache = FALSE, .area = area)
    expect_identical(total_result$area_code, expected[[area]])
    expect_identical(age_result$area_code, expected[[area]])
    expect_identical(
      names(total_result),
      c("gender", "area_code", "area", "value")
    )
    expect_identical(
      names(age_result),
      c("gender", "area_code", "area", "age", "value")
    )
  }
})

test_that("all and prefecture never collect metadata", {
  total <- data.frame(
    gender = "\u7dcf\u6570",
    area_code = "01000",
    area = "prefecture",
    value = 1
  )
  age <- transform(total, age = "0\u6b73")
  age <- age[c("gender", "area_code", "area", "age", "value")]
  metadata_calls <- 0L
  local_mocked_bindings(
    collect_jinkou_raw = function(year, appid) total,
    collect_jinkou_age_raw = function(year, appid, cache) age,
    collect_estat_area_meta = function(...) {
      metadata_calls <<- metadata_calls + 1L
      stop("metadata must not be collected")
    },
    .package = "jpops"
  )

  get_jinkou(2020, cache = FALSE, .area = "all")
  get_jinkou_age(2020, cache = FALSE, .area = "all")
  get_jinkou(2020, cache = FALSE, .area = "prefecture")
  get_jinkou_age(2020, cache = FALSE, .area = "prefecture")

  expect_identical(metadata_calls, 0L)
})

test_that("hierarchical areas collect table metadata and propagate cache", {
  cache_dir <- withr::local_tempdir()
  area_meta <- make_public_area_meta()
  total <- make_public_population(area_meta)
  age <- make_public_population(area_meta, age = TRUE)
  calls <- list()
  local_mocked_bindings(
    jpops_cache_dir = function(create = FALSE) cache_dir,
    collect_jinkou_raw = function(year, appid) total,
    collect_jinkou_age_raw = function(year, appid, cache) age,
    collect_estat_area_meta = function(stats_data_id, appid, cache) {
      calls[[length(calls) + 1L]] <<- list(stats_data_id, appid, cache)
      area_meta
    },
    .package = "jpops"
  )

  get_jinkou(
    2020,
    appid = "total-appid",
    cache = TRUE,
    .area = "municipality"
  )
  get_jinkou_age(
    2020,
    appid = "age-appid",
    cache = FALSE,
    .area = "ward"
  )

  expect_identical(
    calls,
    list(
      list("0003445078", "total-appid", TRUE),
      list("0003445139", "age-appid", FALSE)
    )
  )
})

test_that("city warns and returns exactly municipality", {
  withr::local_options(lifecycle_verbosity = "warning")
  area_meta <- make_public_area_meta()
  total <- make_public_population(area_meta)
  age <- make_public_population(area_meta, age = TRUE)
  local_mocked_bindings(
    collect_jinkou_raw = function(year, appid) total,
    collect_jinkou_age_raw = function(year, appid, cache) age,
    collect_estat_area_meta = function(stats_data_id, appid, cache) area_meta,
    .package = "jpops"
  )

  expect_warning(
    total_city <- get_jinkou(2020, cache = FALSE, .area = "city"),
    "The meaning changed",
    class = "lifecycle_warning_deprecated"
  )
  total_municipality <- get_jinkou(
    2020,
    cache = FALSE,
    .area = "municipality"
  )
  expect_warning(
    age_city <- get_jinkou_age(2020, cache = FALSE, .area = "city"),
    "The meaning changed",
    class = "lifecycle_warning_deprecated"
  )
  age_municipality <- get_jinkou_age(
    2020,
    cache = FALSE,
    .area = "municipality"
  )

  expect_identical(total_city, total_municipality)
  expect_identical(age_city, age_municipality)
})

test_that("invalid public areas are arg_match errors", {
  local_mocked_bindings(
    collect_jinkou_raw = function(...) stop("must not fetch population"),
    collect_jinkou_age_raw = function(...) stop("must not fetch population"),
    .package = "jpops"
  )

  expect_error(
    get_jinkou(2020, cache = FALSE, .area = "invalid"),
    class = "rlang_error"
  )
  expect_error(
    get_jinkou_age(2020, cache = FALSE, .area = "invalid"),
    class = "rlang_error"
  )
})

test_that("population code coverage is checked after metadata collection", {
  area_meta <- make_public_area_meta()
  total <- make_public_population(area_meta)
  total$area_code[1] <- "99999"
  local_mocked_bindings(
    collect_jinkou_raw = function(year, appid) total,
    collect_estat_area_meta = function(stats_data_id, appid, cache) area_meta,
    .package = "jpops"
  )

  expect_error(
    get_jinkou(2020, cache = FALSE, .area = "municipality"),
    "99999",
    class = "jpops_area_meta_population_code_error"
  )
})

test_that("the city deprecation warning is not throttled away", {
  withr::local_options(lifecycle_verbosity = NULL)
  area_meta <- make_public_area_meta()
  total <- make_public_population(area_meta)
  local_mocked_bindings(
    collect_jinkou_raw = function(year, appid) total,
    collect_estat_area_meta = function(stats_data_id, appid, cache) area_meta,
    .package = "jpops"
  )

  warned <- 0L
  for (i in seq_len(3L)) {
    withCallingHandlers(
      get_jinkou(2020, cache = FALSE, .area = "city"),
      lifecycle_warning_deprecated = function(w) {
        warned <<- warned + 1L
        invokeRestart("muffleWarning")
      }
    )
  }

  expect_identical(warned, 3L)
})
