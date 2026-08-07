test_that("metadata cache paths encode stage and area version", {
  cache_dir <- withr::local_tempdir()
  local_mocked_bindings(
    jpops_cache_dir = function(create = FALSE) cache_dir,
    .package = "jpops"
  )

  expect_identical(
    jpops:::jpops_estat_meta_cache_file("0003149040", "raw"),
    file.path(cache_dir, "estat_meta_0003149040_raw.rds")
  )
  expect_identical(
    jpops:::jpops_estat_meta_cache_file("0003149040", "area"),
    file.path(cache_dir, "estat_area_meta_0003149040_v1.rds")
  )
})

test_that("metadata fetch adapter translates arguments", {
  received <- new.env(parent = emptyenv())
  local_mocked_bindings(
    estat_getMetaInfo = function(appId, statsDataId, ...) {
      received$appId <- appId
      received$statsDataId <- statsDataId
      received$dots <- list(...)
      "metadata"
    },
    .package = "estatapi"
  )

  result <- jpops:::fetch_estat_meta(
    "0003149040",
    "test-appid",
    lang = "E"
  )

  expect_identical(result, "metadata")
  expect_identical(received$appId, "test-appid")
  expect_identical(received$statsDataId, "0003149040")
  expect_identical(received$dots, list(lang = "E"))
})

test_that("first metadata call writes both caches and area cache is quiet", {
  cache_dir <- withr::local_tempdir()
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  local_mocked_bindings(
    jpops_cache_dir = function(create = FALSE) cache_dir,
    fetch_estat_meta = function(stats_data_id, appid, ...) {
      calls$count <- calls$count + 1L
      make_area_meta_fixture(kumamoto_defect = TRUE)
    },
    .package = "jpops"
  )

  expect_warning(
    first <- jpops:::collect_estat_area_meta("0003149040", "test-appid"),
    class = "jpops_area_meta_repair_warning"
  )
  expect_no_warning(
    second <- jpops:::collect_estat_area_meta("0003149040", "test-appid")
  )

  expect_identical(calls$count, 1L)
  expect_identical(second, first)
  expect_true(file.exists(file.path(
    cache_dir,
    "estat_meta_0003149040_raw.rds"
  )))
  expect_true(file.exists(file.path(
    cache_dir,
    "estat_area_meta_0003149040_v1.rds"
  )))
})

test_that("raw metadata cache is renormalized without fetching", {
  cache_dir <- withr::local_tempdir()
  raw_file <- file.path(cache_dir, "estat_meta_0003149040_raw.rds")
  saveRDS(make_area_meta_fixture(), raw_file)
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  local_mocked_bindings(
    jpops_cache_dir = function(create = FALSE) cache_dir,
    fetch_estat_meta = function(stats_data_id, appid, ...) {
      calls$count <- calls$count + 1L
      stop("API must not be called")
    },
    .package = "jpops"
  )

  area_meta <- jpops:::collect_estat_area_meta("0003149040", NULL)

  expect_identical(calls$count, 0L)
  expect_true(isTRUE(attr(
    area_meta,
    "jpops_area_meta_validated",
    exact = TRUE
  )))
  expect_true(file.exists(file.path(
    cache_dir,
    "estat_area_meta_0003149040_v1.rds"
  )))
})

test_that("new area cache version reuses unversioned raw cache", {
  cache_dir <- withr::local_tempdir()
  saveRDS(
    make_area_meta_fixture(),
    file.path(cache_dir, "estat_meta_0003149040_raw.rds")
  )
  saveRDS(
    "old-area-cache",
    file.path(cache_dir, "estat_area_meta_0003149040_v0.rds")
  )
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  local_mocked_bindings(
    jpops_cache_dir = function(create = FALSE) cache_dir,
    fetch_estat_meta = function(stats_data_id, appid, ...) {
      calls$count <- calls$count + 1L
      stop("API must not be called")
    },
    .package = "jpops"
  )

  area_meta <- jpops:::collect_estat_area_meta("0003149040", NULL)

  expect_identical(calls$count, 0L)
  expect_s3_class(area_meta, "data.frame")
  expect_true(file.exists(file.path(
    cache_dir,
    "estat_area_meta_0003149040_v1.rds"
  )))
})

test_that("cache false always fetches and never reads or writes caches", {
  cache_dir <- withr::local_tempdir()
  saveRDS(
    "invalid-cached-value",
    file.path(cache_dir, "estat_area_meta_0003149040_v1.rds")
  )
  initial_files <- list.files(cache_dir)
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  local_mocked_bindings(
    jpops_cache_dir = function(create = FALSE) cache_dir,
    fetch_estat_meta = function(stats_data_id, appid, ...) {
      calls$count <- calls$count + 1L
      make_area_meta_fixture()
    },
    .package = "jpops"
  )

  first <- jpops:::collect_estat_area_meta(
    "0003149040",
    "test-appid",
    cache = FALSE
  )
  second <- jpops:::collect_estat_area_meta(
    "0003149040",
    "test-appid",
    cache = FALSE
  )

  expect_identical(calls$count, 2L)
  expect_identical(first, second)
  expect_identical(list.files(cache_dir), initial_files)
})

test_that("metadata fetching explains when appid is required", {
  cache_dir <- withr::local_tempdir()
  fetches <- 0L
  local_mocked_bindings(
    jpops_cache_dir = function(create = FALSE) cache_dir,
    fetch_estat_meta = function(...) {
      fetches <<- fetches + 1L
      stop("must not fetch without an appid")
    },
    .package = "jpops"
  )

  expect_error(
    collect_estat_area_meta("0003149040", NULL, cache = TRUE),
    "need it on first use when no metadata cache exists",
    class = "jpops_estat_meta_appid_error"
  )
  expect_error(
    collect_estat_area_meta("0003149040", NULL, cache = FALSE),
    "with `cache = FALSE`, they always need it",
    class = "jpops_estat_meta_appid_error"
  )
  expect_identical(fetches, 0L)
})

test_that("corrupted area cache fails without fallback", {
  cache_dir <- withr::local_tempdir()
  area_file <- file.path(cache_dir, "estat_area_meta_0003149040_v1.rds")
  writeBin(charToRaw("not an rds file"), area_file)
  saveRDS(
    make_area_meta_fixture(),
    file.path(cache_dir, "estat_meta_0003149040_raw.rds")
  )
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  local_mocked_bindings(
    jpops_cache_dir = function(create = FALSE) cache_dir,
    fetch_estat_meta = function(stats_data_id, appid, ...) {
      calls$count <- calls$count + 1L
      make_area_meta_fixture()
    },
    .package = "jpops"
  )

  expect_error(
    jpops:::collect_estat_area_meta("0003149040", "test-appid")
  )
  expect_identical(calls$count, 0L)
})

test_that("area cache round trip preserves audit data and validation", {
  cache_dir <- withr::local_tempdir()
  local_mocked_bindings(
    jpops_cache_dir = function(create = FALSE) cache_dir,
    fetch_estat_meta = function(stats_data_id, appid, ...) {
      make_area_meta_fixture(kumamoto_defect = TRUE)
    },
    .package = "jpops"
  )

  expect_warning(
    first <- jpops:::collect_estat_area_meta("0003149040", "test-appid"),
    class = "jpops_area_meta_repair_warning"
  )
  expect_no_warning(
    cached <- jpops:::collect_estat_area_meta("0003149040", NULL)
  )

  audit_columns <- c(
    "parent_area_code_raw",
    "parent_repaired",
    "repair_rule"
  )
  expect_identical(cached[audit_columns], first[audit_columns])
  expect_identical(
    attr(cached, "repair_log", exact = TRUE),
    attr(first, "repair_log", exact = TRUE)
  )
  expect_true(isTRUE(attr(
    cached,
    "jpops_area_meta_validated",
    exact = TRUE
  )))
  expect_type(
    jpops:::classify_area_codes(cached, "municipality"),
    "character"
  )
  expect_type(jpops:::classify_area_codes(cached, "ward"), "character")
})

test_that("a foreign object at the area cache path fails loudly", {
  cache_dir <- withr::local_tempdir()
  fetches <- 0L
  local_mocked_bindings(
    jpops_cache_dir = function(create = FALSE) cache_dir,
    fetch_estat_meta = function(stats_data_id, appid, ...) {
      fetches <<- fetches + 1L
      make_area_meta_fixture()
    },
    .package = "jpops"
  )

  saveRDS(
    data.frame(x = 1),
    jpops_estat_meta_cache_file("0000000001", "area", create = TRUE)
  )

  expect_error(
    collect_estat_area_meta("0000000001", appid = NULL, cache = TRUE),
    class = "jpops_area_meta_cache_error"
  )
  expect_identical(fetches, 0L)
})

test_that("area metadata stripped of its validation attribute is rejected", {
  cache_dir <- withr::local_tempdir()
  local_mocked_bindings(
    jpops_cache_dir = function(create = FALSE) cache_dir,
    fetch_estat_meta = function(stats_data_id, appid, ...) {
      make_area_meta_fixture()
    },
    .package = "jpops"
  )

  area_meta <- collect_estat_area_meta("0000000001", appid = "test-appid")
  attr(area_meta, "jpops_area_meta_validated") <- NULL
  saveRDS(area_meta, jpops_estat_meta_cache_file("0000000001", "area"))

  expect_error(
    collect_estat_area_meta("0000000001", appid = NULL, cache = TRUE),
    class = "jpops_area_meta_cache_error"
  )
})
