test_that("area metadata are normalized to the internal contract", {
  raw <- make_area_meta_fixture()
  meta <- normalize_estat_area_meta(
    raw,
    data_area_codes = raw$area[["@code"]],
    stats_data_id = "fixture"
  )

  expect_identical(
    names(meta),
    c(
      "area_code",
      "area_name",
      "area_level",
      "parent_area_code",
      "parent_area_code_raw",
      "parent_repaired",
      "repair_rule"
    )
  )
  expect_type(meta$area_code, "character")
  expect_type(meta$area_level, "integer")
  expect_true(isTRUE(attr(meta, "jpops_area_meta_validated")))
  expect_equal(
    attr(meta, "repair_log"),
    data.frame(
      stats_data_id = character(),
      area_code = character(),
      parent_area_code_raw = character(),
      parent_area_code = character(),
      repair_rule = character(),
      stringsAsFactors = FALSE
    )
  )
})

test_that("level 2 roots and measured level transitions are accepted", {
  raw <- make_area_meta_fixture(include_national = FALSE)
  meta <- normalize_estat_area_meta(raw)

  expect_true(all(is.na(meta$parent_area_code[meta$area_level == 2L])))
  expect_equal(meta$parent_area_code[meta$area_code == "01400"], "01300")
  expect_equal(meta$parent_area_code[meta$area_code == "01299"], "01200")
})

test_that("structural metadata defects fail loudly", {
  duplicate <- make_area_meta_fixture()
  duplicate$area[nrow(duplicate$area), "@code"] <- "01000"
  expect_error(
    normalize_estat_area_meta(duplicate),
    class = "jpops_area_meta_duplicate_error"
  )

  self <- make_area_meta_fixture()
  self$area[self$area[["@code"]] == "01100", "@parentCode"] <- "01100"
  expect_error(
    normalize_estat_area_meta(self),
    class = "jpops_area_meta_self_reference_error"
  )

  dangling <- make_area_meta_fixture()
  dangling$area[dangling$area[["@code"]] == "01100", "@parentCode"] <- "99999"
  expect_error(
    normalize_estat_area_meta(dangling),
    class = "jpops_area_meta_dangling_parent_error"
  )

  missing_parent <- make_area_meta_fixture()
  missing_parent$area[
    missing_parent$area[["@code"]] == "01100",
    "@parentCode"
  ] <- NA_character_
  expect_error(
    normalize_estat_area_meta(missing_parent),
    class = "jpops_area_meta_root_error"
  )

  missing_level <- make_area_meta_fixture()
  missing_level$area[1L, "@level"] <- NA_character_
  expect_error(
    normalize_estat_area_meta(missing_level),
    class = "jpops_area_meta_level_error"
  )

  cycle <- make_area_meta_fixture()
  cycle$area[cycle$area[["@code"]] == "01000", "@parentCode"] <- "01300"
  expect_error(
    normalize_estat_area_meta(cycle),
    class = "jpops_area_meta_cycle_error"
  )

  expect_error(
    normalize_estat_area_meta(
      make_area_meta_fixture(),
      data_area_codes = c("01000", "99999")
    ),
    class = "jpops_area_meta_population_code_error"
  )
})

test_that("unmeasured transitions and paths outside prefectures fail", {
  invalid_transition <- make_area_meta_fixture()
  invalid_transition$area[
    invalid_transition$area[["@code"]] == "01299",
    "@parentCode"
  ] <- "01101"
  expect_error(
    normalize_estat_area_meta(invalid_transition),
    class = "jpops_area_meta_transition_error"
  )

  no_prefecture <- make_area_meta_fixture()
  no_prefecture$area[
    no_prefecture$area[["@code"]] == "01300",
    "@parentCode"
  ] <- "00000"
  expect_error(
    normalize_estat_area_meta(no_prefecture),
    class = "jpops_area_meta_transition_error"
  )
})

test_that("the narrow level 5 repair warns and remains auditable", {
  expect_warning(
    meta <- normalize_estat_area_meta(
      make_area_meta_fixture(kumamoto_defect = TRUE),
      stats_data_id = "0003149040"
    ),
    class = "jpops_area_meta_repair_warning"
  )

  repaired <- meta$area_code %in% sprintf("431%02d", 1:5)
  expect_equal(meta$parent_area_code[repaired], rep("43100", 5L))
  expect_equal(meta$parent_area_code_raw[repaired], rep("43000", 5L))
  expect_true(all(meta$parent_repaired[repaired]))
  expect_equal(meta$repair_rule[repaired], rep("level5_parent_from_code", 5L))
  log <- attr(meta, "repair_log")
  expect_equal(nrow(log), 5L)
  expect_equal(log$stats_data_id, rep("0003149040", 5L))
  expect_equal(log$area_code, sprintf("431%02d", 1:5))
})

test_that("unsafe level 5 repairs stop without guessing", {
  duplicated_candidate <- make_area_meta_fixture(kumamoto_defect = TRUE)
  duplicated_candidate$area <- rbind(
    duplicated_candidate$area,
    duplicated_candidate$area[
      duplicated_candidate$area[["@code"]] == "43100",
    ]
  )
  expect_error(
    normalize_estat_area_meta(duplicated_candidate),
    class = "jpops_area_meta_duplicate_error"
  )

  missing_candidate <- make_area_meta_fixture(kumamoto_defect = TRUE)
  missing_candidate$area[
    missing_candidate$area[["@code"]] == "43100",
    "@code"
  ] <- "43900"
  expect_error(
    normalize_estat_area_meta(missing_candidate),
    class = "jpops_area_meta_repair_error"
  )

  wrong_level <- make_area_meta_fixture(kumamoto_defect = TRUE)
  wrong_level$area[
    wrong_level$area[["@code"]] == "43100",
    "@level"
  ] <- "3"
  expect_error(
    normalize_estat_area_meta(wrong_level),
    class = "jpops_area_meta_repair_error"
  )

  wrong_parent <- make_area_meta_fixture(kumamoto_defect = TRUE)
  wrong_parent$area[
    wrong_parent$area[["@code"]] == "43100",
    "@parentCode"
  ] <- "13000"
  expect_error(
    normalize_estat_area_meta(wrong_parent),
    class = "jpops_area_meta_repair_error"
  )
})

test_that("classification returns hierarchy frontiers", {
  meta <- normalize_estat_area_meta(make_area_meta_fixture())

  municipality <- classify_area_codes(meta, "municipality")
  ward <- classify_area_codes(meta, "ward")

  expect_equal(
    municipality,
    c("01100", "01200", "01400", "13100", "43100", "43200", "43400")
  )
  expect_equal(
    ward,
    c(
      "01101",
      "01200",
      "01400",
      "13101",
      "13102",
      "43101",
      "43102",
      "43103",
      "43104",
      "43105",
      "43200",
      "43400"
    )
  )
  expect_false("01299" %in% c(municipality, ward))
  expect_false(all(c("13100", "13101") %in% municipality))
  unvalidated <- meta
  attr(unvalidated, "jpops_area_meta_validated") <- NULL
  expect_error(
    classify_area_codes(unvalidated, "municipality"),
    class = "jpops_area_meta_unvalidated_error"
  )
})

test_that("population fixtures are additive by gender and age", {
  meta <- normalize_estat_area_meta(make_area_meta_fixture())
  population <- make_population_fixture(meta)

  for (area in c("municipality", "ward")) {
    diagnostics <- diagnose_area_additivity(
      population,
      meta,
      area,
      group_vars = c("gender", "age")
    )
    expect_true(all(diagnostics$difference == 0))
    expect_true(all(lengths(diagnostics$missing_area_codes) == 0L))
  }
})

test_that("additivity diagnostics identify excess and missing codes", {
  raw_defect <- make_area_meta_fixture(kumamoto_defect = TRUE)
  raw_area <- raw_defect$area
  raw_levels <- as.integer(raw_area[["@level"]])
  candidate <- raw_levels %in% c(4L, 5L, 6L)
  naive_codes <- raw_area[["@code"]][
    candidate & !raw_area[["@parentCode"]] %in% raw_area[["@code"]][candidate]
  ]
  expect_true(all(c("43100", sprintf("431%02d", 1:5)) %in% naive_codes))

  expect_warning(
    meta <- normalize_estat_area_meta(raw_defect),
    class = "jpops_area_meta_repair_warning"
  )
  population <- make_population_fixture(meta)
  before_repair_sum <- sum(
    population$value[
      population$area_code %in%
        naive_codes &
        population$gender == "\u7dcf\u6570" &
        population$age == "0\u6b73" &
        population$area_code %in% c("43100", sprintf("431%02d", 1:5))
    ],
    na.rm = TRUE
  )
  expect_equal(before_repair_sum - 100, 100)

  repaired <- diagnose_area_additivity(
    population,
    meta,
    "ward",
    group_vars = c("gender", "age")
  )
  expect_true(all(repaired$difference == 0))

  duplicated_ward <- population[
    population$area_code == "43101" &
      population$gender == "\u7dcf\u6570" &
      population$age == "0\u6b73",
  ]
  excess <- diagnose_area_additivity(
    rbind(population, duplicated_ward),
    meta,
    "ward",
    group_vars = c("gender", "age")
  )
  excess_kumamoto <- excess[
    excess$prefecture_code == "43000" &
      excess$gender == "\u7dcf\u6570" &
      excess$age == "0\u6b73",
  ]
  expect_equal(excess_kumamoto$difference, duplicated_ward$value)
  expect_true(
    "43101" %in% excess_kumamoto$selected_area_codes[[1L]]
  )

  missing <- population[population$area_code != "43400", ]
  diagnostics <- diagnose_area_additivity(
    missing,
    meta,
    "municipality",
    group_vars = c("gender", "age")
  )
  kumamoto <- diagnostics[diagnostics$prefecture_code == "43000", ]
  expect_true(all(kumamoto$difference < 0))
  expect_true(all(vapply(
    kumamoto$missing_area_codes,
    function(codes) identical(codes, "43400"),
    logical(1)
  )))

  unknown <- rbind(
    population,
    data.frame(
      gender = "\u7dcf\u6570",
      age = "0\u6b73",
      area_code = "99999",
      value = 1
    )
  )
  expect_error(
    diagnose_area_additivity(
      unknown,
      meta,
      "municipality",
      group_vars = c("gender", "age")
    ),
    class = "jpops_area_additivity_input_error"
  )
})
