JPOPS_AREA_REPAIR_LEVEL5_PARENT <- "level5_parent_from_code"

abort_area_meta <- function(message, subclass, ...) {
  rlang::abort(
    message,
    class = c(subclass, "jpops_area_meta_error"),
    ...
  )
}

normalize_estat_area_meta <- function(
  meta,
  data_area_codes = NULL,
  stats_data_id = NA_character_
) {
  required <- c("@code", "@name", "@level", "@parentCode")
  if (!is.list(meta) || is.null(meta[["area"]])) {
    abort_area_meta(
      "`meta` must contain an `area` table.",
      "jpops_area_meta_shape_error"
    )
  }

  area <- meta[["area"]]
  if (!is.data.frame(area) || !all(required %in% names(area))) {
    abort_area_meta(
      paste0(
        "The `area` table must contain columns: ",
        paste(required, collapse = ", "),
        "."
      ),
      "jpops_area_meta_shape_error"
    )
  }
  if (!all(vapply(area[required], is.character, logical(1)))) {
    abort_area_meta(
      "All required `area` metadata columns must be character vectors.",
      "jpops_area_meta_type_error"
    )
  }

  raw_level <- area[["@level"]]
  valid_level <- !is.na(raw_level) & grepl("^[0-9]+$", raw_level)
  if (!all(valid_level)) {
    abort_area_meta(
      "`@level` must contain non-missing integer strings.",
      "jpops_area_meta_level_error"
    )
  }

  parent <- area[["@parentCode"]]
  parent[!is.na(parent) & parent == ""] <- NA_character_
  out <- data.frame(
    area_code = area[["@code"]],
    area_name = area[["@name"]],
    area_level = as.integer(raw_level),
    parent_area_code = parent,
    parent_area_code_raw = parent,
    parent_repaired = FALSE,
    repair_rule = NA_character_,
    stringsAsFactors = FALSE
  )

  validate_area_meta_structure(out, data_area_codes)
  out <- repair_area_meta(out, stats_data_id)
  validate_area_meta_hierarchy(out)

  attr(out, "jpops_area_meta_validated") <- TRUE
  if (is.null(attr(out, "repair_log", exact = TRUE))) {
    attr(out, "repair_log") <- empty_area_repair_log()
  }
  out
}

empty_area_repair_log <- function() {
  data.frame(
    stats_data_id = character(),
    area_code = character(),
    parent_area_code_raw = character(),
    parent_area_code = character(),
    repair_rule = character(),
    stringsAsFactors = FALSE
  )
}

validate_area_meta_structure <- function(area_meta, data_area_codes = NULL) {
  codes <- area_meta$area_code
  parents <- area_meta$parent_area_code
  levels <- area_meta$area_level

  if (anyNA(codes) || any(codes == "")) {
    abort_area_meta(
      "Area codes must be non-missing and non-empty.",
      "jpops_area_meta_code_error"
    )
  }
  duplicated_codes <- unique(codes[duplicated(codes)])
  if (length(duplicated_codes) > 0L) {
    abort_area_meta(
      paste0(
        "Duplicated area codes: ",
        paste(duplicated_codes, collapse = ", ")
      ),
      "jpops_area_meta_duplicate_error",
      area_codes = duplicated_codes
    )
  }
  self_references <- codes[!is.na(parents) & codes == parents]
  if (length(self_references) > 0L) {
    abort_area_meta(
      paste0(
        "Self-referencing area codes: ",
        paste(self_references, collapse = ", ")
      ),
      "jpops_area_meta_self_reference_error",
      area_codes = self_references
    )
  }
  dangling <- unique(parents[!is.na(parents) & !parents %in% codes])
  if (length(dangling) > 0L) {
    abort_area_meta(
      paste0(
        "Parent area codes are missing from metadata: ",
        paste(dangling, collapse = ", ")
      ),
      "jpops_area_meta_dangling_parent_error",
      parent_area_codes = dangling
    )
  }
  invalid_roots <- codes[is.na(parents) & !levels %in% c(1L, 2L)]
  if (length(invalid_roots) > 0L) {
    abort_area_meta(
      paste0(
        "Only level 1 or level 2 nodes may have no parent: ",
        paste(invalid_roots, collapse = ", ")
      ),
      "jpops_area_meta_root_error",
      area_codes = invalid_roots
    )
  }
  if (anyNA(levels)) {
    abort_area_meta(
      "Area levels must not be missing.",
      "jpops_area_meta_level_error"
    )
  }

  assert_acyclic_area_meta(area_meta)

  if (!is.null(data_area_codes)) {
    missing_codes <- unique(data_area_codes[!data_area_codes %in% codes])
    missing_codes <- missing_codes[!is.na(missing_codes)]
    if (length(missing_codes) > 0L) {
      abort_area_meta(
        paste0(
          "Population area codes are missing from metadata: ",
          paste(missing_codes, collapse = ", ")
        ),
        "jpops_area_meta_population_code_error",
        area_codes = missing_codes
      )
    }
  }
  invisible(area_meta)
}

assert_acyclic_area_meta <- function(area_meta) {
  parent_map <- stats::setNames(area_meta$parent_area_code, area_meta$area_code)
  for (start in area_meta$area_code) {
    seen <- character()
    current <- start
    while (!is.na(current)) {
      if (current %in% seen) {
        cycle <- c(seen[match(current, seen):length(seen)], current)
        abort_area_meta(
          paste0("Cycle in area hierarchy: ", paste(cycle, collapse = " -> ")),
          "jpops_area_meta_cycle_error",
          area_codes = cycle
        )
      }
      seen <- c(seen, current)
      current <- unname(parent_map[[current]])
    }
  }
  invisible(area_meta)
}

repair_area_meta <- function(area_meta, stats_data_id) {
  level_map <- stats::setNames(area_meta$area_level, area_meta$area_code)
  parent_map <- stats::setNames(area_meta$parent_area_code, area_meta$area_code)
  parent_levels <- unname(level_map[area_meta$parent_area_code])
  candidates <- which(area_meta$area_level == 5L & parent_levels != 4L)

  if (length(candidates) == 0L) {
    attr(area_meta, "repair_log") <- empty_area_repair_log()
    return(area_meta)
  }

  repaired_parents <- paste0(
    substr(area_meta$area_code[candidates], 1L, 3L),
    "00"
  )
  safe <- grepl("^[0-9]{5}$", area_meta$area_code[candidates]) &
    repaired_parents %in% area_meta$area_code &
    unname(level_map[repaired_parents]) == 4L &
    unname(parent_map[repaired_parents]) ==
      area_meta$parent_area_code[candidates]

  if (!all(safe)) {
    failed <- candidates[!safe]
    abort_area_meta(
      paste0(
        "Unsafe level 5 parent repair for area codes: ",
        paste(area_meta$area_code[failed], collapse = ", "),
        "."
      ),
      "jpops_area_meta_repair_error",
      area_codes = area_meta$area_code[failed],
      derived_parent_area_codes = repaired_parents[!safe]
    )
  }

  area_meta$parent_area_code[candidates] <- repaired_parents
  area_meta$parent_repaired[candidates] <- TRUE
  area_meta$repair_rule[candidates] <- JPOPS_AREA_REPAIR_LEVEL5_PARENT
  repair_log <- data.frame(
    stats_data_id = rep(as.character(stats_data_id), length(candidates)),
    area_code = area_meta$area_code[candidates],
    parent_area_code_raw = area_meta$parent_area_code_raw[candidates],
    parent_area_code = repaired_parents,
    repair_rule = rep(JPOPS_AREA_REPAIR_LEVEL5_PARENT, length(candidates)),
    stringsAsFactors = FALSE
  )
  attr(area_meta, "repair_log") <- repair_log
  rlang::warn(
    paste0(
      "Repaired ",
      length(candidates),
      " level 5 area parent",
      if (length(candidates) == 1L) "" else "s",
      " for statsDataId `",
      stats_data_id,
      "`: ",
      paste(area_meta$area_code[candidates], collapse = ", "),
      "."
    ),
    class = "jpops_area_meta_repair_warning",
    repair_log = repair_log
  )
  area_meta
}

validate_area_meta_hierarchy <- function(area_meta) {
  allowed <- c(
    "2<-1",
    "3<-2",
    "4<-2",
    "4<-4",
    "5<-4",
    "6<-2",
    "6<-3",
    "7<-2",
    "7<-3",
    "7<-4"
  )
  has_parent <- !is.na(area_meta$parent_area_code)
  parent_level <- stats::setNames(
    area_meta$area_level,
    area_meta$area_code
  )[area_meta$parent_area_code[has_parent]]
  transitions <- paste0(
    area_meta$area_level[has_parent],
    "<-",
    unname(parent_level)
  )
  invalid <- which(has_parent)[!transitions %in% allowed]
  if (length(invalid) > 0L) {
    abort_area_meta(
      paste0(
        "Disallowed child-parent level transitions for area codes: ",
        paste(area_meta$area_code[invalid], collapse = ", "),
        "."
      ),
      "jpops_area_meta_transition_error",
      area_codes = area_meta$area_code[invalid],
      transitions = transitions[!transitions %in% allowed]
    )
  }

  for (code in area_meta$area_code[area_meta$area_level >= 3L]) {
    ancestors <- area_ancestors(code, area_meta)
    ancestor_levels <- area_meta$area_level[match(
      ancestors,
      area_meta$area_code
    )]
    if (!2L %in% ancestor_levels) {
      abort_area_meta(
        paste0("Area code does not reach a level 2 prefecture: ", code, "."),
        "jpops_area_meta_prefecture_path_error",
        area_codes = code
      )
    }
  }
  invisible(area_meta)
}

area_ancestors <- function(area_code, area_meta) {
  parent_map <- stats::setNames(area_meta$parent_area_code, area_meta$area_code)
  ancestors <- character()
  current <- unname(parent_map[[area_code]])
  while (!is.na(current)) {
    ancestors <- c(ancestors, current)
    current <- unname(parent_map[[current]])
  }
  ancestors
}

classify_area_codes <- function(area_meta, area) {
  area <- rlang::arg_match(area, c("municipality", "ward"))
  if (!isTRUE(attr(area_meta, "jpops_area_meta_validated", exact = TRUE))) {
    abort_area_meta(
      "`area_meta` must be normalized and validated before classification.",
      "jpops_area_meta_unvalidated_error"
    )
  }

  has_level4_ancestor <- vapply(
    area_meta$area_code,
    function(code) {
      ancestors <- area_ancestors(code, area_meta)
      any(area_meta$area_level[match(ancestors, area_meta$area_code)] == 4L)
    },
    logical(1)
  )
  municipality <- area_meta$area_code[
    (area_meta$area_level == 4L & !has_level4_ancestor) |
      area_meta$area_level == 6L
  ]
  if (area == "municipality") {
    assert_area_frontier(municipality, area_meta)
    return(municipality)
  }

  is_ward <- area_meta$area_level %in%
    c(4L, 5L) &
    area_meta$parent_area_code %in% municipality
  replaced <- unique(area_meta$parent_area_code[is_ward])
  ward <- area_meta$area_code[
    area_meta$area_code %in%
      c(setdiff(municipality, replaced), area_meta$area_code[is_ward])
  ]
  assert_area_frontier(ward, area_meta)
  ward
}

assert_area_frontier <- function(area_codes, area_meta) {
  overlap <- vapply(
    area_codes,
    function(code) any(area_ancestors(code, area_meta) %in% area_codes),
    logical(1)
  )
  if (any(overlap)) {
    abort_area_meta(
      paste0(
        "Area classification is not a hierarchy frontier: ",
        paste(area_codes[overlap], collapse = ", "),
        "."
      ),
      "jpops_area_frontier_error",
      area_codes = area_codes[overlap]
    )
  }
  invisible(area_codes)
}

diagnose_area_additivity <- function(population, area_meta, area, group_vars) {
  area_code <- value <- prefecture_code <- NULL
  selected_value <- prefecture_value <- difference <- NULL
  selected_area_codes <- missing_area_codes <- NULL

  area <- rlang::arg_match(area, c("municipality", "ward"))
  if (!is.character(group_vars) || !all(group_vars %in% names(population))) {
    rlang::abort(
      "`group_vars` must name columns in `population`.",
      class = "jpops_area_additivity_input_error"
    )
  }
  required <- c("area_code", "value")
  if (!all(required %in% names(population))) {
    rlang::abort(
      "`population` must contain `area_code` and `value` columns.",
      class = "jpops_area_additivity_input_error"
    )
  }
  unknown_codes <- unique(population$area_code[
    !population$area_code %in% area_meta$area_code
  ])
  unknown_codes <- unknown_codes[!is.na(unknown_codes)]
  if (length(unknown_codes) > 0L) {
    rlang::abort(
      paste0(
        "Population area codes are missing from metadata: ",
        paste(unknown_codes, collapse = ", "),
        "."
      ),
      class = "jpops_area_additivity_input_error",
      area_codes = unknown_codes
    )
  }

  selected <- classify_area_codes(area_meta, area)
  missing_population <- setdiff(selected, unique(population$area_code))
  parent_map <- stats::setNames(area_meta$parent_area_code, area_meta$area_code)
  level_map <- stats::setNames(area_meta$area_level, area_meta$area_code)
  find_prefecture <- function(code) {
    current <- code
    while (!is.na(current) && unname(level_map[[current]]) != 2L) {
      current <- unname(parent_map[[current]])
    }
    current
  }
  code_to_prefecture <- stats::setNames(
    vapply(area_meta$area_code, find_prefecture, character(1)),
    area_meta$area_code
  )

  population <- dplyr::mutate(
    population,
    prefecture_code = unname(code_to_prefecture[area_code])
  )
  group_columns <- c(group_vars, "prefecture_code")
  selected_summary <- population |>
    dplyr::filter(area_code %in% selected) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_columns))) |>
    dplyr::summarise(
      selected_value = sum(value, na.rm = TRUE),
      selected_area_codes = list(unique(area_code)),
      .groups = "drop"
    )
  prefecture_summary <- population |>
    dplyr::filter(area_code == prefecture_code) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_columns))) |>
    dplyr::summarise(
      prefecture_value = sum(value, na.rm = TRUE),
      .groups = "drop"
    )
  diagnostics <- dplyr::full_join(
    prefecture_summary,
    selected_summary,
    by = group_columns
  ) |>
    dplyr::mutate(
      prefecture_value = dplyr::coalesce(prefecture_value, 0),
      selected_value = dplyr::coalesce(selected_value, 0),
      difference = selected_value - prefecture_value
    )

  diagnostics$missing_area_codes <- lapply(
    diagnostics$prefecture_code,
    function(code) {
      missing_population[code_to_prefecture[missing_population] == code]
    }
  )
  dplyr::select(
    diagnostics,
    dplyr::all_of(group_vars),
    prefecture_code,
    prefecture_value,
    selected_value,
    difference,
    selected_area_codes,
    missing_area_codes
  )
}
