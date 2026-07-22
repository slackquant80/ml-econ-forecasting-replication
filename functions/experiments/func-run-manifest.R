###############################################################################
### Forecast Experiment Run Manifest and Publication Pointer
###############################################################################

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0L) y else x
  }
}

format_run_time <- function(x = Sys.time()) {
  format(as.POSIXct(x), "%Y-%m-%dT%H:%M:%S%z")
}

scalar_manifest_value <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  if (inherits(x, "POSIXt")) return(format_run_time(x[1L]))
  if (inherits(x, "Date")) return(as.character(x[1L]))
  if (length(x) > 1L) return(paste(as.character(x), collapse = "|"))
  if (is.na(x[1L])) return(NA_character_)
  as.character(x[1L])
}

flatten_manifest <- function(manifest) {
  if (!is.list(manifest) || is.null(names(manifest))) {
    stop("manifest는 이름이 있는 list여야 합니다.")
  }
  vapply(manifest, scalar_manifest_value, FUN.VALUE = character(1))
}

atomic_save_rds <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp")
  saveRDS(object, temporary)
  if (file.exists(path)) unlink(path)
  if (!file.rename(temporary, path)) {
    unlink(temporary)
    stop("RDS 파일을 원자적으로 저장하지 못했습니다: ", path)
  }
  invisible(path)
}

atomic_write_csv <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp")
  utils::write.csv(data, temporary, row.names = FALSE, na = "")
  if (file.exists(path)) unlink(path)
  if (!file.rename(temporary, path)) {
    unlink(temporary)
    stop("CSV 파일을 원자적으로 저장하지 못했습니다: ", path)
  }
  invisible(path)
}

new_experiment_manifest <- function(
    run_id,
    target_code,
    target_path_key,
    target_display_name,
    execution_profile,
    run_directory,
    result_directory,
    data_file,
    data_vintage,
    data_md5,
    base_seed,
    forward_forecasts_enabled,
    statistical_validation_enabled,
    source = "experiment_runner"
) {
  now <- format_run_time()

  list(
    manifest_version = "1.1",
    run_id = run_id,
    target_code = target_code,
    target_path_key = target_path_key,
    target_display_name = target_display_name,
    execution_profile = execution_profile,
    status = "queued",
    validation_status = "pending",
    publication_status = "not_published",
    source = source,
    project_root = NA_character_,
    run_directory = normalizePath(run_directory, mustWork = FALSE),
    result_directory = normalizePath(result_directory, mustWork = FALSE),
    result_rds = file.path(
      normalizePath(result_directory, mustWork = FALSE),
      "forecast_project_results.rds"
    ),
    data_file = normalizePath(data_file, mustWork = FALSE),
    data_vintage = as.character(data_vintage),
    data_md5 = as.character(data_md5),
    base_seed = as.integer(base_seed),
    forward_forecasts_enabled = isTRUE(forward_forecasts_enabled),
    statistical_validation_enabled = isTRUE(statistical_validation_enabled),
    created_at = now,
    started_at = NA_character_,
    completed_at = NA_character_,
    failed_at = NA_character_,
    elapsed_seconds = NA_real_,
    error_message = NA_character_,
    validation_message = NA_character_
  )
}

infer_experiment_project_root <- function(run_directory) {
  current <- normalizePath(run_directory, winslash = "/", mustWork = FALSE)
  for (i in seq_len(8L)) {
    if (
      file.exists(file.path(current, "dashboard.Rmd")) &&
        dir.exists(file.path(current, "functions")) &&
        dir.exists(file.path(current, "results"))
    ) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  NA_character_
}

project_relative_path_portable <- function(project_root, path) {
  project_root <- normalize_path_portable(project_root, mustWork = FALSE)
  path <- normalize_path_portable(path, mustWork = FALSE)
  prefix <- paste0(sub("/+$", "", project_root), "/")
  compare_path <- path
  compare_prefix <- prefix
  if (.Platform$OS.type == "windows") {
    compare_path <- tolower(compare_path)
    compare_prefix <- tolower(compare_prefix)
  }
  if (startsWith(compare_path, compare_prefix)) {
    substring(path, nchar(prefix) + 1L)
  } else {
    path
  }
}

rebase_experiment_manifest_paths <- function(manifest, run_directory) {
  if (!is.list(manifest)) return(manifest)

  run_directory <- normalizePath(
    run_directory,
    winslash = "/",
    mustWork = FALSE
  )
  project_root <- infer_experiment_project_root(run_directory)
  result_directory <- file.path(run_directory, "results")
  result_rds <- file.path(result_directory, "forecast_project_results.rds")

  manifest$run_directory <- run_directory
  manifest$result_directory <- normalizePath(
    result_directory,
    winslash = "/",
    mustWork = FALSE
  )
  manifest$result_rds <- normalizePath(
    result_rds,
    winslash = "/",
    mustWork = FALSE
  )

  if (!is.na(project_root)) {
    manifest$project_root <- project_root
    canonical_data <- file.path(project_root, "data", "current.csv")
    stored_data_missing <- (
      !is.character(manifest$data_file) || length(manifest$data_file) != 1L ||
        is.na(manifest$data_file) || !file.exists(manifest$data_file)
    )
    if (stored_data_missing && file.exists(canonical_data)) {
      manifest$data_file <- normalizePath(
        canonical_data,
        winslash = "/",
        mustWork = FALSE
      )
    }
  }

  manifest
}

write_experiment_manifest <- function(manifest, run_directory) {
  if (!is.list(manifest)) stop("manifest는 list여야 합니다.")
  dir.create(run_directory, recursive = TRUE, showWarnings = FALSE)
  manifest <- rebase_experiment_manifest_paths(manifest, run_directory)

  atomic_save_rds(
    manifest,
    file.path(run_directory, "manifest.rds")
  )

  flat <- flatten_manifest(manifest)
  manifest_table <- data.frame(
    field = names(flat),
    value = unname(flat),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  atomic_write_csv(
    manifest_table,
    file.path(run_directory, "manifest.csv")
  )

  invisible(manifest)
}

read_experiment_manifest <- function(run_directory) {
  path <- file.path(run_directory, "manifest.rds")
  if (!file.exists(path)) stop("manifest.rds를 찾을 수 없습니다: ", path)
  manifest <- readRDS(path)
  if (!is.list(manifest)) stop("manifest.rds의 형식이 올바르지 않습니다.")
  rebase_experiment_manifest_paths(manifest, run_directory)
}

update_experiment_manifest <- function(manifest, ..., run_directory = manifest$run_directory) {
  updates <- list(...)
  if (length(updates) > 0L) {
    for (name in names(updates)) manifest[[name]] <- updates[[name]]
  }
  manifest <- write_experiment_manifest(manifest, run_directory)
  manifest
}

published_run_pointer_path <- function(project_root) {
  file.path(project_root, "results", "published_run.rds")
}

published_target_registry_rds_path <- function(project_root) {
  file.path(project_root, "results", "published_targets.rds")
}

published_target_registry_csv_path <- function(project_root) {
  file.path(project_root, "results", "published_targets.csv")
}

published_target_registry_columns <- function() {
  c(
    "pointer_version", "target_code", "target_display_name", "run_id",
    "execution_profile", "result_path", "run_directory", "published_at",
    "validation_status", "is_default"
  )
}

coerce_publication_flag <- function(x) {
  if (is.logical(x)) {
    output <- x
  } else {
    value <- tolower(trimws(as.character(x)))
    output <- rep(NA, length(value))
    output[value %in% c("true", "t", "1", "yes", "y")] <- TRUE
    output[value %in% c("false", "f", "0", "no", "n", "")] <- FALSE
  }
  as.logical(output)
}

empty_published_target_registry <- function() {
  data.frame(
    pointer_version = character(0),
    target_code = character(0),
    target_display_name = character(0),
    run_id = character(0),
    execution_profile = character(0),
    result_path = character(0),
    run_directory = character(0),
    published_at = character(0),
    validation_status = character(0),
    is_default = logical(0),
    stringsAsFactors = FALSE
  )
}

normalize_published_target_registry <- function(registry) {
  required <- published_target_registry_columns()
  if (is.null(registry) || !is.data.frame(registry) || nrow(registry) < 1L) {
    return(empty_published_target_registry())
  }

  for (name in setdiff(required, names(registry))) {
    registry[[name]] <- if (identical(name, "is_default")) FALSE else NA_character_
  }
  registry <- registry[, required, drop = FALSE]
  character_columns <- setdiff(required, "is_default")
  registry[character_columns] <- lapply(registry[character_columns], as.character)
  registry$is_default <- coerce_publication_flag(registry$is_default)
  registry$is_default[is.na(registry$is_default)] <- FALSE
  registry <- registry[
    !is.na(registry$target_code) & nzchar(registry$target_code) &
      !is.na(registry$run_id) & nzchar(registry$run_id),
    ,
    drop = FALSE
  ]
  if (nrow(registry) < 1L) return(empty_published_target_registry())

  registry <- registry[!duplicated(registry$target_code, fromLast = TRUE), , drop = FALSE]
  if (sum(registry$is_default) > 1L) {
    default_rows <- which(registry$is_default)
    registry$is_default[default_rows[-length(default_rows)]] <- FALSE
  }
  registry <- registry[order(registry$target_code), , drop = FALSE]
  rownames(registry) <- NULL
  registry
}

pointer_to_published_target_row <- function(pointer, is_default = FALSE) {
  if (is.null(pointer) || !is.list(pointer)) return(empty_published_target_registry())
  data.frame(
    pointer_version = as.character(pointer$pointer_version %||% "2.0"),
    target_code = as.character(pointer$target_code %||% ""),
    target_display_name = as.character(pointer$target_display_name %||% ""),
    run_id = as.character(pointer$run_id %||% ""),
    execution_profile = as.character(pointer$execution_profile %||% ""),
    result_path = as.character(pointer$result_path %||% ""),
    run_directory = as.character(pointer$run_directory %||% ""),
    published_at = as.character(pointer$published_at %||% ""),
    validation_status = as.character(pointer$validation_status %||% ""),
    is_default = isTRUE(is_default),
    stringsAsFactors = FALSE
  )
}

write_published_target_registry <- function(project_root, registry) {
  registry <- normalize_published_target_registry(registry)
  if (nrow(registry) > 0L && sum(registry$is_default) != 1L) {
    stop("게시 registry에는 기본 target이 정확히 하나 있어야 합니다.")
  }
  if (anyDuplicated(registry$target_code) > 0L) {
    stop("게시 registry에 중복 target이 있습니다.")
  }
  atomic_save_rds(registry, published_target_registry_rds_path(project_root))
  atomic_write_csv(registry, published_target_registry_csv_path(project_root))
  invisible(registry)
}

read_published_target_registry <- function(project_root, migrate_legacy = TRUE) {
  rds_path <- published_target_registry_rds_path(project_root)
  csv_path <- published_target_registry_csv_path(project_root)
  registry <- NULL

  if (file.exists(rds_path)) {
    registry <- tryCatch(readRDS(rds_path), error = function(e) NULL)
  }
  if (is.null(registry) && file.exists(csv_path)) {
    registry <- tryCatch(
      utils::read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
  }
  registry <- normalize_published_target_registry(registry)

  legacy_path <- published_run_pointer_path(project_root)
  legacy <- if (file.exists(legacy_path)) {
    tryCatch(readRDS(legacy_path), error = function(e) NULL)
  } else {
    NULL
  }

  if (nrow(registry) < 1L && isTRUE(migrate_legacy) && !is.null(legacy)) {
    registry <- pointer_to_published_target_row(legacy, is_default = TRUE)
    write_published_target_registry(project_root, registry)
  } else if (nrow(registry) > 0L && !is.null(legacy)) {
    default_match <- registry$run_id == as.character(legacy$run_id %||% "")
    if (any(default_match)) {
      registry$is_default <- FALSE
      registry$is_default[which(default_match)[1L]] <- TRUE
    }
  }

  normalize_published_target_registry(registry)
}

is_absolute_path_portable <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(FALSE)
  }
  grepl("^(?:[A-Za-z]:/|/)", path, perl = TRUE)
}

normalize_path_portable <- function(path, mustWork = FALSE) {
  normalizePath(path, winslash = "/", mustWork = mustWork)
}

resolve_published_result_path <- function(project_root, pointer) {
  if (is.null(pointer) || !is.list(pointer)) return(NA_character_)
  stored_path <- pointer$result_path
  if (
    !is.character(stored_path) || length(stored_path) != 1L ||
      is.na(stored_path) || !nzchar(stored_path)
  ) {
    return(NA_character_)
  }

  stored_path <- gsub("\\", "/", stored_path, fixed = TRUE)
  candidate <- if (is_absolute_path_portable(stored_path)) {
    stored_path
  } else {
    file.path(project_root, stored_path)
  }
  normalize_path_portable(candidate, mustWork = FALSE)
}

build_published_pointer <- function(project_root, manifest) {
  if (!identical(manifest$status, "completed")) {
    stop("완료되지 않은 run은 publish할 수 없습니다.")
  }
  if (!identical(manifest$validation_status, "passed")) {
    stop("검증을 통과하지 않은 run은 publish할 수 없습니다.")
  }
  if (!(manifest$execution_profile %in% c("quick", "full"))) {
    stop("웹 연구결과로 publish할 수 있는 profile은 Quick 또는 Full입니다.")
  }
  if (!file.exists(manifest$result_rds)) {
    stop("publish할 결과 RDS를 찾을 수 없습니다: ", manifest$result_rds)
  }

  project_root <- normalize_path_portable(project_root, mustWork = FALSE)
  result_rds <- normalize_path_portable(manifest$result_rds, mustWork = FALSE)
  project_prefix <- paste0(sub("/+$", "", project_root), "/")

  compare_result <- result_rds
  compare_prefix <- project_prefix
  if (.Platform$OS.type == "windows") {
    compare_result <- tolower(compare_result)
    compare_prefix <- tolower(compare_prefix)
  }

  relative_result <- if (startsWith(compare_result, compare_prefix)) {
    substring(result_rds, nchar(project_prefix) + 1L)
  } else {
    result_rds
  }
  list(
    pointer_version = "2.0",
    run_id = manifest$run_id,
    target_code = manifest$target_code,
    target_display_name = manifest$target_display_name,
    execution_profile = manifest$execution_profile,
    result_path = relative_result,
    run_directory = project_relative_path_portable(
      project_root,
      manifest$run_directory
    ),
    published_at = format_run_time(),
    validation_status = manifest$validation_status
  )
}

write_legacy_published_run_pointer <- function(project_root, pointer) {
  pointer_path <- published_run_pointer_path(project_root)
  atomic_save_rds(pointer, pointer_path)
  flat <- flatten_manifest(pointer)
  atomic_write_csv(
    data.frame(field = names(flat), value = unname(flat), stringsAsFactors = FALSE),
    file.path(project_root, "results", "published_run.csv")
  )
  invisible(pointer)
}

write_published_target_pointer <- function(project_root, manifest, set_default = FALSE) {
  pointer <- build_published_pointer(project_root, manifest)
  registry <- read_published_target_registry(project_root, migrate_legacy = TRUE)
  legacy <- if (file.exists(published_run_pointer_path(project_root))) {
    tryCatch(readRDS(published_run_pointer_path(project_root)), error = function(e) NULL)
  } else {
    NULL
  }

  effective_set_default <- isTRUE(set_default) || is.null(legacy) ||
    identical(as.character(legacy$target_code %||% ""), pointer$target_code)

  registry <- registry[registry$target_code != pointer$target_code, , drop = FALSE]
  if (isTRUE(effective_set_default) && nrow(registry) > 0L) {
    registry$is_default <- FALSE
  }
  registry <- rbind(
    registry,
    pointer_to_published_target_row(pointer, is_default = effective_set_default)
  )
  registry <- write_published_target_registry(project_root, registry)

  if (isTRUE(effective_set_default)) {
    write_legacy_published_run_pointer(project_root, pointer)
  }

  pointer$is_default <- effective_set_default
  pointer
}

write_published_run_pointer <- function(project_root, manifest) {
  write_published_target_pointer(project_root, manifest, set_default = TRUE)
}

set_default_published_target <- function(project_root, target_code) {
  registry <- read_published_target_registry(project_root, migrate_legacy = TRUE)
  matched <- which(registry$target_code == target_code)
  if (length(matched) != 1L) {
    stop("게시된 target을 찾을 수 없습니다: ", target_code)
  }
  registry$is_default <- FALSE
  registry$is_default[matched] <- TRUE
  write_published_target_registry(project_root, registry)
  pointer <- as.list(registry[matched, setdiff(names(registry), "is_default"), drop = FALSE])
  pointer <- lapply(pointer, function(x) x[[1L]])
  write_legacy_published_run_pointer(project_root, pointer)
  invisible(pointer)
}

read_published_run_pointer <- function(project_root) {
  path <- published_run_pointer_path(project_root)
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

refresh_experiment_index <- function(project_root) {
  experiments_root <- file.path(project_root, "results", "experiments")
  dir.create(experiments_root, recursive = TRUE, showWarnings = FALSE)

  manifest_files <- list.files(
    experiments_root,
    recursive = TRUE,
    full.names = TRUE
  )
  manifest_files <- manifest_files[
    basename(manifest_files) == "manifest.rds"
  ]

  published <- read_published_run_pointer(project_root)
  published_id <- if (is.null(published)) NA_character_ else published$run_id
  published_targets <- read_published_target_registry(project_root, migrate_legacy = TRUE)
  published_target_runs <- if (nrow(published_targets) > 0L) {
    stats::setNames(published_targets$run_id, published_targets$target_code)
  } else {
    character(0)
  }

  rows <- lapply(
    manifest_files,
    function(path) {
      run_directory <- dirname(path)
      manifest <- tryCatch(
        read_experiment_manifest(run_directory),
        error = function(e) NULL
      )
      if (is.null(manifest) || !is.list(manifest)) return(NULL)
      flat <- flatten_manifest(manifest)
      data.frame(
        run_id = flat[["run_id"]],
        target_code = flat[["target_code"]],
        target_display_name = flat[["target_display_name"]],
        execution_profile = flat[["execution_profile"]],
        status = flat[["status"]],
        validation_status = flat[["validation_status"]],
        publication_status = if (identical(flat[["run_id"]], published_id)) {
          "published_default"
        } else if (
          flat[["target_code"]] %in% names(published_target_runs) &&
            identical(flat[["run_id"]], unname(published_target_runs[[flat[["target_code"]]]]))
        ) {
          "published_target"
        } else {
          "not_published"
        },
        created_at = flat[["created_at"]],
        started_at = flat[["started_at"]],
        completed_at = flat[["completed_at"]],
        elapsed_seconds = suppressWarnings(as.numeric(flat[["elapsed_seconds"]])),
        run_directory = normalizePath(
          run_directory,
          winslash = "/",
          mustWork = FALSE
        ),
        result_rds = normalizePath(
          file.path(run_directory, "results", "forecast_project_results.rds"),
          winslash = "/",
          mustWork = FALSE
        ),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  )

  rows <- Filter(Negate(is.null), rows)
  index <- if (length(rows) == 0L) {
    data.frame(
      run_id = character(0),
      target_code = character(0),
      target_display_name = character(0),
      execution_profile = character(0),
      status = character(0),
      validation_status = character(0),
      publication_status = character(0),
      created_at = character(0),
      started_at = character(0),
      completed_at = character(0),
      elapsed_seconds = numeric(0),
      run_directory = character(0),
      result_rds = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, rows)
  }

  if (nrow(index) > 0L) {
    index <- index[order(index$created_at, decreasing = TRUE), , drop = FALSE]
    rownames(index) <- NULL
  }

  atomic_write_csv(index, file.path(project_root, "results", "experiment_index.csv"))
  index
}

###############################################################################
### Structured Experiment Progress
###############################################################################

new_experiment_progress <- function(
    run_id,
    status = "queued",
    stage = "queued",
    progress_percent = 0,
    message = "Experiment request created."
) {
  list(
    progress_version = "1.0",
    run_id = run_id,
    status = status,
    stage = stage,
    progress_percent = as.numeric(progress_percent),
    message = as.character(message),
    horizon = NA_integer_,
    forecast_number = NA_integer_,
    forecast_total = NA_integer_,
    updated_at = format_run_time()
  )
}

write_experiment_progress <- function(progress, run_directory) {
  if (!is.list(progress)) stop("progress must be a list.")

  percent <- suppressWarnings(
    as.numeric(if (is.null(progress$progress_percent)) 0 else progress$progress_percent)
  )
  if (length(percent) < 1L || is.na(percent[1L]) || !is.finite(percent[1L])) {
    percent <- 0
  } else {
    percent <- percent[1L]
  }
  progress$progress_percent <- max(0, min(100, percent))
  progress$updated_at <- format_run_time()

  atomic_save_rds(progress, file.path(run_directory, "progress.rds"))
  flat <- flatten_manifest(progress)
  atomic_write_csv(
    data.frame(
      field = names(flat),
      value = unname(flat),
      stringsAsFactors = FALSE,
      row.names = NULL
    ),
    file.path(run_directory, "progress.csv")
  )
  invisible(progress)
}

read_experiment_progress <- function(run_directory) {
  path <- file.path(run_directory, "progress.rds")
  if (!file.exists(path)) return(NULL)
  progress <- readRDS(path)
  if (!is.list(progress)) stop("progress.rds is not a list.")
  progress
}

update_experiment_progress <- function(
    progress,
    ...,
    run_directory
) {
  updates <- list(...)
  if (length(updates) > 0L) {
    for (name in names(updates)) progress[[name]] <- updates[[name]]
  }
  write_experiment_progress(progress, run_directory)
  progress
}
