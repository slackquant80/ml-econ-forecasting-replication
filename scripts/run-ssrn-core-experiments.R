###############################################################################
### Run, Validate, Publish, and Freeze the Four SSRN Core FULL Experiments
###############################################################################

source("scripts/experiment-script-utils.R")
project_root <- resolve_experiment_project_root()
source(file.path(project_root, "functions", "core", "func-utils.R"))
source(file.path(project_root, "functions", "core", "func-target-processing.R"))
source(file.path(project_root, "functions", "registry", "target-registry.R"))
source(file.path(project_root, "functions", "experiments", "func-run-manifest.R"))
source(file.path(project_root, "functions", "experiments", "func-experiment-runner.R"))
source(file.path(project_root, "functions", "experiments", "func-ssrn-release.R"))

args <- parse_experiment_cli_args()
publish <- cli_flag(args, "publish", TRUE)
force <- cli_flag(args, "force", FALSE)
freeze <- cli_flag(args, "freeze", TRUE)
base_seed <- suppressWarnings(as.integer(cli_value(args, "seed", "20260716")))
target_argument <- cli_value(args, "targets", NULL)

if (is.na(base_seed) || base_seed < 1L) {
  stop("--seed must be an integer greater than or equal to 1.")
}

registry <- build_target_registry(
  fred_md_file = file.path(project_root, "data", "current.csv")
)
protocol <- ssrn_protocol_specification()
core_targets <- protocol$required_targets

if (!is.null(target_argument) && nzchar(trimws(target_argument))) {
  requested <- trimws(strsplit(target_argument, ",", fixed = TRUE)[[1L]])
  requested <- requested[nzchar(requested)]
  unknown <- setdiff(requested, core_targets)
  if (length(unknown) > 0L) {
    stop("--targets includes non-core targets: ", paste(unknown, collapse = ", "))
  }
  core_targets <- requested
}

core_registry <- registry[match(core_targets, registry$target_code), , drop = FALSE]
if (anyNA(core_registry$target_code)) {
  stop("One or more SSRN core targets are missing from the target registry.")
}
if (any(!core_registry$paper_core)) {
  stop("One or more requested targets are not flagged paper_core.")
}
if (any(!core_registry$eligible_default_run)) {
  failed <- core_registry[!core_registry$eligible_default_run, c(
    "target_code", "eligibility_status"
  ), drop = FALSE]
  print(failed, row.names = FALSE)
  stop("One or more SSRN core targets are not eligible for a FULL run.")
}

run_plan_directory <- file.path(project_root, "results", "ssrn")
dir.create(run_plan_directory, recursive = TRUE, showWarnings = FALSE)
run_plan_id <- paste0("SSRN_CORE_", format(Sys.time(), "%Y%m%d_%H%M%S"))
data_file <- file.path(project_root, "data", "current.csv")
data_md5 <- unname(as.character(tools::md5sum(data_file)))

run_plan <- data.frame(
  plan_id = run_plan_id,
  target_code = core_targets,
  execution_profile = "full",
  base_seed = base_seed,
  data_md5 = data_md5,
  requested_at = format_run_time(),
  status = "queued",
  run_id = NA_character_,
  result_directory = NA_character_,
  stringsAsFactors = FALSE,
  row.names = NULL
)
run_plan_path <- file.path(run_plan_directory, paste0(run_plan_id, ".csv"))
atomic_write_csv(run_plan, run_plan_path)

cat("SSRN FULL run plan: ", run_plan_id, "\n", sep = "")
cat("Targets: ", paste(core_targets, collapse = ", "), "\n", sep = "")
cat("FRED-MD MD5: ", data_md5, "\n", sep = "")

for (i in seq_along(core_targets)) {
  target_code <- core_targets[[i]]
  cat("\nRunning FULL experiment for ", target_code, "...\n", sep = "")
  run_plan$status[[i]] <- "running"
  atomic_write_csv(run_plan, run_plan_path)

  result <- tryCatch(
    run_forecast_experiment(
      project_root = project_root,
      target_code = target_code,
      execution_profile = "full",
      publish = FALSE,
      base_seed = base_seed,
      enable_statistical_validation = TRUE,
      force = force
    ),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    run_plan$status[[i]] <- "failed"
    run_plan$result_directory[[i]] <- conditionMessage(result)
    atomic_write_csv(run_plan, run_plan_path)
    stop("FULL experiment failed for ", target_code, ": ", conditionMessage(result))
  }

  run_plan$status[[i]] <- "validated"
  run_plan$run_id[[i]] <- as.character(result$manifest$run_id)
  run_plan$result_directory[[i]] <- project_relative_path_portable(
    project_root,
    result$result_directory
  )
  atomic_write_csv(run_plan, run_plan_path)

  if (isTRUE(publish)) {
    pointer <- write_published_target_pointer(
      project_root = project_root,
      manifest = result$manifest,
      set_default = identical(target_code, "CPIAUCSL")
    )
    result$manifest <- update_experiment_manifest(
      result$manifest,
      publication_status = if (isTRUE(pointer$is_default)) {
        "published_default"
      } else {
        "published_target"
      }
    )
    run_plan$status[[i]] <- "published"
    atomic_write_csv(run_plan, run_plan_path)
  }
}

if (isTRUE(publish)) {
  published_registry <- read_published_target_registry(
    project_root,
    migrate_legacy = TRUE
  )
  if ("CPIAUCSL" %in% published_registry$target_code) {
    set_default_published_target(project_root, "CPIAUCSL")
  }
  refresh_experiment_index(project_root)

  full_release_requested <- setequal(
    core_targets,
    protocol$required_targets
  )
  if (isTRUE(full_release_requested)) {
    validation <- validate_ssrn_release(project_root = project_root)
    if (!isTRUE(validation$passed)) {
      failed <- validation$validation[!validation$validation$passed, , drop = FALSE]
      print(failed, row.names = FALSE)
      stop("The four FULL runs completed, but SSRN release validation failed.")
    }
    if (isTRUE(freeze)) {
      freeze_ssrn_release(project_root = project_root)
    }
  } else {
    cat(
      "Partial core-target run completed. Release-wide validation and freeze ",
      "will run after all four targets are selected together or via ",
      "scripts/validate-ssrn-release.R and scripts/freeze-ssrn-release.R.\n",
      sep = ""
    )
  }
}

run_plan$status[run_plan$status %in% c("validated", "published")] <- "completed"
atomic_write_csv(run_plan, run_plan_path)
cat("\nPASS: SSRN core FULL experiment workflow completed.\n")
cat("Run plan: ", run_plan_path, "\n", sep = "")
