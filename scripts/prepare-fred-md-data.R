###############################################################################
### Download and verify the frozen FRED-MD vintage used by the paper
###############################################################################
source("scripts/experiment-script-utils.R")
project_root <- resolve_experiment_project_root()
url <- "https://www.stlouisfed.org/-/media/project/frbstl/stlouisfed/research/fred-md/monthly/2026-06-md.csv"
destination <- file.path(project_root, "data", "current.csv")
expected_md5 <- "8591dd9f169f7aeb45b7c91782fbd947"
dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
tmp <- tempfile(fileext = ".csv")
on.exit(unlink(tmp), add = TRUE)
utils::download.file(url, tmp, mode = "wb", quiet = FALSE)
observed <- unname(as.character(tools::md5sum(tmp)))
if (!identical(observed, expected_md5)) {
  stop("Checksum mismatch. Expected ", expected_md5, ", observed ", observed,
       ". The official file may have changed; do not substitute a different vintage.")
}
if (!file.copy(tmp, destination, overwrite = TRUE)) stop("Could not write ", destination)
cat("PASS: data/current.csv verified (MD5 ", observed, ").\n", sep = "")
