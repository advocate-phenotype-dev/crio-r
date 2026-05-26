#' Validate an advocate-phenotype.yaml schema
#'
#' Reads advocate-phenotype.yaml from the given directory, runs all schema
#' validations, and prints a human-readable report.
#'
#' @param project_dir Path to the project directory. Defaults to `getwd()`.
#' @return Invisibly returns a list with `$valid` (logical), `$errors`
#'   (character vector), and `$schema` (raw list on success, NULL on failure).
#' @export
crio_validate <- function(project_dir = NULL) {
  project_dir <- project_dir %||% getwd()
  schema_path <- file.path(project_dir, "advocate-phenotype.yaml")

  if (!file.exists(schema_path)) {
    result <- list(
      valid  = FALSE,
      errors = "[advocate-phenotype.yaml] File not found",
      schema = NULL
    )
    cat(cli::col_red("✗"), "Schema invalid — 1 error:\n")
    cat("  1.", result$errors, "\n")
    return(invisible(result))
  }

  raw    <- yaml::read_yaml(schema_path)
  result <- .validate_schema(raw)

  if (result$valid) {
    phen <- raw$phenotype %||% list()
    comp <- raw$compute   %||% list()
    cont <- raw$contribution %||% list()
    cat(cli::col_green("✓"), "Schema valid\n")
    cat("  Phenotype:         ", phen$name %||% "—", "\n", sep = "")
    cat("  Version:           ", phen$version %||% "—", "\n", sep = "")
    cat("  Validation status: ", phen$validation_status %||% "—", "\n", sep = "")
    cat("  Deposit eligible:  ", as.character(cont$deposit_eligible %||% FALSE), "\n", sep = "")
    cat("  SCE tier:          ", as.character(comp$sce_tier %||% "—"), "\n", sep = "")
  } else {
    n <- length(result$errors)
    cat(cli::col_red("✗"), "Schema invalid —", n, "error(s):\n")
    for (i in seq_along(result$errors)) {
      cat(" ", i, ".", result$errors[i], "\n")
    }
  }

  invisible(result)
}
