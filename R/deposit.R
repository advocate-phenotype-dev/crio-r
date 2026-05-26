#' Submit a phenotype for deposit to the clearinghouse
#'
#' Validates the schema, checks deposit eligibility, and marks the phenotype
#' as submitted (sandbox mode only; live endpoint not yet configured).
#'
#' @param project_dir Path to the project directory. Defaults to `getwd()`.
#' @param sandbox     If TRUE (default), simulate submission without hitting API.
#' @return Invisibly returns TRUE on success, FALSE on failure.
#' @export
crio_deposit <- function(project_dir = NULL, sandbox = TRUE) {
  project_dir <- project_dir %||% getwd()
  schema_path <- file.path(project_dir, "advocate-phenotype.yaml")

  if (!file.exists(schema_path)) {
    cat(cli::col_red("Deposit aborted — advocate-phenotype.yaml not found.\n"))
    return(invisible(FALSE))
  }

  raw    <- yaml::read_yaml(schema_path)
  result <- .validate_schema(raw)

  if (!result$valid) {
    cat(cli::col_red("Deposit aborted — schema invalid:\n"))
    for (e in result$errors) cat(" ", e, "\n")
    return(invisible(FALSE))
  }

  schema <- result$schema
  contr  <- schema$contribution %||% list()
  phen   <- schema$phenotype    %||% list()
  inv    <- schema$investigator  %||% list()

  if (!isTRUE(contr$deposit_eligible)) {
    cat("Deposit not eligible.\n")
    cat("  Phenotype must be at least internal_validated status\n")
    cat("  and have description, inclusion, and exclusion criteria populated.\n")
    return(invisible(FALSE))
  }

  if (isTRUE(contr$deposit_submitted)) {
    cat("Deposit already submitted.\n")
    return(invisible(FALSE))
  }

  if (!sandbox) {
    stop("Clearinghouse API endpoint not yet configured. Use sandbox = TRUE.")
  }

  now <- .crio_now()
  raw$contribution$deposit_submitted <- TRUE
  raw$contribution$deposit_date      <- now
  raw$project$updated                <- now

  .write_schema(raw, schema_path)

  cat(cli::col_green("✓"), "Deposit submitted (sandbox)\n")
  cat("  Phenotype: ", phen$name %||% "—", "\n", sep = "")
  cat("  Version:   ", phen$version %||% "—", "\n", sep = "")
  cat("  PI ORCID:  ", inv$pi_orcid %||% "—", "\n", sep = "")
  cat("  Clearinghouse notified: [sandbox mock]\n")

  invisible(TRUE)
}
