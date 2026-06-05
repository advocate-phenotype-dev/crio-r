ADVOCATE_DIR <- ".advocate"
SESSION_LOCK <- ".advocate/session.lock"

SANDBOX_CREDENTIALS <- list(
  mode            = "sandbox",
  sce_tier        = NULL,
  environment     = "local",
  library_remote  = "https://github.com/advocate-phenotype-dev/phenotype-library.git",
  token           = "sandbox-mock-token",
  issued_at       = NULL,
  expires_at      = NULL
)

.find_project_root <- function(start = NULL) {
  current <- normalizePath(start %||% getwd(), mustWork = FALSE)
  candidates <- c(current, normalizePath(
    sapply(seq_len(20), function(i) {
      p <- current
      for (j in seq_len(i)) p <- dirname(p)
      p
    }), mustWork = FALSE
  ))
  for (p in unique(candidates)) {
    if (file.exists(file.path(p, "advocate-phenotype.yaml"))) return(p)
    if (p == dirname(p)) break
  }
  stop(
    "No advocate-phenotype.yaml found in ", start %||% getwd(), " or any parent directory. ",
    "Run crio_init() to create a project, then set the working directory."
  )
}

#' Start a crio session for a phenotype project
#'
#' Locates the project root, validates the schema, writes a session lock file,
#' and sets environment variables for downstream tooling.
#'
#' @param project_dir Path to the project directory (or any subdirectory).
#'   Defaults to `getwd()`.
#' @param sandbox     If TRUE (default), use sandbox credentials.
#' @return Invisibly returns the session list.
#' @export
crio_source <- function(project_dir = NULL, sandbox = TRUE) {
  project_dir  <- .find_project_root(project_dir)
  advocate_dir <- file.path(project_dir, ADVOCATE_DIR)
  dir.create(advocate_dir, showWarnings = FALSE)

  schema_path <- file.path(project_dir, "advocate-phenotype.yaml")
  raw         <- yaml::read_yaml(schema_path)
  result      <- .validate_schema(raw)

  if (!result$valid) {
    cat(cli::col_red("Session not started — schema invalid:\n"))
    for (e in result$errors) cat(" ", e, "\n")
    return(invisible(list()))
  }

  schema <- result$schema
  phen   <- schema$phenotype    %||% list()
  comp   <- schema$compute      %||% list()
  inv    <- schema$investigator  %||% list()
  proj   <- schema$project      %||% list()
  now    <- .crio_now()

  # TRE portal stub for SCE tier 4+
  if (!sandbox && !is.null(comp$sce_tier) && comp$sce_tier >= 4) {
    project_id <- as.character(proj$id)
    portal_url <- paste0(
      "https://tre.advocatehealth.org/projects/", project_id,
      "?source=crio&version=", phen$version
    )
    cat("\n⚠  SCE tier 4+ detected.\n")
    cat("   Full TRE portal integration is a planned feature.\n")
    cat("   When live, your browser will open to:", portal_url, "\n")
    cat("   For now: open the TRE portal manually and source from within.\n\n")
  }

  if (sandbox) {
    session <- modifyList(SANDBOX_CREDENTIALS, list(
      issued_at   = now,
      sce_tier    = comp$sce_tier,
      environment = comp$environment %||% "local",
      project_id  = as.character(proj$id),
      pi_orcid    = inv$pi_orcid %||% ""
    ))
  } else {
    az_tok <- .az_token()
    if (is.null(az_tok)) {
      stop("No active Azure CLI session. Run `az login` and try again.")
    }
    session <- list(
      mode           = "production",
      sce_tier       = comp$sce_tier,
      environment    = comp$environment %||% "local",
      library_remote = SANDBOX_CREDENTIALS$library_remote,
      token          = az_tok$token,
      issued_at      = now,
      expires_at     = az_tok$expires_at,
      project_id     = as.character(proj$id),
      pi_orcid       = inv$pi_orcid %||% ""
    )
  }

  lock_path <- file.path(project_dir, SESSION_LOCK)
  writeLines(jsonlite::toJSON(session, auto_unbox = TRUE, null = "null", pretty = TRUE),
             lock_path)

  Sys.setenv(
    CRIO_PROJECT_ID  = session$project_id,
    CRIO_SCE_TIER    = as.character(session$sce_tier),
    CRIO_ENVIRONMENT = session$environment,
    CRIO_SANDBOX     = if (sandbox) "true" else "false",
    CRIO_PI_ORCID    = session$pi_orcid
  )

  cat(cli::col_green("✓"), "Session started\n")
  cat("  Project:    ", phen$name %||% "—", "\n", sep = "")
  cat("  SCE tier:   ", as.character(session$sce_tier), "\n", sep = "")
  cat("  Environment:", session$environment, "\n")
  cat("  Mode:       ", session$mode, "\n", sep = "")

  invisible(session)
}
