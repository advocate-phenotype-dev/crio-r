#' @importFrom cli col_green col_yellow col_red col_cyan style_bold style_dim
NULL

`%||%` <- function(a, b) if (!is.null(a)) a else b

.crio_now <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%S+00:00")
}

# Consume one entry from the test-responses option, or call readline().
.crio_prompt <- function(label, default = NULL) {
  test_q <- getOption("crio.test_responses")
  if (!is.null(test_q) && length(test_q) > 0) {
    resp <- test_q[[1]]
    options(crio.test_responses = test_q[-1])
    return(as.character(resp))
  }
  suffix <- if (!is.null(default)) paste0(" [", default, "]") else ""
  resp <- readline(paste0(label, suffix, ": "))
  if (nchar(trimws(resp)) == 0 && !is.null(default)) return(as.character(default))
  resp
}

.crio_confirm <- function(label, default = TRUE) {
  test_q <- getOption("crio.test_responses")
  if (!is.null(test_q) && length(test_q) > 0) {
    resp <- test_q[[1]]
    options(crio.test_responses = test_q[-1])
    if (is.logical(resp)) return(resp)
    return(tolower(as.character(resp)) %in% c("y", "yes", "true"))
  }
  hint <- if (default) "[Y/n]" else "[y/N]"
  resp <- tolower(trimws(readline(paste0(label, " ", hint, ": "))))
  if (nchar(resp) == 0) return(default)
  resp %in% c("y", "yes")
}

.crio_choose <- function(label, choices) {
  test_q <- getOption("crio.test_responses")
  if (!is.null(test_q) && length(test_q) > 0) {
    resp <- test_q[[1]]
    options(crio.test_responses = test_q[-1])
    if (is.numeric(resp)) return(choices[as.integer(resp)])
    return(as.character(resp))
  }
  cat(paste0(label, "\n"))
  for (i in seq_along(choices)) cat(sprintf("  %d. %s\n", i, choices[i]))
  repeat {
    n <- suppressWarnings(as.integer(readline("Choice: ")))
    if (!is.na(n) && n >= 1 && n <= length(choices)) return(choices[n])
    cat("Invalid — enter a number 1-", length(choices), "\n", sep = "")
  }
}

# Returns list(name, email) from an active `az` session, or NULL if unavailable.
.az_identity <- function() {
  if (nchar(Sys.which("az")) == 0) return(NULL)
  tryCatch({
    out <- suppressWarnings(system2(
      "az",
      c("ad", "signed-in-user", "show",
        "--query", "{name:displayName,email:mail,upn:userPrincipalName}",
        "--output", "json"),
      stdout = TRUE, stderr = FALSE
    ))
    parsed <- jsonlite::fromJSON(paste(out, collapse = "\n"))
    list(
      name  = parsed$name,
      email = if (!is.null(parsed$email) && nchar(parsed$email) > 0)
                parsed$email else parsed$upn
    )
  }, error = function(e) NULL)
}

.write_schema <- function(schema, path) {
  yaml_str <- yaml::as.yaml(schema, indent = 2)
  yaml_str <- gsub(":\\s*\\{\\}", ": []", yaml_str)
  writeLines(yaml_str, con = path)
}

.status_color <- function(status) {
  switch(status,
    internal_validated = ,
    peer_reviewed      = cli::col_green(status),
    draft              = cli::col_yellow(status),
    deprecated         = cli::col_red(status),
    status
  )
}
