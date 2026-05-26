test_that("load_registry parses entries", {
  tmp <- withr::local_tempdir()
  write_registry(tmp, SAMPLE_PROJECTS)
  entries <- load_registry(file.path(tmp, "registry.yaml"))
  expect_length(entries, 3)
  expect_equal(entries[[1]]$phenotype_name, "Acute MI Phenotype")
})

test_that("load_registry returns empty list for empty registry", {
  tmp <- withr::local_tempdir()
  write_registry(tmp, list())
  expect_equal(load_registry(file.path(tmp, "registry.yaml")), list())
})

test_that("load_registry errors on malformed YAML", {
  tmp <- withr::local_tempdir()
  writeLines(": : bad yaml [[", file.path(tmp, "registry.yaml"))
  expect_error(load_registry(file.path(tmp, "registry.yaml")), "Malformed")
})

test_that("apply_filters domain", {
  result <- apply_filters(SAMPLE_PROJECTS, domain = "drug")
  expect_length(result, 1)
  expect_equal(result[[1]]$phenotype_name, "Metformin Adherence Study")
})

test_that("apply_filters status", {
  result <- apply_filters(SAMPLE_PROJECTS, status = "draft")
  expect_length(result, 1)
  expect_equal(result[[1]]$phenotype_name, "Type 2 Diabetes Screening")
})

test_that("apply_filters pi substring", {
  result <- apply_filters(SAMPLE_PROJECTS, pi = "alice")
  expect_length(result, 2)
  names_found <- sapply(result, `[[`, "phenotype_name")
  expect_true("Acute MI Phenotype" %in% names_found)
  expect_true("Metformin Adherence Study" %in% names_found)
})

test_that("apply_filters search by phenotype_name", {
  result <- apply_filters(SAMPLE_PROJECTS, search = "diabetes")
  expect_length(result, 1)
  expect_equal(result[[1]]$phenotype_name, "Type 2 Diabetes Screening")
})

test_that("apply_filters search by pi_name", {
  result <- apply_filters(SAMPLE_PROJECTS, search = "bob")
  expect_length(result, 1)
  expect_equal(result[[1]]$pi_name, "Dr. Bob Jones")
})

test_that("apply_filters AND semantics", {
  result <- apply_filters(SAMPLE_PROJECTS, domain = "condition", pi = "alice")
  expect_length(result, 1)
  expect_equal(result[[1]]$phenotype_name, "Acute MI Phenotype")
})

test_that("render_card contains key fields", {
  card <- render_card(SAMPLE_PROJECTS[[1]], 1L, 3L)
  plain <- gsub("\033\\[[0-9;]*m", "", card)
  expect_true(grepl("Acute MI Phenotype", plain))
  expect_true(grepl("Dr. Alice Smith", plain))
  expect_true(grepl("1 / 3", plain))
  expect_true(grepl("internal_validated", plain))
  expect_true(grepl("SCE tier 3", plain))
  expect_true(grepl("domain: condition", plain))
})

test_that("crio_list no_pager shows all names", {
  tmp <- withr::local_tempdir()
  write_registry(tmp, SAMPLE_PROJECTS)
  withr::with_envvar(c(CRIO_LIBRARY_DIR = tmp), {
    out <- capture.output(crio_list(no_pager = TRUE))
    combined <- paste(out, collapse = "\n")
    expect_true(grepl("Acute MI Phenotype", combined))
    expect_true(grepl("Type 2 Diabetes Screening", combined))
    expect_true(grepl("Metformin Adherence Study", combined))
  })
})

test_that("crio_list no_pager domain filter", {
  tmp <- withr::local_tempdir()
  write_registry(tmp, SAMPLE_PROJECTS)
  withr::with_envvar(c(CRIO_LIBRARY_DIR = tmp), {
    out <- capture.output(crio_list(domain = "drug", no_pager = TRUE))
    combined <- paste(out, collapse = "\n")
    expect_true(grepl("Metformin Adherence Study", combined))
    expect_false(grepl("Acute MI Phenotype", combined))
  })
})

test_that("crio_list empty registry message", {
  tmp <- withr::local_tempdir()
  write_registry(tmp, list())
  withr::with_envvar(c(CRIO_LIBRARY_DIR = tmp), {
    out <- capture.output(crio_list(no_pager = TRUE))
    combined <- paste(out, collapse = "\n")
    expect_true(grepl("No phenotypes", combined))
  })
})

test_that("crio_list no results after filter", {
  tmp <- withr::local_tempdir()
  write_registry(tmp, SAMPLE_PROJECTS)
  withr::with_envvar(c(CRIO_LIBRARY_DIR = tmp), {
    out <- capture.output(crio_list(domain = "observation", no_pager = TRUE))
    combined <- paste(out, collapse = "\n")
    expect_true(grepl("No phenotypes match", combined))
  })
})

test_that("crio_list errors when registry not found", {
  withr::with_envvar(c(CRIO_LIBRARY_DIR = "/nonexistent/path"), {
    expect_error(crio_list(no_pager = TRUE), "Registry not found")
  })
})

test_that("find_registry respects CRIO_LIBRARY_DIR", {
  tmp <- withr::local_tempdir()
  writeLines("projects: []", file.path(tmp, "registry.yaml"))
  withr::with_envvar(c(CRIO_LIBRARY_DIR = tmp), {
    path <- find_registry()
    expect_false(is.null(path))
    expect_true(file.exists(path))
  })
})

test_that("find_registry returns NULL when env dir has no registry", {
  withr::with_envvar(c(CRIO_LIBRARY_DIR = "/nonexistent"), {
    expect_null(find_registry())
  })
})
