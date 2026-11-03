# ---- Helpers ----

# Stub the clipboard so tests never depend on the OS having one. Returns an
# environment whose `$value` holds whatever copy_to_clip() was given.
stub_clipboard <- function(env = parent.frame()) {
  store <- new.env(parent = emptyenv())
  store$value <- NULL
  testthat::local_mocked_bindings(
    copy_to_clip = function(x) {
      store$value <- x
      invisible(x)
    },
    .env = env
  )
  store
}

# Capture cli output as one string, wide enough that URLs are never wrapped.
cli_out <- function(expr) {
  withr::local_options(cli.width = 1000)
  paste(cli::cli_fmt(expr), collapse = "\n")
}

# Make pick_codes() answer with the first code of whatever dictionary it is
# given, except for the support type, which is chosen by the test.
stub_menu <- function(support = "S_D", env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$titles <- character()
  testthat::local_mocked_bindings(
    pick_codes = function(dict, title) {
      calls$titles <- c(calls$titles, title)
      if (identical(dict, SUPPORT)) support else names(dict)[[1L]]
    },
    .env = env
  )
  calls
}

# ---- Input validation (no prompting, no clipboard) ----

test_that("create_survey_url validates support_type correctly", {
  expect_error(
    create_survey_url(support_type = "INVALID"),
    "Invalid support_type"
  )
})

test_that("create_survey_url validates design_type correctly", {
  expect_error(
    create_survey_url(
      support_type = "S_D",
      design_type = "INVALID",
      aagi_node = "CU",
      organisation_type = "O_GOV"
    ),
    "Invalid design_type"
  )
})

test_that("create_survey_url validates analysis_type correctly", {
  expect_error(
    create_survey_url(
      support_type = "S_A",
      analysis_type = "INVALID",
      aagi_node = "CU",
      organisation_type = "O_GOV"
    ),
    "Invalid analysis_type"
  )
})

test_that("create_survey_url validates aagi_node correctly", {
  expect_error(
    create_survey_url(
      support_type = "S_A",
      analysis_type = "A_SP",
      aagi_node = "INVALID",
      organisation_type = "O_GOV"
    ),
    "Invalid aagi_node"
  )
})

test_that("create_survey_url validates organisation_type correctly", {
  expect_error(
    create_survey_url(
      support_type = "S_A",
      analysis_type = "A_SP",
      aagi_node = "CU",
      organisation_type = "INVALID"
    ),
    "Invalid organisation_type"
  )
})

test_that("create_survey_url rejects an empty-string support_type", {
  expect_error(create_survey_url(support_type = ""), "Invalid support_type")
})

test_that("create_survey_url validates before it prompts", {
  rlang::local_interactive(TRUE)
  testthat::local_mocked_bindings(
    pick_codes = function(...) stop("should not prompt")
  )
  expect_error(create_survey_url(support_type = "INVALID"), "Invalid")
})

test_that("create_survey_url rejects multiple support types", {
  expect_error(
    create_survey_url(
      support_type = c("S_D", "S_A"),
      design_type = "D_SP",
      analysis_type = "A_SP",
      aagi_node = "CU",
      organisation_type = "O_GOV"
    ),
    "Only one support type may be selected."
  )
})

# ---- Non-interactive: missing values must abort ----

test_that("create_survey_url requires design_type for S_D non-interactively", {
  rlang::local_interactive(FALSE)
  expect_error(
    create_survey_url(
      support_type = "S_D",
      aagi_node = "CU",
      organisation_type = "O_GOV"
    ),
    "Missing required value"
  )
})

test_that("create_survey_url requires analysis_type for S_A non-interactively", {
  rlang::local_interactive(FALSE)
  expect_error(
    create_survey_url(
      support_type = "S_A",
      aagi_node = "CU",
      organisation_type = "O_GOV"
    ),
    "Missing required value"
  )
})

test_that("create_survey_url requires support_type non-interactively", {
  rlang::local_interactive(FALSE)
  expect_error(create_survey_url(), "Missing required value")
})

test_that("create_survey_url requires aagi_node non-interactively", {
  rlang::local_interactive(FALSE)
  expect_error(
    create_survey_url(
      support_type = "S_D",
      design_type = "D_SP",
      organisation_type = "O_GOV"
    ),
    "Missing required value"
  )
})

test_that("create_survey_url requires organisation_type non-interactively", {
  rlang::local_interactive(FALSE)
  expect_error(
    create_survey_url(
      support_type = "S_D",
      design_type = "D_SP",
      aagi_node = "CU"
    ),
    "Missing required value"
  )
})

# ---- Happy path (clipboard stubbed, so none of these skip) ----

test_that("create_survey_url returns a URL string", {
  rlang::local_interactive(FALSE)
  stub_clipboard()
  result <- suppressMessages(create_survey_url(
    support_type = "S_A",
    analysis_type = "A_BIO",
    aagi_node = "AU",
    organisation_type = "O_ACA"
  ))
  expect_type(result, "character")
  expect_length(result, 1L)
  expect_true(startsWith(result, paste0(survey_base_url(), "?")))
  expect_match(result, "ST=S_A", fixed = TRUE)
  expect_match(result, "AT=A_BIO", fixed = TRUE)
  expect_match(result, "AN=AU", fixed = TRUE)
  expect_match(result, "OT=O_ACA", fixed = TRUE)
})

test_that("create_survey_url includes correct parameters in URL", {
  rlang::local_interactive(FALSE)
  stub_clipboard()
  result <- suppressMessages(create_survey_url(
    support_type = "S_A",
    analysis_type = "A_SP",
    aagi_node = "CU",
    organisation_type = "O_GOV"
  ))
  expect_match(result, "ST=S_A", fixed = TRUE)
  expect_match(result, "DT=&", fixed = TRUE)
  expect_match(result, "AT=A_SP", fixed = TRUE)
  expect_match(result, "AN=CU", fixed = TRUE)
  expect_match(result, "OT=O_GOV", fixed = TRUE)
})

test_that("create_survey_url with S_D support includes design_type in URL", {
  rlang::local_interactive(FALSE)
  stub_clipboard()
  result <- suppressMessages(create_survey_url(
    support_type = "S_D",
    design_type = "D_GH",
    aagi_node = "UQ",
    organisation_type = "O_ACA"
  ))
  expect_match(result, "ST=S_D", fixed = TRUE)
  expect_match(result, "DT=D_GH", fixed = TRUE)
  expect_match(result, "AT=&", fixed = TRUE)
})

test_that("create_survey_url returns the URL invisibly", {
  rlang::local_interactive(FALSE)
  stub_clipboard()
  res <- withVisible(suppressMessages(create_survey_url(
    "S_D",
    "D_SP",
    NULL,
    "CU",
    "O_ACA"
  )))
  expect_false(res$visible)
})

test_that("create_survey_url copies exactly the URL it returns", {
  rlang::local_interactive(FALSE)
  clip <- stub_clipboard()
  result <- suppressMessages(create_survey_url(
    "S_D",
    "D_SP",
    NULL,
    "CU",
    "O_ACA"
  ))
  expect_identical(clip$value, result)
})

test_that("create_survey_url URL matches build_url() for the same inputs", {
  rlang::local_interactive(FALSE)
  stub_clipboard()
  result <- suppressMessages(create_survey_url(
    "S_D",
    "D_OFE",
    NULL,
    "UWA",
    "O_AGR"
  ))
  expect_identical(
    result,
    build_url(
      support_type = "S_D",
      design_type = "D_OFE",
      analysis_type = NULL,
      aagi_node = "UWA",
      organisation_type = "O_AGR"
    )
  )
})

test_that("create_survey_url accepts every code for each field", {
  rlang::local_interactive(FALSE)
  stub_clipboard()
  for (code in names(DESIGN)) {
    url <- suppressMessages(create_survey_url("S_D", code, NULL, "CU", "O_ACA"))
    expect_match(url, paste0("DT=", code), fixed = TRUE)
  }
  for (code in names(ANALYSIS)) {
    url <- suppressMessages(create_survey_url("S_A", NULL, code, "CU", "O_ACA"))
    expect_match(url, paste0("AT=", code), fixed = TRUE)
  }
  for (code in names(NODE)) {
    url <- suppressMessages(create_survey_url(
      "S_D",
      "D_SP",
      NULL,
      code,
      "O_ACA"
    ))
    expect_match(url, paste0("AN=", code), fixed = TRUE)
  }
  for (code in names(ORG)) {
    url <- suppressMessages(create_survey_url("S_D", "D_SP", NULL, "CU", code))
    expect_match(url, paste0("OT=", code), fixed = TRUE)
  }
})

test_that("create_survey_url prints the summary and guidance", {
  rlang::local_interactive(FALSE)
  stub_clipboard()
  out <- cli_out(create_survey_url("S_D", "D_SP", NULL, "CU", "O_ACA"))
  expect_match(out, "Survey Summary", fixed = TRUE)
  expect_match(out, "Experimental design support", fixed = TRUE)
  expect_match(out, "Small plot trial design", fixed = TRUE)
  expect_match(out, "Analysis type: <blank>", fixed = TRUE)
  expect_match(out, "Curtin University", fixed = TRUE)
  expect_match(out, "Academic institution", fixed = TRUE)
  expect_match(out, "Best practice", fixed = TRUE)
  expect_match(out, "copied to your clipboard", fixed = TRUE)
})

# ---- Interactive prompting ----

test_that("create_survey_url prompts for every missing field for S_D", {
  rlang::local_interactive(TRUE)
  stub_clipboard()
  calls <- stub_menu(support = "S_D")
  url <- suppressMessages(create_survey_url())
  expect_identical(
    calls$titles,
    c(
      "Select support type.",
      "Select design type",
      "Select AAGI node",
      "Select organisation type"
    )
  )
  expect_match(url, "ST=S_D", fixed = TRUE)
  expect_match(url, "DT=D_SP", fixed = TRUE)
  expect_match(url, "AT=&", fixed = TRUE)
})

test_that("create_survey_url prompts for analysis_type for S_A", {
  rlang::local_interactive(TRUE)
  stub_clipboard()
  calls <- stub_menu(support = "S_A")
  url <- suppressMessages(create_survey_url())
  expect_true("Select analysis type" %in% calls$titles)
  expect_false("Select design type" %in% calls$titles)
  expect_match(url, "ST=S_A", fixed = TRUE)
  expect_match(url, "DT=&", fixed = TRUE)
  expect_match(url, "AT=A_SP", fixed = TRUE)
})

test_that("create_survey_url does not prompt for fields it was given", {
  rlang::local_interactive(TRUE)
  stub_clipboard()
  calls <- stub_menu()
  suppressMessages(create_survey_url(
    support_type = "S_D",
    design_type = "D_LAB",
    aagi_node = "CU",
    organisation_type = "O_GOV"
  ))
  expect_identical(calls$titles, character())
})

test_that("create_survey_url only prompts for what is missing", {
  rlang::local_interactive(TRUE)
  stub_clipboard()
  calls <- stub_menu()
  suppressMessages(create_survey_url(
    support_type = "S_D",
    design_type = "D_SP",
    organisation_type = "O_GOV"
  ))
  expect_identical(calls$titles, "Select AAGI node")
})

test_that("create_survey_url aborts if the support menu is cancelled", {
  rlang::local_interactive(TRUE)
  testthat::local_mocked_bindings(pick_codes = function(dict, title) {
    character()
  })
  expect_error(create_survey_url(), "You must select a support type")
})

# ---- resolve_support_type() ----

test_that("resolve_support_type returns a valid single value untouched", {
  testthat::local_mocked_bindings(pick_codes = function(...) stop("no prompt"))
  expect_identical(resolve_support_type("S_A"), "S_A")
  expect_identical(resolve_support_type("S_D"), "S_D")
})

test_that("resolve_support_type errors on more than one value", {
  expect_error(
    resolve_support_type(c("S_D", "S_A")),
    "Only one support type"
  )
})

test_that("resolve_support_type prompts for NULL and for an empty string", {
  testthat::local_mocked_bindings(pick_codes = function(dict, title) "S_A")
  expect_identical(resolve_support_type(NULL), "S_A")
  expect_identical(resolve_support_type(""), "S_A")
})

test_that("resolve_support_type errors if the menu is cancelled", {
  testthat::local_mocked_bindings(pick_codes = function(dict, title) {
    character()
  })
  expect_error(resolve_support_type(NULL), "You must select a support type")
})

# ---- prompt_if_missing() ----

test_that("prompt_if_missing returns a supplied value without prompting", {
  testthat::local_mocked_bindings(pick_codes = function(...) stop("no prompt"))
  expect_identical(prompt_if_missing("CU", NODE, "Select node"), "CU")
})

test_that("prompt_if_missing skips the prompt when not needed", {
  testthat::local_mocked_bindings(pick_codes = function(...) stop("no prompt"))
  expect_null(prompt_if_missing(NULL, DESIGN, "Select", needed = FALSE))
})

test_that("prompt_if_missing prompts when needed and missing", {
  testthat::local_mocked_bindings(pick_codes = function(dict, title) "UWA")
  expect_identical(prompt_if_missing(NULL, NODE, "Select node"), "UWA")
})

# ---- label_for() ----

test_that("label_for returns <blank> for NULL and empty strings", {
  expect_identical(label_for(NULL, DESIGN), "<blank>")
  expect_identical(label_for("", DESIGN), "<blank>")
})

test_that("label_for returns the label for a code", {
  expect_identical(label_for("CU", NODE), "Curtin University")
  expect_identical(label_for("S_A", SUPPORT), "Analysis support")
})

test_that("label_for errors on an unknown code", {
  expect_error(label_for("nope", NODE))
})

# ---- print_survey_summary() ----

test_that("print_survey_summary shows the URL and every selection", {
  out <- cli_out(print_survey_summary(
    "https://example.com/form?ST=S_D",
    "S_D",
    "D_SP",
    NULL,
    "CU",
    "O_ACA"
  ))
  expect_match(out, "https://example.com/form?ST=S_D", fixed = TRUE)
  expect_match(out, "Support type: Experimental design support", fixed = TRUE)
  expect_match(out, "Design type: Small plot trial design", fixed = TRUE)
  expect_match(out, "Analysis type: <blank>", fixed = TRUE)
  expect_match(out, "AAGI node: Curtin University", fixed = TRUE)
  expect_match(out, "Organisation type: Academic institution", fixed = TRUE)
})

test_that("print_survey_summary shows <blank> for an unused design type", {
  out <- cli_out(print_survey_summary(
    "https://example.com",
    "S_A",
    NULL,
    "A_BIO",
    "AU",
    "O_GOV"
  ))
  expect_match(out, "Design type: <blank>", fixed = TRUE)
  expect_match(
    out,
    "Analysis type: Bioinformatics or genetic data",
    fixed = TRUE
  )
})

test_that("print_survey_summary returns NULL invisibly", {
  res <- withVisible(suppressMessages(print_survey_summary(
    "https://example.com",
    "S_D",
    "D_SP",
    NULL,
    "CU",
    "O_ACA"
  )))
  expect_null(res$value)
  expect_false(res$visible)
})

# ---- ensure_valid() ----

test_that("ensure_valid returns NULL for NULL input", {
  expect_null(ensure_valid(NULL, NODE, "aagi_node"))
})

test_that("ensure_valid returns NULL invisibly for valid input", {
  res <- withVisible(ensure_valid("CU", NODE, "aagi_node"))
  expect_null(res$value)
  expect_false(res$visible)
})

test_that("ensure_valid errors and names the field and the bad value", {
  expect_error(ensure_valid("XXX", NODE, "aagi_node"), "Invalid aagi_node")
  expect_error(ensure_valid("XXX", NODE, "aagi_node"), "XXX")
})

test_that("ensure_valid errors if any value in a vector is invalid", {
  expect_error(
    ensure_valid(c("S_D", "nope"), SUPPORT, "support_type"),
    "nope"
  )
})

# ---- pick_codes() (needs the select_one() wrapper, see notes) ----

test_that("pick_codes errors in a non-interactive session", {
  rlang::local_interactive(FALSE)
  expect_error(
    pick_codes(NODE, "Select AAGI node"),
    "non-interactive session"
  )
})

test_that("pick_codes maps the selected label back to its code", {
  rlang::local_interactive(TRUE)
  testthat::local_mocked_bindings(
    select_one = function(choices, title) "Curtin University"
  )
  expect_identical(pick_codes(NODE, "Select AAGI node"), "CU")
})

test_that("pick_codes returns character(0) if the menu is cancelled", {
  rlang::local_interactive(TRUE)
  testthat::local_mocked_bindings(select_one = function(choices, title) "")
  expect_identical(pick_codes(NODE, "Select AAGI node"), character())
})

test_that("pick_codes passes the labels and title to the menu", {
  rlang::local_interactive(TRUE)
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    select_one = function(choices, title) {
      seen$choices <- choices
      seen$title <- title
      choices[[1L]]
    }
  )
  pick_codes(SUPPORT, "Select support type.")
  expect_identical(seen$choices, unname(SUPPORT))
  expect_identical(seen$title, "Select support type.")
})
