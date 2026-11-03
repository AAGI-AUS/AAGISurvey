# ---- Helpers ----
# Build a URL with sensible defaults so each test only states what it changes.
make_url <- function(
  support_type = "S_D",
  design_type = "D_SP",
  analysis_type = "A_SP",
  aagi_node = "CU",
  organisation_type = "O_ACA",
  ...
) {
  build_url(
    support_type = support_type,
    design_type = design_type,
    analysis_type = analysis_type,
    aagi_node = aagi_node,
    organisation_type = organisation_type,
    ...
  )
}

# ---- Structure ----
test_that("build_url includes all parameters", {
  url <- build_url("S_A", "D_SP", "A_BIO", "ANU", "O_GOV")
  expect_match(url, "ST=S_A", fixed = TRUE)
  expect_match(url, "DT=D_SP", fixed = TRUE)
  expect_match(url, "AT=A_BIO", fixed = TRUE)
  expect_match(url, "AN=ANU", fixed = TRUE)
  expect_match(url, "OT=O_GOV", fixed = TRUE)
})

test_that("build_url returns a single string", {
  url <- make_url()
  expect_type(url, "character")
  expect_length(url, 1L)
})

test_that("build_url returns full URL with base and query string", {
  url <- make_url(base = "https://example.com/form")
  expect_identical(
    url,
    "https://example.com/form?ST=S_D&DT=D_SP&AT=A_SP&AN=CU&OT=O_ACA"
  )
})

test_that("build_url emits parameters in a fixed order", {
  url <- make_url()
  query <- sub("^[^?]*\\?", "", url)
  keys <- sub("=.*$", "", strsplit(query, "&", fixed = TRUE)[[1L]])
  expect_identical(keys, c("ST", "DT", "AT", "AN", "OT"))
})

test_that("build_url has exactly one '?' and four '&' separators", {
  url <- make_url()
  expect_identical(
    lengths(regmatches(url, gregexpr("?", url, fixed = TRUE))),
    1L
  )
  expect_identical(
    lengths(regmatches(url, gregexpr("&", url, fixed = TRUE))),
    4L
  )
})

# ---- Base URL ----
test_that("build_url uses survey_base_url() by default", {
  url <- make_url()
  expect_true(startsWith(url, paste0(survey_base_url(), "?")))
})

test_that("build_url honours a custom base", {
  url <- make_url(base = "https://example.com/other")
  expect_true(startsWith(url, "https://example.com/other?"))
  expect_false(grepl(survey_base_url(), url, fixed = TRUE))
})

test_that("build_url accepts base positionally in last position", {
  url <- build_url(
    "S_D",
    "D_SP",
    "A_SP",
    "CU",
    "O_ACA",
    "https://example.com/form"
  )
  expect_true(startsWith(url, "https://example.com/form?"))
})

test_that("survey_base_url() returns a single https URL", {
  base <- survey_base_url()
  expect_type(base, "character")
  expect_length(base, 1L)
  expect_match(base, "^https://")
  expect_false(grepl("[?&]", base))
})

test_that("survey_base_url() matches the internal constant", {
  expect_identical(survey_base_url(), SURVEY_BASE_URL)
})

# ---- Empty and NULL parameters ----
test_that("build_url handles empty parameters", {
  url <- build_url("S_A", "", "A_SP", "UQ", "O_AGR")
  expect_match(url, "DT=&", fixed = TRUE)
})

test_that("build_url turns NULL optional parameters into empty strings", {
  url <- make_url(
    support_type = "S_A",
    design_type = NULL,
    analysis_type = "A_BIO"
  )
  expect_match(url, "DT=&", fixed = TRUE)

  url <- make_url(analysis_type = NULL)
  expect_match(url, "AT=&", fixed = TRUE)
})

test_that("build_url turns NULL node and organisation into empty strings", {
  url <- make_url(aagi_node = NULL, organisation_type = NULL)
  expect_match(url, "AN=&", fixed = TRUE)
  expect_match(url, "OT=$")
})

test_that("build_url handles all-NULL optional parameters", {
  url <- build_url(
    support_type = "S_D",
    design_type = NULL,
    analysis_type = NULL,
    aagi_node = NULL,
    organisation_type = NULL,
    base = "https://example.com/form"
  )
  expect_identical(url, "https://example.com/form?ST=S_D&DT=&AT=&AN=&OT=")
})

test_that("build_url gives an empty ST for NULL support_type", {
  url <- make_url(support_type = NULL)
  expect_match(url, "ST=&", fixed = TRUE)
})

# ---- Encoding ----
test_that("build_url URL-encodes reserved characters", {
  url <- make_url(design_type = "a b&c=d")
  expect_false(grepl(" ", url, fixed = TRUE))
  expect_match(url, "DT=a%20b%26c%3Dd", fixed = TRUE)
})

test_that("build_url encodes '?' and '#' in values", {
  url <- make_url(analysis_type = "x?y#z")
  expect_match(url, "AT=x%3Fy%23z", fixed = TRUE)
})

test_that("build_url leaves the underscores in codes unencoded", {
  url <- make_url()
  expect_match(url, "ST=S_D", fixed = TRUE)
  expect_false(grepl("%5F", url, fixed = TRUE))
})

# ---- Multiple values ----
test_that("build_url joins multiple support types with comma", {
  url <- build_url(c("S_D", "S_A"), "D_SP", "A_SP", "CU", "O_ACA")
  expect_match(url, "ST=S_D%2CS_A", fixed = TRUE)
})

test_that("build_url keeps the order of multiple support types", {
  url <- make_url(support_type = c("S_A", "S_D"))
  expect_match(url, "ST=S_A%2CS_D", fixed = TRUE)
})

test_that("build_url errors on a vector for a non-support parameter", {
  expect_error(make_url(design_type = c("D_SP", "D_GH")))
  expect_error(make_url(aagi_node = c("CU", "UWA")))
})

# ---- Argument matching ----
test_that("build_url gives the same result for positional and named calls", {
  positional <- build_url("S_A", "D_SP", "A_BIO", "ANU", "O_GOV")
  named <- build_url(
    support_type = "S_A",
    design_type = "D_SP",
    analysis_type = "A_BIO",
    aagi_node = "ANU",
    organisation_type = "O_GOV"
  )
  expect_identical(positional, named)
})

# ---- Every code in the constants ----
test_that("build_url accepts every documented code", {
  for (code in names(DESIGN)) {
    expect_match(
      make_url(design_type = code),
      paste0("DT=", code),
      fixed = TRUE
    )
  }
  for (code in names(ANALYSIS)) {
    expect_match(
      make_url(analysis_type = code),
      paste0("AT=", code),
      fixed = TRUE
    )
  }
  for (code in names(NODE)) {
    expect_match(make_url(aagi_node = code), paste0("AN=", code), fixed = TRUE)
  }
  for (code in names(ORG)) {
    expect_match(
      make_url(organisation_type = code),
      paste0("OT=", code),
      fixed = TRUE
    )
  }
})
