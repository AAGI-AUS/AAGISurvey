test_that("ensure_valid returns NULL for valid values", {
  result <- ensure_valid("S_D", SUPPORT, "support_type")
  expect_null(result)
})

test_that("ensure_valid accepts NULL input", {
  expect_no_error(ensure_valid(NULL, SUPPORT, "support_type"))
})

test_that("ensure_valid throws error for single invalid value", {
  expect_error(
    ensure_valid("INVALID", SUPPORT, "support_type"),
    "Invalid support_type"
  )
})

test_that("ensure_valid catches multiple invalid values", {
  expect_error(
    ensure_valid(c("S_D", "INVALID"), SUPPORT, "support_type"),
    "Invalid support_type"
  )
})

test_that("ensure_valid works with DESIGN dictionary", {
  expect_no_error(ensure_valid("D_SP", DESIGN, "design_type"))
  expect_error(
    ensure_valid("D_INVALID", DESIGN, "design_type"),
    "Invalid design_type"
  )
})

test_that("ensure_valid works with ANALYSIS dictionary", {
  expect_no_error(ensure_valid("A_SP", ANALYSIS, "analysis_type"))
  expect_error(
    ensure_valid("A_INVALID", ANALYSIS, "analysis_type"),
    "Invalid analysis_type"
  )
})

test_that("ensure_valid works with NODE dictionary", {
  expect_no_error(ensure_valid("ANU", NODE, "aagi_node"))
  expect_error(
    ensure_valid("INVALID", NODE, "aagi_node"),
    "Invalid aagi_node"
  )
})

test_that("ensure_valid works with ORG dictionary", {
  expect_no_error(ensure_valid("O_GRO", ORG, "organisation_type"))
  expect_error(
    ensure_valid("O_INVALID", ORG, "organisation_type"),
    "Invalid organisation_type"
  )
})

test_that("ensure_valid handles multiple valid values", {
  expect_no_error(ensure_valid(c("S_D", "S_A"), SUPPORT, "support_type"))
})
