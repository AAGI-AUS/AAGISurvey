test_that("build_url constructs correct URL structure", {
  url <- build_url(
    "https://example.com/form",
    "S_D",
    "D_SP",
    "A_SP",
    "CU",
    "O_ACA"
  )
  expect_match(url, "^https://example.com/form\\?")
})

test_that("build_url includes all parameters", {
  url <- build_url(
    "https://example.com/form",
    "S_A",
    "D_SP",
    "A_BIO",
    "ANU",
    "O_GOV"
  )
  expect_match(url, "ST=S_A")
  expect_match(url, "DT=D_SP")
  expect_match(url, "AT=A_BIO")
  expect_match(url, "AN=ANU")
  expect_match(url, "OT=O_GOV")
})

test_that("build_url handles empty parameters", {
  url <- build_url(
    "https://example.com/form",
    "S_A",
    "",
    "A_SP",
    "UQ",
    "O_AGR"
  )
  expect_match(url, "DT=&")
})

test_that("build_url URL-encodes parameters", {
  url <- build_url(
    "https://example.com/form",
    "S_D",
    "D_SP",
    "A_SP",
    "CU",
    "O_ACA"
  )
  expect_false(grepl(" ", url, fixed = TRUE))
})

test_that("build_url joins multiple support types with comma", {
  url <- build_url(
    "https://example.com/form",
    c("S_D", "S_A"),
    "D_SP",
    "A_SP",
    "CU",
    "O_ACA"
  )
  expect_match(url, "ST=S_D%2CS_A")
})

test_that("build_url returns full URL with base and query string", {
  url <- build_url(
    "https://example.com/form",
    "S_D",
    "D_SP",
    "A_SP",
    "CU",
    "O_ACA"
  )
  expect_true(startsWith(url, "https://example.com/form?"))
  expect_true(grepl("ST=", url, fixed = TRUE))
})
