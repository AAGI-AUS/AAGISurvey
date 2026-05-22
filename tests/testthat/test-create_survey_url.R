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

test_that("create_survey_url requires design_type when support_type is S_D in non-interactive", {
  skip_if(rlang::is_interactive(), "Interactive mode enabled")
  expect_error(
    create_survey_url(
      support_type = "S_D",
      aagi_node = "CU",
      organisation_type = "O_GOV"
    ),
    "Missing required value"
  )
})

test_that("create_survey_url requires analysis_type when support_type is S_A in non-interactive", {
  skip_if(rlang::is_interactive(), "Interactive mode enabled")
  expect_error(
    create_survey_url(
      support_type = "S_A",
      aagi_node = "CU",
      organisation_type = "O_GOV"
    ),
    "Missing required value"
  )
})

test_that("create_survey_url rejects multiple support types", {
  skip_if(rlang::is_interactive(), "Interactive mode enabled")
  skip_if(!is_clipr_available, skip_msg)
  expect_error(
    local({
      capture_output(
        create_survey_url(
          support_type = c("S_D", "S_A"),
          design_type = "D_SP",
          analysis_type = "A_SP",
          aagi_node = "CU",
          organisation_type = "O_GOV"
        )
      )
    }),
    "You selected more than one support type"
  )
})

test_that("create_survey_url returns a URL string", {
  skip_if(rlang::is_interactive(), "Interactive mode enabled")
  skip_if(!is_clipr_available, skip_msg)
  result <- local({
    capture_output(
      result <- create_survey_url(
        support_type = "S_A",
        analysis_type = "A_BIO",
        aagi_node = "AU",
        organisation_type = "O_ACA"
      )
    )
    result
  })
  expect_match(result, "https://curtin.au1.qualtrics.com")
  expect_match(result, "ST=S_A")
  expect_match(result, "AT=A_BIO")
  expect_match(result, "AN=AU")
  expect_match(result, "OT=O_ACA")
})

test_that("create_survey_url includes correct parameters in URL", {
  skip_if(rlang::is_interactive(), "Interactive mode enabled")
  skip_if(!is_clipr_available, skip_msg)
  result <- local({
    capture_output(
      result <- create_survey_url(
        support_type = "S_A",
        analysis_type = "A_SP",
        aagi_node = "CU",
        organisation_type = "O_GOV"
      )
    )
    result
  })
  expect_match(result, "ST=S_A")
  expect_match(result, "AT=A_SP")
  expect_match(result, "AN=CU")
  expect_match(result, "OT=O_GOV")
})

test_that("create_survey_url with S_D support includes design_type in URL", {
  skip_if(rlang::is_interactive(), "Interactive mode enabled")
  skip_if(!is_clipr_available, skip_msg)
  result <- local({
    capture_output(
      result <- create_survey_url(
        support_type = "S_D",
        design_type = "D_GH",
        aagi_node = "UQ",
        organisation_type = "O_ACA"
      )
    )
    result
  })
  expect_match(result, "DT=D_GH")
})
