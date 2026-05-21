#' Create an AAGI Survey URL
#'
#' @description This function is designed to be used each time you need to
#' generate a survey URL for requesting feedback from a partner. It embeds
#' information already known by the team so that respondents do not need to
#' provide it. This includes the type of support provided, the output delivered,
#' the AAGI node that delivered the work, and the organisation type of the
#' partner. These details are included as metadata within the URL so that the
#' survey platform can automatically link each respondents feedback to the
#' relevant contextual information.
#'
#' @details
#' You can use the function in two ways. You can supply all required arguments
#' directly in your script, or you can run the function in interactive mode. In
#' interactive use, the function presents a menu in the console that guides you
#' through each field.
#'
#'
#' @param support_type Character vector specifying the type of support being
#' evaluated. You must choose one.
#' Options are:
#'   \describe{
#'     \item{"S_D"}{Experimental design support}
#'     \item{"S_A"}{Analysis support}
#'   }
#'
#' @param design_type Character string specifying the type of experimental
#' design (required if "S_D" is selected for `support_type`).
#' Options are:
#'   \describe{
#'     \item{"D_SP"}{Small plot trial design}
#'     \item{"D_OFE"}{On farm experiment design}
#'     \item{"D_GH"}{Glasshouse experiment design}
#'     \item{"D_GC"}{Growth chamber experiment design}
#'     \item{"D_LAB"}{Lab experiment design}
#'     \item{"D_OTHER"}{Other design type chosen by the recipient}
#'   }
#'
#' @param analysis_type Character string of analysis type. Required if
#'   "S_A" is selected for `support_type`. Options are:
#'   \describe{
#'     \item{"A_SP"}{Small plot trial data analysis}
#'     \item{"A_OFE"}{On farm experiment data analysis}
#'     \item{"A_PRO"}{Protected environment experiment data analysis
#'       (glasshouse or growth chamber)}
#'     \item{"A_BIO"}{Bioinformatics or genetic data}
#'     \item{"A_BRE"}{Breeding or selection program data}
#'     \item{"A_ENV"}{Environmental or geospatial data}
#'     \item{"A_IMG"}{Imagery data}
#'     \item{"A_REC"}{Farm records or monitoring data}
#'     \item{"A_OTHER"}{Other type to be selected by recipient}
#'   }
#'
#' @param aagi_node Character string of AAGI node. Options are:
#'   \describe{
#'     \item{"CU"}{Curtin University}
#'     \item{"AU"}{Adelaide University}
#'     \item{"UQ"}{University of Queensland}
#'   }
#'
#' @param organisation_type Character string of organisation type. Options are:
#'   \describe{
#'     \item{"O_GRO"}{Grower group, industry association or farm cooperative}
#'     \item{"O_AGR"}{Agronomy or farm advisory practice}
#'     \item{"O_ACA"}{Academic institution}
#'     \item{"O_GOV"}{Government agency or department}
#'     \item{"O_BRE"}{Seed or breeding company}
#'     \item{"O_TEC"}{Technology, biotechnology or chemical company}
#'     \item{"O_OTHER"}{Other organisation type chosen by the recipient}
#'   }
#'
#' @examplesIf interactive()
#' # create a survey URL for
#' # - a small plot trial design & analysis
#' # - for a government agency or department
#' # - performed by CU
#' create_survey_url(
#'   support_type = c("S_D", "S_A"),
#'   design_type = "D_SP",
#'   analysis_type = "A_SP",
#'   aagi_node = "CU",
#'   organisation_type = "O_GOV"
#' )
#'
#' # create a survey URL for
#' # - a bioinformatics analysis
#' # - for an academic institution
#' # - performed by UA
#' create_survey_url(
#'   support_type = "S_A",
#'   analysis_type = "A_BIO",
#'   aagi_node = "UA",
#'   organisation_type = "O_ACA"
#' )
#' @author Rose Megirian, \email{rose.megirian@@curtin.edu.au} and Adam H.
#'  Sparks, \email{adam.sparks@@curtin.edu.au}
#' @returns Once the relevant information has been provided, the
#' function returns the completed survey URL, prints a short summary of the
#' details you supplied and copies the URL to your operating system clipboard
#' for inclusion in an email or other communication.
#' @autoglobal
#' @export

create_survey_url <- function(
  support_type = NULL,
  design_type = NULL,
  analysis_type = NULL,
  aagi_node = NULL,
  organisation_type = NULL
) {
  # ---- Validate inputs ----
  ensure_valid(support_type, SUPPORT, "support_type")
  ensure_valid(design_type, DESIGN, "design_type")
  ensure_valid(analysis_type, ANALYSIS, "analysis_type")
  ensure_valid(aagi_node, NODE, "aagi_node")
  ensure_valid(organisation_type, ORG, "organisation_type")

  # ---- Interactive prompts for missing ----
  if (is.null(support_type) || (!all(nzchar(support_type)))) {
    support_type <- pick_codes(
      SUPPORT,
      "Select support type",
      multiple = FALSE
    )
    if (!all(nzchar(support_type))) {
      cli::cli_abort(c(x = "You must select a support type."))
    }
  }
  if ("S_D" %in% support_type && is.null(design_type)) {
    design_type <- pick_codes(DESIGN, "Select design type")
  }
  if ("S_A" %in% support_type && is.null(analysis_type)) {
    analysis_type <- pick_codes(ANALYSIS, "Select analysis type")
  }
  if (is.null(aagi_node)) {
    aagi_node <- pick_codes(NODE, "Select AAGI node")
  }
  if (is.null(organisation_type)) {
    organisation_type <- pick_codes(ORG, "Select organisation type")
  }

  # ---- Build URL ----
  survey_url <- build_url(
    "https://curtin.au1.qualtrics.com/jfe/form/SV_eXLvfgMz58RktQa",
    support_type,
    design_type,
    analysis_type,
    aagi_node,
    organisation_type
  )

  # ---- Summary ----
  cli::cli_h1("Survey Summary")
  cli::cli_h2("Survey URL")
  cli::cli_text(survey_url)
  cli::cli_h2("Selections")
  cli::cli_par(
    "Support type: {SUPPORT[support_type]}"
  )
  cli::cli_text(
    "Design type: {ifelse(nzchar(design_type), DESIGN[[design_type]],
      '<blank>')}"
  )
  cli::cli_text(
    "Analysis type: {ifelse(nzchar(analysis_type), ANALYSIS[[analysis_type]],
      '<blank>')}"
  )
  cli::cli_text("AAGI node: {NODE[[aagi_node]]}")
  cli::cli_text("Organisation type: {ORG[[organisation_type]]}")
  if (length(support_type) != 1) {
    cli::cli_abort(c(x = "You selected more than one support type."))
  }
  cli::cli_end()
  cli::cli_par()
  cli::cli_text(
    "Best practice: send a survey 7-10 days after each set of outputs rather
     than combining feedback."
  )
  cli::cli_end()
  cli::cli_par()
  cli::cli_text(
    "The URL has been copied to your clipboard for you to paste in your e-mail
     to your partners."
  )
  cli::cli_end()
  copy_to_clip(x = survey_url)
  invisible(survey_url)
}

#' Pick and match codes from a named character vector
#'
#' Ensures that all necessary values are provided and prompts for a response if
#'  they are not.
#'
#' @returns A character vector of the selected codes.
#' @dev

pick_codes <- function(dict, title, multiple = FALSE) {
  if (isFALSE(rlang::is_interactive())) {
    cli::cli_abort(
      "Missing required value for {title} in non-interactive session."
    )
  }
  sel <- utils::select.list(unname(dict), title = title, multiple = multiple)
  if (!any(nzchar(sel))) {
    return(character())
  }
  names(dict)[match(sel, dict)]
}

#' Validate user inputs
#'
#' @param vals Character vector of user inputs.
#' @param allowed Named character vector of allowed values.
#' @param field Character string of field name for error messages.
#'
#' @returns `NULL` invisibly if all values are valid, otherwise throws an
#'  informative error.
#' @dev

ensure_valid <- function(vals, allowed, field) {
  if (is.null(vals)) {
    return()
  }
  bad <- setdiff(vals, names(allowed))
  if (length(bad)) {
    cli::cli_abort(
      "Invalid {field}: {bad}"
    )
  }
  return(invisible(NULL))
}

#' Create the URL string from user provided values
#'
#' Create the URL string from user provided values for returning to the user.
#'
#' @param base Character string of base URL.
#' @param support_type Character vector of support types.
#' @param design_type Character string of design type.
#' @param analysis_type Character string of analysis type.
#' @param aagi_node Character string of AAGI node.
#' @param organisation_type Character string of organisation type.
#'
#' @examples
#' build_url(
#'   "https://curtin.au1.qualtrics.com/jfe/form/SV_eXLvfgMz58RktQa",
#'   "S_D",
#'   "D_SP",
#'   "CU",
#'   "O_ACA"
#' )
#'
#' @dev
#' @returns The full URL string.

build_url <- function(
  base,
  support_type,
  design_type,
  analysis_type,
  aagi_node,
  organisation_type
) {
  params <- list(
    ST = paste(support_type, collapse = ","),
    DT = design_type %||% "",
    AT = analysis_type %||% "",
    AN = aagi_node %||% "",
    OT = organisation_type %||% ""
  )
  query <- paste(
    names(params),
    vapply(params, utils::URLencode, character(1L), reserved = TRUE),
    sep = "=",
    collapse = "&"
  )
  sprintf("%s?%s", base, query)
}
