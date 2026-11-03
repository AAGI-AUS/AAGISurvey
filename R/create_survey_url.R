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
#' @param support_type Character string specifying the type of support being
#' evaluated. You must choose one.
#' Options are:
#'   \describe{
#'     \item{"S_D"}{Experimental design support}
#'     \item{"S_A"}{Analysis support}
#'   }
#'
#' @param design_type Character string specifying the type of experimental
#' design (required if "S_D" is selected for `support_type`). You must choose
#' one.'
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
#'     \item{"ANU"}{Australian National University}
#'     \item{"AU"}{Adelaide University}
#'     \item{"CU"}{Curtin University}
#'     \item{"QDPI"}{Queensland Department of Primary Industries}
#'     \item{"UQ"}{University of Queensland}
#'     \item{"UWA"}{University of Western Australia}
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
#' # - a small plot trial design
#' # - for a government agency or department
#' # - performed by Curtin node
#' create_survey_url(
#'   support_type = "S_D",
#'   design_type = "D_SP",
#'   aagi_node = "CU",
#'   organisation_type = "O_GOV"
#' )
#'
#' # create a survey URL for
#' # - a bioinformatics analysis
#' # - for an academic institution
#' # - performed by Adelaide University node
#' create_survey_url(
#'   support_type = "S_A",
#'   analysis_type = "A_BIO",
#'   aagi_node = "AU",
#'   organisation_type = "O_ACA"
#' )
#' @author Rose Megirian, \email{rose.megirian@@curtin.edu.au} and Adam H.
#'  Sparks, \email{adam.sparks@@curtin.edu.au}
#'
#' @returns Once the relevant information has been provided, the
#' function returns the completed survey URL, prints a short summary of the
#' details you supplied and copies the URL to your operating system clipboard
#' for inclusion in an email or other communication.
#'
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
  support_type <- resolve_support_type(support_type)
  design_type <- prompt_if_missing(
    design_type,
    DESIGN,
    "Select design type",
    needed = "S_D" %in% support_type
  )
  analysis_type <- prompt_if_missing(
    analysis_type,
    ANALYSIS,
    "Select analysis type",
    needed = "S_A" %in% support_type
  )
  aagi_node <- prompt_if_missing(aagi_node, NODE, "Select AAGI node")
  organisation_type <- prompt_if_missing(
    organisation_type,
    ORG,
    "Select organisation type"
  )

  # ---- Build URL ----
  survey_url <- build_url(
    support_type = support_type,
    design_type = design_type,
    analysis_type = analysis_type,
    aagi_node = aagi_node,
    organisation_type = organisation_type
  )

  # ---- Summary ----
  print_survey_summary(
    survey_url,
    support_type,
    design_type,
    analysis_type,
    aagi_node,
    organisation_type
  )
  copy_to_clip(x = survey_url)
  invisible(survey_url)
}

#' Resolve the support type, prompting if it is missing
#'
#' @param support_type Character string or `NULL`.
#'
#' @returns A single support type code.
#' @dev

resolve_support_type <- function(support_type) {
  if (length(support_type) > 1L) {
    cli::cli_abort(c(
      x = "Only one support type may be selected.",
      i = "Valid values are {.val {names(SUPPORT)}}."
    ))
  }
  if (is.null(support_type) || !nzchar(support_type)) {
    support_type <- pick_codes(SUPPORT, "Select support type.")
  }
  if (length(support_type) == 0L) {
    cli::cli_abort(c(x = "You must select a support type."))
  }
  support_type
}

#' Prompt for a code only if it is needed and missing
#'
#' @param value The user supplied value, possibly `NULL`.
#' @param dict Named character vector of allowed values.
#' @param title Character string used as the menu title.
#' @param needed Logical; if `FALSE`, `value` is returned untouched.
#'
#' @returns `value`, or the code the user selected.
#' @dev

prompt_if_missing <- function(value, dict, title, needed = TRUE) {
  if (needed && is.null(value)) {
    return(pick_codes(dict, title))
  }
  value
}

#' Look up a human-readable label for a code
#'
#' @returns The label, or `"<blank>"` if `code` is `NULL` or empty.
#' @dev

label_for <- function(code, dict) {
  if (is.null(code) || !nzchar(code)) {
    return("<blank>")
  }
  dict[[code]]
}

#' Print the survey summary to the console
#'
#' @returns `NULL`, invisibly.
#' @dev

print_survey_summary <- function(
  survey_url,
  support_type,
  design_type,
  analysis_type,
  aagi_node,
  organisation_type
) {
  cli::cli_h1("Survey Summary")
  cli::cli_h2("Survey URL")
  cli::cli_text(survey_url)
  cli::cli_h2("Selections")
  cli::cli_text("Support type: {label_for(support_type, SUPPORT)}")
  cli::cli_text("Design type: {label_for(design_type, DESIGN)}")
  cli::cli_text("Analysis type: {label_for(analysis_type, ANALYSIS)}")
  cli::cli_text("AAGI node: {label_for(aagi_node, NODE)}")
  cli::cli_text("Organisation type: {label_for(organisation_type, ORG)}")
  cli::cli_par()
  cli::cli_text(
    "Best practice: deliver your customised survey link in the same email as the
    output it relates to."
  )
  cli::cli_end()
  cli::cli_par()
  cli::cli_text(
    "The URL has been copied to your clipboard for you to paste in your e-mail
     to your partners."
  )
  cli::cli_end()
  invisible(NULL)
}

#' Pick and match codes from a named character vector
#'
#' Ensures that all necessary values are provided and prompts for a response if
#'  they are not.
#'
#' @returns A character vector of the selected codes.
#' @dev

pick_codes <- function(dict, title) {
  if (isFALSE(rlang::is_interactive())) {
    cli::cli_abort(
      c(x = "Missing required value for {title} in non-interactive session.")
    )
  }
  sel <- select_one(unname(dict), title)
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
      c(x = "Invalid {field}: {bad}")
    )
  }
  return(invisible(NULL))
}

select_one <- function(choices, title) {
  utils::select.list(choices, title = title, multiple = FALSE)
}
