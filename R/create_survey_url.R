#' Create an AAGI Survey URL
#'
#' An interactive menu-driven function to generate a URL for sharing a survey
#' with AAGI partners. You can either supply all the necessary details and
#' script it or use interactively and run the function answering a menu-driven
#' set of questions to generate the URL.
#'
#' @param support_type Character vector of support types. Options are:
#'  \describe{
#'    \item{S_D}{Experimental design support}
#'    \item{S_A}{Analysis support}.
#'    }
#' You may choose both.
#' @param design_type Character string of design type. Required if "S_D" is
#'  selected for `support_type`. Options are:
#'  \describe{
#'      \item{"D_SP"}{Small plot trial design},
#'      \item{"D_OFE"}{On farm experiment design},
#'      \item{"D_GH"}{Glasshouse experiment design},
#'      \item{"D_GC"}{Growth chamber experiment design},
#'      \item{"D_LAB"}{Lab experiment design},
#'      \item{"D_OTHER"}{Other type to be selected by recipient}.
#'   }
#' @param analysis_type Character string of analysis type. Required if "S_A" is
#'  selected for `support_type`. Options are:
#'  \describe{
#'    \item{"A_SP"}{Small plot trial data analysis},
#'    \item{"A_OFE"}{On farm experiment data analysis},
#'    \item{"A_PRO"}{Protected environment experiment data analysis (glasshouse
#'      or growth chamber)},
#'    \item{"A_BIO"}{Bioinformatics or genetic data},
#'    \item{"A_BRE"}{Breeding or selection program data},
#'    \item{"A_ENV"}{Environmental or geospatial data},
#'    \item{"A_IMG"}{Imagery data},
#'    \item{"A_REC"}{Farm records or monitoring data},
#'    \item{"A_OTHER"}{Other type to be selected by recipient}.
#'   }
#' @param aagi_node Character string of AAGI node. Options are:
#' \describe{
#'   \item{"CU"}{Curtin University},
#'   \item{"UA"}{University of Adelaide},
#'   \item{"UQ"}{University of Queensland}.
#'  }
#' @param organisation_type Character string of organisation type. Options are:
#'  \describe{
#'  \item{"O_GRO"}{Grower group, industry association, or farm cooperative},
#'  \item{"O_AGR"}{Agronomy or farm advisory practice},
#'  \item{"O_ACA"}{Academic institution},
#'  \item{"O_GOV"}{Government agency or department},
#'  \item{"O_BRE"}{Seed or breeding company},
#'  \item{"O_TEC"}{Tech, biotech, or chemical company},
#'  \item{"O_OTHER"}{Other type to be selected by recipient}.
#' }
#'
#' @examplesIf interactive()
#' # create a survey URL for design and analysis performed on small plots for
#' # a government agency or department performed by CU
#' create_survey_url(support_type = c("S_D", "S_A"),
#'                   design_type = "D_SP",
#'                   analysis_type = "A_SP",
#'                   aagi_node = "CU",
#'                   organisation_type = "O_GOV")
#'
#' # create a survey URL for a bioinformatics analysis for an academic
#' # institution performed by UA
#' create_survey_url(support_type = "S_A",
#'                   analysis_type = "A_BIO",
#'                   aagi_node = "UA",
#'                   organisation_type = "O_ACA")
#' @author Rose Megirian, \email{rose.megirian@@curtin.edu.au} and Adam H.
#'  Sparks, \email{adam.sparks@@curtin.edu.au}
#' @returns The full survey URL to the OS clipboard (invisibly). Prints a summary
#'  for convenience.
#' @autoglobal
#' @export

create_survey_url <- function(
  support_type = NULL,
  design_type = NULL,
  analysis_type = NULL,
  aagi_node = NULL,
  organisation_type = NULL
) {
  # ---- Constants ----
  SUPPORT <- c(S_D = "Experimental design support", S_A = "Analysis support")
  DESIGN <- c(
    D_SP = "Small plot trial design",
    D_OFE = "On farm experiment design",
    D_GH = "Glasshouse experiment design",
    D_GC = "Growth chamber experiment design",
    D_LAB = "Lab experiment design",
    D_OTHER = "Other (recipient selects)"
  )
  ANALYSIS <- c(
    A_SP = "Small plot trial data analysis",
    A_OFE = "On farm experiment data analysis",
    A_PRO = "Protected environment experiment data analysis",
    A_BIO = "Bioinformatics or genetic data",
    A_BRE = "Breeding or selection program data",
    A_ENV = "Environmental or geospatial data",
    A_IMG = "Imagery data",
    A_REC = "Farm records or monitoring data",
    A_OTHER = "Other (recipient selects)"
  )
  NODE <- c(
    CU = "Curtin University",
    UA = "University of Adelaide",
    UQ = "University of Queensland"
  )
  ORG <- c(
    O_GRO = "Grower group / industry association / cooperative",
    O_AGR = "Agronomy or farm advisory practice",
    O_ACA = "Academic institution",
    O_GOV = "Government agency or department",
    O_BRE = "Seed or breeding company",
    O_TEC = "Tech, biotech, or chemical company",
    O_OTHER = "Other"
  )

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
  }

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
    paste0(base, "?", query)
  }

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
      "Select support type(s)",
      multiple = TRUE
    )
    if (!all(nzchar(support_type))) {
      cli::cli_abort("You must select at least one support type.")
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
    "Support type{?s}: {SUPPORT[support_type]}"
  )
  cli::cli_text(
    "Design type: {ifelse(nzchar(design_type), DESIGN[[design_type]], '<blank>')}"
  )
  cli::cli_text(
    "Analysis type: {ifelse(nzchar(analysis_type), ANALYSIS[[analysis_type]], '<blank>')}"
  )
  cli::cli_text("AAGI node: {NODE[[aagi_node]]}")
  cli::cli_text("Organisation type: {ORG[[organisation_type]]}")
  if (length(support_type) > 1L) {
    cli::cli_alert_info("You selected more than one support type.")
  }
  cli::cli_end()
  cli::cli_par()
  cli::cli_text(
    "Best practice: send a survey 7-10 days after each set of outputs rather than combining feedback."
  )
  cli::cli_end()
  cli::cli_par()
  cli::cli_text(
    "The URL has been copied to your clipboard for you to paste in your e-mail to your partners."
  )
  cli::cli_end()
  copy_to_clip(x = survey_url)
  invisible(survey_url)
}
