#' Create the URL string from user provided values
#'
#' Create the URL string from user provided values for returning to the user.
#'
#' @param base Character string of base URL.
#' @param support_type Character string of support type.
#' @param design_type Character string of design type.
#' @param analysis_type Character string of analysis type.
#' @param aagi_node Character string of AAGI node.
#' @param organisation_type Character string of organisation type.
#'
#' @examples
#' build_url(
#'   base = "https://curtin.au1.qualtrics.com/jfe/form/SV_eXLvfgMz58RktQa",
#'   support_type = "S_D",
#'   design_type = "D_SP",
#'   analysis_type = "A_BIO",
#'   aagi_node = "CU",
#'   organisation_type = "O_ACA"
#' )
#'
#' @export
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
