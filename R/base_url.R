#' Get the AAGI Survey Base URL
#'
#' Returns the base URL of the AAGI Qualtrics survey. This is the single place
#' the URL is defined, so dependent packages and apps (e.g., Shiny) can use it
#' rather than hard coding their own copy.
#'
#' @examples
#' survey_base_url()
#'
#' @returns A length-one character string.
#' @export
survey_base_url <- function() {
  SURVEY_BASE_URL
}
