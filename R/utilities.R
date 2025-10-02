#' Return the left-hand side if it's not NULL, otherwise it return the right-hand side
#' @dev
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
