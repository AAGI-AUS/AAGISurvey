#' Return the left-hand side if it's not NULL, else return the right-hand side
#' @dev
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
