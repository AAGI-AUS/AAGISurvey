#' Copy an \R Object's Text to the OS's Clipboard
#'
#' @param x An object to be written to the system clipboard.
#' @examples
#' copy_to_clip(x = "https://github.com/AAGI-AUS")
#' @returns A URL copied (invisibly) to the OS's clipboard.
#' @dev
#' @autoglobal
copy_to_clip <- function(x) {
  if (isFALSE(clipr::clipr_available())) {
    clipr::dr_clipr()
  }
  return(invisible(clipr::write_clip(content = x, object_type = "character")))
}
