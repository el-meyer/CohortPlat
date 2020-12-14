
#' Helper Function: Check which cohorts are left
#' @export
coh_left_check <- function(x) {
  if (x$decision[1] %in% c("none", "PROMISING", "CONTINUE") & x$decision[2] == "none") {
    ret <- TRUE
  } else {
    ret <- FALSE
  }
  return(ret)
}
