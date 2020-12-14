
#' Helper Function: Check whether stopping rule reached
#' @export
is_sr_reached <- function(res_list, sr_drugs_pos, sr_pats, expected) {
  ret <- 0
  positives <- sum(substring(sapply(res_list, function(x) x$decision[2]), 1, 2) == "GO")
  if (positives >= sr_drugs_pos) {
    ret <- 1
  }
  if (sr_pats < expected) {
    if (sum(sapply(res_list, function(x) total_n(x)), na.rm = T) > sr_pats) {
      ret <- 1
    }
  }
  return(ret)
}
