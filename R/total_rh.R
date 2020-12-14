
#' Helper Function: Total Responders_Hist
#' @export
total_rh <- function(x) {
  if ("Plac" %in% names(x)) {
    sum(sapply(x[c("Comb", "Back", "Mono", "Plac")], function(y) y$resp_hist), na.rm = T)
  } else {
    sum(sapply(x[c("Comb", "Back", "Mono")], function(y) y$resp_hist), na.rm = T)
  }
}
