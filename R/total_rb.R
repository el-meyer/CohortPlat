
#' Helper Function: Total Responders_Bio
#' @export
total_rb <- function(x) {
  if ("Plac" %in% names(x)) {
    sum(sapply(x[c("Comb", "Back", "Mono", "Plac")], function(y) y$resp_bio), na.rm = T)
  } else {
    sum(sapply(x[c("Comb", "Back", "Mono")], function(y) y$resp_bio), na.rm = T)
  }
}
