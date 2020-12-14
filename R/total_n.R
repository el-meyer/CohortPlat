
#' Helper Function: Total Sample Size
#' @export
total_n <- function(x) {
  if ("Plac" %in% names(x)) {
    sum(sapply(x[c("Comb", "Back", "Mono", "Plac")], function(y) y$n), na.rm = T)
  } else {
    sum(sapply(x[c("Comb", "Back", "Mono")], function(y) y$n), na.rm = T)
  }
}
