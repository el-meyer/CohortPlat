
#' Helper Function for final sample sizes per cohort
#' @export
final_n_cohort <- function(res_list) {
  res <- matrix(nrow = 4, ncol = length(res_list))
  for (i in 1:length(res_list)) {
    for (j in 1:length(res_list[[i]]$alloc_ratio)) {
      res[j, i] <- sum(res_list[[i]][[j+4]]$n, na.rm = T)
    }
  }
  rownames(res) <- c("Combo", "Mono", "Backbone", "Placebo")
  colnames(res) <- paste0("Cohort", 1:length(res_list))
  return(res)
}
