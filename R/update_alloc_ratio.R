
#' Helper Function: Update Allocation Ratio
#' @export
update_alloc_ratio <- function(res_list) {

  cohorts_left <- which(sapply(res_list, function(x) coh_left_check(x)))
  comb_numb <- sum(sapply(res_list[cohorts_left], function(x) names(x)[5:8]) == "Comb", na.rm = T)
  back_numb <- sum(sapply(res_list[cohorts_left], function(x) names(x)[5:8]) == "Back", na.rm = T)
  mono_numb <- sum(sapply(res_list[cohorts_left], function(x) names(x)[5:8]) == "Mono", na.rm = T)
  plac_numb <- sum(sapply(res_list[cohorts_left], function(x) names(x)[5:8]) == "Plac", na.rm = T)

  for (i in cohorts_left) {
    if (length(res_list[[i]]$alloc_ratio) == 3) {
      res_list[[i]]$alloc_ratio <- c(comb_numb, mono_numb, 1)
    } else {
      res_list[[i]]$alloc_ratio <- c(comb_numb, mono_numb, 1, 1)
    }
  }

  return(res_list)
}
