
#' Helper Function: Create new cohort initially
#' @export
create_cohort_initial <- function(trial_struc, cohorts_start, n_int, n_fin,
                                  rr_comb_vec, rr_mono_vec, rr_back_vec, rr_plac_vec) {

  if (trial_struc == "no_plac") {

    res_list <- rep(list(c(list(decision = rep("none", 2), alloc_ratio = NULL, n_thresh = NULL, start_n = 0),
                           rep(list(list(rr = NULL, resp_bio = NULL, resp_hist = NULL, n = NULL)), 3))),
                    cohorts_start)

    for (i in 1:cohorts_start) {
      names(res_list)[i] <- paste0("Cohort", i)
      names(res_list[[i]])[5:7] <- c("Comb", "Mono", "Back")
      res_list[[i]]$alloc_ratio <- c(1,1,1)
      if (n_int == n_fin) {n_thresh_vec <- c(Inf, n_int)}else{n_thresh_vec <- c(n_int, Inf)}
      res_list[[i]]$n_thresh <- n_thresh_vec
      res_list[[i]][[5]]$rr <- rr_comb_vec[i]
      res_list[[i]][[6]]$rr <- rr_mono_vec[i]
      res_list[[i]][[7]]$rr <- rr_back_vec[i]
    }

  } else {

    res_list <- rep(list(c(list(decision = rep("none", 2), alloc_ratio = NULL, n_thresh = NULL, start_n = 0),
                           rep(list(list(rr = NULL, resp_bio = NULL, resp_hist = NULL, n = NULL)), 4))),
                    cohorts_start)

    for (i in 1:cohorts_start) {
      names(res_list)[i] <- paste0("Cohort", i)
      names(res_list[[i]])[5:8] <- c("Comb", "Mono", "Back", "Plac")
      res_list[[i]]$alloc_ratio <- c(1,1,1,1)
      if (n_int == n_fin) {n_thresh_vec <- c(Inf, n_int)}else{n_thresh_vec <- c(n_int, Inf)}
      res_list[[i]]$n_thresh <- n_thresh_vec
      res_list[[i]][[5]]$rr <- rr_comb_vec[i]
      res_list[[i]][[6]]$rr <- rr_mono_vec[i]
      res_list[[i]][[7]]$rr <- rr_back_vec[i]
      res_list[[i]][[8]]$rr <- rr_plac_vec[i]
    }

  }

  # Update allocation ratio
  if (cohorts_start > 1) {
    if (sharing_type != "cohort") {
      res_list <- update_alloc_ratio(res_list)
    }
  }

  return(res_list)

}
