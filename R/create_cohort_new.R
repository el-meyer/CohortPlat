
#' Helper Function: Create new cohort and update allocation ratio
#' @export
create_cohort_new <- function(res_list, plac, n_int, n_fin, sharing_type,
                              rr_comb_vec, rr_mono_vec, rr_back_vec, rr_plac_vec) {

  if (n_int == n_fin) {n_thresh_vec <- c(Inf, n_int)}else{n_thresh_vec <- c(n_int, Inf)}

  if (plac) {

    new_list <- list(c(list(decision = rep("none", 2),
                            alloc_ratio = c(1,1,1,1),
                            n_thresh = n_thresh_vec,
                            start_n = sum(sapply(res_list, function(x) total_n(x)), na.rm = T)
    ),
    rep(list(list(rr = NULL,
                  resp_bio = rep(NA, length(res_list[[1]][[5]]$n)),
                  resp_hist = rep(NA, length(res_list[[1]][[5]]$n)),
                  n = rep(NA, length(res_list[[1]][[5]]$n)))), 4)))

    names(new_list)[1] <- paste0("Cohort", length(res_list) + 1)
    names(new_list[[1]])[5:8] <- c("Comb", "Mono", "Back", "Plac")
    new_list[[1]][[5]]$rr <- rr_comb_vec[length(res_list) + 1]
    new_list[[1]][[6]]$rr <- rr_mono_vec[length(res_list) + 1]
    new_list[[1]][[7]]$rr <- rr_back_vec[length(res_list) + 1]
    new_list[[1]][[8]]$rr <- rr_plac_vec[length(res_list) + 1]

  } else {

    new_list <- list(c(list(decision = rep("none", 2),
                            alloc_ratio = c(1,1,1),
                            n_thresh = n_thresh_vec,
                            start_n = sum(sapply(res_list, function(x) total_n(x)), na.rm = T)
    ),
    rep(list(list(rr = NULL,
                  resp_bio = rep(NA, length(res_list[[1]][[5]]$n)),
                  resp_hist = rep(NA, length(res_list[[1]][[5]]$n)),
                  n = rep(NA, length(res_list[[1]][[5]]$n)))), 3)))

    names(new_list)[1] <- paste0("Cohort", length(res_list) + 1)
    names(new_list[[1]])[5:7] <- c("Comb", "Mono", "Back")
    new_list[[1]][[5]]$rr <- rr_comb_vec[length(res_list) + 1]
    new_list[[1]][[6]]$rr <- rr_mono_vec[length(res_list) + 1]
    new_list[[1]][[7]]$rr <- rr_back_vec[length(res_list) + 1]

  }

  res_list <- c(res_list, new_list)

  # Update allocation ratio

  if (sharing_type != "cohort") {
    res_list <- update_alloc_ratio(res_list)
  }

  return(res_list)

}
