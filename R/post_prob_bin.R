
#' Helper function posterior probability of one Beta being by margin delta greater than other Beta
#' @export
post_prob_bin <- function(n_exp, n_contr, resp_exp, resp_contr, delta,
                          a0_exp, b0_exp, a0_contr, b0_contr) {

  # in notation of diploma thesis, this calculates the probability P(P_e >= P_c + delta_sup)
  prob_sup <- stats::integrate(function(y) {
    stats::dbeta(y, a0_exp + resp_exp, b0_exp - resp_exp + n_exp) *
      stats::pbeta(y - delta, a0_contr + resp_contr, b0_contr - resp_contr + n_contr)
  }, delta, 1)$value

  # return posterior probability
  return(prob_sup)
}
