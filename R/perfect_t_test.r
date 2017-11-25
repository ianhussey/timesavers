#' perfect_t_test
#'
#' Calculates a Welch's independent t-test, Cohen's d with 95% CIs, and returns the output as natural language.
#' @param data dataframe to be analysed. Must contain columns named IV (two values: factor, string or integer) and DV (continuous).
#' @param DV_name Number of decimal places to round to.
#' @param IV_condition_a_name labelling of the IV condition A for the output (e.g., "the control condition").
#' @param IV_condition_b_name labelling of the IV condition A for the output (e.g., "the experimental condition").
#' @param IV_condition_a_code_in_data assignment of condition A in the IV column (e.g., 0).
#' @param IV_condition_b_code_in_data assignment of condition A in the IV column (e.g., 1).
#' @return Returns a long string of the output of the tests. E.g., "A Welch's independent t-test demonstrated significant differences of large effect size in self-report ratings between the positive source (N = 50, M = -0.16, SD = 0.92) and the negative source (N = 50, M = 0.79, SD = 0.99), t(97.49) = -4.99, p < .001, d = -1, 95% CI [-1.42, -0.58]."
#' @export
#' @examples
#' perfect_t_test(data                         = data,
#'                DV_name                      = "self-report ratings",
#'                IV_condition_a_name          = "the experimental condition",
#'                IV_condition_b_name          = "the control condition",
#'                IV_condition_a_code_in_data  = 1,
#'                IV_condition_b_code_in_data  = 0)

perfect_t_test <- function(data, DV_name, IV_condition_a_name, IV_condition_b_name,
                           IV_condition_a_code_in_data, IV_condition_b_code_in_data) {

  # dependencies ------------------------------------------------------------

  require(tidyverse)
  require(effsize)
  require(weights)  # for rd(), a round() alternative

  # tests -------------------------------------------------------------------

  # t test
  t_test <- t.test(formula = DV ~ IV,
                   alternative = "two.sided",
                   paired = FALSE,
                   data = data)

  # effect size
  cohens_d <- cohen.d(DV ~ IV,
                      paired = FALSE,
                      data = data)

  desc_stats <- data %>%
    group_by(IV) %>%
    dplyr::summarize(mean = round(mean(DV), 2),
                     sd = round(sd(DV), 2),
                     n = n())

  # extract values ----------------------------------------------------------

  # t test
  t_test_est        <- round(t_test$statistic[[1]], 2)
  t_test_df         <- round(t_test$parameter[[1]], 2)
  t_test_p          <- round(t_test$p.value[[1]], 5)

  # effect size
  d_est             <- round(cohens_d$estimate[[1]], 2)
  d_ci_lower        <- round(cohens_d$conf.int[["inf"]], 2)
  d_ci_upper        <- round(cohens_d$conf.int[["sup"]], 2)
  d_interpretation  <- cohens_d$magnitude[[1]]

  ## descriptives
  a_m   <- desc_stats %>% filter(IV == IV_condition_a_code_in_data) %>% dplyr::select(mean) %>% as.numeric()  # IV adjusted here
  b_m   <- desc_stats %>% filter(IV == IV_condition_b_code_in_data) %>% dplyr::select(mean) %>% as.numeric()
  a_sd  <- desc_stats %>% filter(IV == IV_condition_a_code_in_data) %>% dplyr::select(sd)   %>% as.numeric()
  b_sd  <- desc_stats %>% filter(IV == IV_condition_b_code_in_data) %>% dplyr::select(sd)   %>% as.numeric()
  a_n   <- desc_stats %>% filter(IV == IV_condition_a_code_in_data) %>% dplyr::select(n)    %>% as.numeric()
  b_n   <- desc_stats %>% filter(IV == IV_condition_b_code_in_data) %>% dplyr::select(n)    %>% as.numeric()

  # convert to natural language ---------------------------------------------

  nhst <- ifelse(t_test_p < 0.05,
                 paste("A Welch's independent t-test demonstrated significant differences of ", d_interpretation, " effect size in ", DV_name, " between ", sep = ""),
                 paste("A Welch's independent t-test demonstrated non-significant differences of ", d_interpretation, " effect size between ", sep = ""))

  # round p values using APA rules
  t_test_p <- ifelse(t_test_p < 0.001, "< .001",
                     paste("= ", rd(t_test_p, 3), sep = ""))  # rd() rounds, converts to string, and removes the leading 0.

  # t test and d
  t_test_output <- paste(", t(", t_test_df, ") = ", t_test_est, ", p ", t_test_p, ", d = ", d_est, ", 95% CI [", d_ci_lower, ", ", d_ci_upper, "]. ", sep = "")

  # descriptive stats
  desc_a <- paste(IV_condition_a_name, " (n = ", a_n, ", M = ", a_m, ", SD = ", a_sd, ")", sep = "")
  desc_b <- paste(IV_condition_b_name, " (n = ", b_n, ", M = ", b_m, ", SD = ", b_sd, ")", sep = "")

  # combine
  t_test_text <- paste(nhst,
                       desc_a,
                       " and ",
                       desc_b,
                       t_test_output,
                       sep = "")

  return(t_test_text)
}
