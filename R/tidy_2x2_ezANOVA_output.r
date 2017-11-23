#' tidy_2x2_ezANOVA_output
#'
#' This function takes the output of a 2x2 ezANOVA model, calculates partial eta squared effect sizes and their 95% CIs, and returns a text summary of the ANOVA results for the intercept, both main effects, and the interaction. Type III sum of squares are employed. Tested on entirely between groups ANOVAs, within and mixed within-between designs not tested.
#' @param model output of ezANOVA using a 2X2 design.
#' @param X1 string name of IV1, as entered in ezANOVA
#' @param X2 string name of IV2, as entered in ezANOVA
#' @export
#' @examples
#' results <- tidy_2x2_ezANOVA_output(model, X1 = "IV_1", X2 = "IV_2")

tidy_2x2_ezANOVA_output <- function(model, X1, X2) {

  interaction <- paste(X1, X2, sep = ":")

  # dependencies
  require(timesavers)
  require(tidyverse)
  require(ez)
  require(schoRsch)
  require(MBESS)

  # round F and p values
  model_summary <-
    model$ANOVA %>%
    dplyr::select(-SSn, -SSd, -`p<.05`, -ges) %>%
    dplyr::mutate(F = round(F, 2),
                  p = round(p, 3))

  # calculate partial etq squared
  peta2s <-
    schoRsch::anova_out(model,
                        etasq = "partial",
                        print = FALSE)$`--- ANOVA RESULTS     ------------------------------------` %>%
    select(Effect, petasq) %>%
    dplyr::mutate(Effect = as.character(Effect),
                  petasq = as.numeric(as.character(petasq)))

  # function to calculate peta2_cis
  peta2_cis <- function(model_summary, row_number) {
    lims <- MBESS::conf.limits.ncf(F.value = model_summary$`F`[row_number],
                                   conf.level = 0.90,
                                   df.1 = model_summary$DFn[row_number],
                                   df.2 = model_summary$DFd[row_number])
    lower.lim <- lims$Lower.Limit/(lims$Lower.Limit + model_summary$DFn[row_number] + model_summary$DFd[row_number] + 1)
    lower.lim <- ifelse(is.na(lower.lim), 0, lower.lim)
    upper.lim <- lims$Upper.Limit/(lims$Upper.Limit + model_summary$DFn[row_number] + model_summary$DFd[row_number] + 1)
    upper.lim <- ifelse(is.na(upper.lim), 0, upper.lim)
    ci <- paste("[", round(lower.lim, 2), ", ", round(upper.lim, 2), "]", sep = "")
    return(ci)
  }

  # combine petasq values into df - THIS ROUTINE WOULD NEED TO BE CHANGED IF THE NUMBER OF IVs WAS CHANGED ***
  peta2_cis <-
    data.frame(Effect = c("(Intercept)",
                          X1,
                          X2,
                          interaction),
               petasq_ci = c(peta2_cis(model_summary = model_summary, row_number = 1),
                             peta2_cis(model_summary = model_summary, row_number = 2),
                             peta2_cis(model_summary = model_summary, row_number = 3),
                             peta2_cis(model_summary = model_summary, row_number = 4))) %>%
    dplyr::mutate(Effect = as.character(Effect),
                  petasq_ci = as.character(petasq_ci))

  # combine all results into a single df
  results <- model_summary %>%
    left_join(peta2s, by = "Effect") %>%
    left_join(peta2_cis, by = "Effect") %>%
    dplyr::rename(DF1 = DFn,
                  DF2 = DFd) %>%
    dplyr::mutate(p = timesavers::apa_p_value(p),
                  petasq = as.character(petasq),
                  petasq = ifelse(petasq < 0.01, "< .01", petasq),
                  Output = paste("F(", DF1, ", ", DF2, ") = ", F, ", p ", p, ", peta2 = ", petasq, ", 95% CI = ", petasq_ci, sep = ""))

  return(results)
}
