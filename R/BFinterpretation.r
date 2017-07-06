#' BFinterpretation
#'
#' This function uses Wagenmakers' (2011) interpretation of Bayes Factors for the purpose of Bayesian hypothesis testing
#' @param BF Bayes Factor to be interpreted.
#' @keywords 
#' @export
#' @examples 
#' BFinterpretation(10.7)
#' 

BFinterpretation <- function(BF){
  BFtext <- ifelse(BF > 100, "Extreme evidence in favour of the alternative hypothesis",
                   ifelse(BF > 30, "Very strong evidence in favour of the alternative hypothesis",
                          ifelse(BF > 10, "Strong evidence in favour of the alternative hypothesis",
                                 ifelse(BF > 3, "Moderate evidence in favour of the alternative hypothesis",
                                        ifelse(BF < 0.01, "Extreme evidence in favour of the null hypothesis",
                                               ifelse(BF < 0.0333333, "Very strong  evidence in favour of the null hypothesis",
                                                      ifelse(BF < 0.1, "Strong evidence in favour of the null hypothesis",
                                                             ifelse(BF < 0.33333, "Moderate evidence in favour of the null hypothesis",
                                                                    "No credible evidence in favour of either hypothesis"))))))))
  BFtext
}
