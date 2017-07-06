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
  BFtext <- ifelse(BF > 100, "extreme evidence in favour of the alternative hypothesis",
                   ifelse(BF > 30, "very strong evidence in favour of the alternative hypothesis",
                          ifelse(BF > 10, "strong evidence in favour of the alternative hypothesis",
                                 ifelse(BF > 3, "moderate evidence in favour of the alternative hypothesis",
                                        ifelse(BF < 0.01, "extreme evidence in favour of the null hypothesis",
                                               ifelse(BF < 0.0333333, "very strong  evidence in favour of the null hypothesis",
                                                      ifelse(BF < 0.1, "strong evidence in favour of the null hypothesis",
                                                             ifelse(BF < 0.33333, "moderate evidence in favour of the null hypothesis",
                                                                    "no credible evidence in favour of either hypothesis"))))))))
  BFtext
}
