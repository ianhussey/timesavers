#' OR_to_d
#'
#' Approximate conversion of Odds Ratios to Cohen's d values
#' Using the method proposed by Hasselblad & Hedges (1995, Equation 5, p. 170): d = log(OR)*(sqrt(3)/pi).
#' Sánchez-Meca, Marín-Martínez & Chacón-Moscoso's (2003) simulation study demonstrated this method to have adequate coverage and bias, although poorer than other methods. However, this method has the benefit of being simplest to implement, and is therefore useful for demonstrations purposes. For example, converting Odds Ratios to Cohen's d just for reader familiarity to illustrate the size of an effect. 
#' @param OR The numeric (or vector of numeric) Odds Ratio to be converted. 
#' @return Returns a numeric Cohen's d value. 
#' @export
#' @examples
#' # Convert a single Odds Ratio
#' OR_to_d(2.13)
#' 
#' # Conert multiple Odds Ratios (e.g., OR and its CIs)
#' OR_to_d(c(26.26, 15.78, 43.69))

OR_to_d <- function(OR){
  log(OR)*(sqrt(3)/pi)
}
