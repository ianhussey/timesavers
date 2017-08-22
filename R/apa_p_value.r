#' apa_p_value
#'
#' This function uses rounds p values using APA rules: No leading zero, rounded to three decimal places, below this shows as "< .001".
#' @param p p value to be formatted
#' @keywords 
#' @export
#' @examples 
#' apa_p_value(0.00000004)
#' apa_p_value(0.487173)

apa_p_value <- function(p){
  p_formatted <- ifelse(p > 0.001, round(p, 3),
                        ifelse(p < 0.001, "<.001"))
  p_formatted <- gsub(pattern = "0.", replacement = ".", x = p_formatted, fixed = TRUE)
  p_formatted
}