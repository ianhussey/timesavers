#' cles
#'
#' This function calculates a permutation test version of the common language effect size. Unlike McGraw & Wong's original method, which used an algebraic/normal approximation, this version makes no assumptions about the distribution of the data.
#' Each X observation is paired to each Y observation. The proportion of XY pairs where X is higher than Y is then computed and output. Ties count as 0.5*TRUE.
#' Code adapted from https://janhove.github.io/reporting/2016/11/16/common-language-effect-sizes
#' @param runs max number of permutations
#' @param variable continuous variable
#' @param group dichotomous group
#' @param value1 assignement of group 1
#' @param value2 assignement of group 2
#' @param data data
#' @examples 
#' cles("Score", "Condition", data = data)

cles <- function(variable, group, data, value1 = 1, value2 = 0, runs = 10000) {
  
  # Ensure data is a data frame (e.g., not a tbl_data)
  data <- as.data.frame(data)
  
  # Select the observations for group 1
  x <- data[data[[group]] == value1, variable]
  
  # Select the observations for group 2
  y <- data[data[[group]] == value2, variable]
  
  # Matrix with difference between XY for all pairs (Guillaume Rousselet's suggestion)
  m <- outer(x,y,FUN="-")
  
  # Convert to booleans; count ties as half true.
  m <- ifelse(m==0, 0.5, m>0)
  
  # Return proportion of TRUEs
  qxly <- mean(m)
  
  return(as.numeric(qxly))
}
