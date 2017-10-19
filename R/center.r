#' center variables
#'
#' This function allows you to combine multiple ggplot2 plots into one.
#' Code from https://hlplab.wordpress.com/2009/04/27/centering-several-variables/
#' @param x Variable or dataframe to be centered.
#' @export
#' @examples
#' center_vars(my_data)

center_vars = function(x) {
  if (is.numeric(x)) { return(x - mean(x, na.rm = T)) }
  if (is.factor(x)) {
    x = as.numeric(x)
    return(x - mean(x, na.rm = T))
  }
  if (is.data.frame(x) || is.matrix(x)) {
    m = matrix(nrow = nrow(x), ncol = ncol(x))
    colnames(m) = paste("c", colnames(x), sep = "")
    for (i in 1:ncol(x)) {
      m[,i] = center_vars(x[,i])
    }
    return(as.data.frame(m))
  }
}
