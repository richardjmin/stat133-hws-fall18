#' @title Minkowski 
#' @description calculates the Minkowski distance
#' @param x numeric vector for one point
#' @param y numeric vector for the other point
#' @param p either a numeric value greater than 1, or a character string max(default 1)
#' @return the Minkowski distance 

minkowski = function(x, y, p = 1) {
  if (length(x) != length(y)) {
    stop('x and y have different lengths')
  }
  if (is.numeric(p) == TRUE & p < 1) {
    stop('p cannot be less than 1')
  }
  if (is.character(p) & p != 'max') {
    stop('invalid character value for p')
  }
  sum = 0
  for (i in 1:length(x)) {
    if (p == 'max') {
      term = rep(0, length(x))
      term[i] = abs(x[i] - y[i])
      return(max(term[i]))
    }
    else {
      term = rep(0, length(x))
      term[i] = (abs(x[i] - y[i]))^p
      sum = sum + term[i]
    }
  }
  return(sum^(1/p))
}



