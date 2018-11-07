#' @title count vowels
#' @description computes the number of vowels of a character string
#' @param  string 
#' @return number of the computed vowels 





count_vowels = function(x) {
  if (is.character(x) == FALSE) {
    stop("invalid input; a string was expected")
  }
  vowels = c('a', 'e', 'i', 'o', 'u')
  a = 0
  e = 0
  i = 0
  o = 0
  u = 0
  split_string = strsplit(tolower(x), '')[[1]]
  for (y in split_string) {
    if (y == vowels[1]) {
      a = a + 1
    }
    if (y == vowels[2]) {
      e = e + 1
    }
    if (y == vowels[3]) {
      i = i + 1
    }
    if (y == vowels[4]) {
      o = o + 1
    }
    if (y == vowels[5]) {
      u = u + 1
    }
  }
  counts = as.double(c(a, e, i, o, u))
  names(counts) = vowels
  return(counts)
}