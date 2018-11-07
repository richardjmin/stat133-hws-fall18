#' @title is_hex
#' @description checks whether an input string is a valid hex color without an alpha transparency value
#' @param any strings
#' @return TRUE or FALSE

is_hex <- function(x) {
  if (is.character(x) == FALSE) {
    stop("invalid input, a string was expected")
  }
  if(nchar(x) == 7) {
    dec = grep(pattern = "^#([1234567890abcdef]){6}", x = x, ignore.case = TRUE)
    if (length(dec) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return (FALSE)
  }
}



#' @title is_hex_alpha
#' @description checks whether an input string is a valid hex color with an alpha transparency value
#' @param any input string 
#' @return TRUE or FALSE


is_hex_alpha = function(x) {
  if (is.character(x) == FALSE) {
    stop("invalid input, a string was expected")
  }
  if(nchar(x) == 9) {
    dec = grep(pattern = "^#([1234567890abcdef]){8}", x = x, ignore.case = TRUE)
    if (length(dec) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return (FALSE)
  }
}








