#' @title reverse_chars()
#' @description reverses a string by characters
#' @param x string of characters
#' @return reversed string by characters


reverse_chars = function(x){
  split_string = strsplit(as.character(x), split = "")
  reverse_string = split_string[[1]][nchar(x):1]
  paste(reverse_string, collapse = "")
}
