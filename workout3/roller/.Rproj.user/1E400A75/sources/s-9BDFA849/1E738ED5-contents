# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#1) Object "die"

#' @title Die Creation
#' @description Creates a die of object class "die"
#' @param sides a vector of length 6 representing the sides of the die
#' @param prob a vector of length 6 with sum of 1 representing the probability of rolling each respective side
#' @return A data table expressing each side of the die and its respective probabilities
#' @export
die <- function(sides = c("1", "2", "3", "4", "5", "6"), prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)) {
  check_sides(sides)
  check_prob(prob)
  res <- list(sides = sides, prob = prob)
  class(res) <- "die"
  return(res)
}

#' @title Print Die Method
#' @description Print method for objects of class "die"
#' @param x a "die" object
#' @return A data table expressing each side of the die and its respective probabilities
#' @export
print.die <- function(x) {
  cat('object "die"\n\n')
  cd <- data.frame(
    side = x$sides, prob = x$prob
  )
  print(cd)
  invisible(x)
}

#' @title Check Probability Function
#' @description Checks that the vector of probabilities is valid
#' @param x a vector of probabilties
#' @return True if the vectors of probabilities is of length 6, sums to 1, and that none of the probabilities is less than 0 or greater than 1. Else, error
#' @export
check_prob <- function(prob) {
  if(length(prob) !=6 | !is.numeric(prob)) {
    stop("\n'prob' must be a numeric vector of length 6")
  }
  if(any(prob < 0) | any(prob > 1)) {
    stop("\n'prob' values must be between 0 and 1")
  }
  if(sum(prob) !=1) {
    stop("\n'prob' values must be between 0 and 1")
  }
  TRUE
}

#' @title Check Sides Function
#' @description Checks that the vector of sides is of length 6
#' @param x A vector of sides
#' @return True if the vector contains 6 values. Else, error
#' @export
check_sides <- function(sides) {
  if (length(sides) != 6) {
    stop("\n'sides' must be on length 6")
  }
}

fair_die <- die()
fair_die

weird_die <- die(sides = c('i', 'ii', 'iii', 'iv', 'v', 'vi'))
weird_die

loaded_die <- die(prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35))
loaded_die

bad_die <- die(sides = c('a', 'b', 'c', 'd', 'e'))

bad_die2 <- die(sides = c('a', 'b', 'c', 'd', 'e', 'f'),
                prob = c(0.2, 0.1, 0.1, 0.1, 0.5, 0.1))

practice_die <- die(sides = c('1', '2', '3', '4', '5', '6'),
                    prob = c(0, 0, 0, .34, .33, .33))
#2) Object "roll"

#' @title Check Times Function
#' @description Checks that the number of times of rolling is valid
#' @param times an integer of number of times to roll
#' @return TRUE if times is an integer >= 0. Else, error
#' @export
check_times <- function(times) {
  if (times <= 0 | !is.numeric(times)) {
    stop("\n'times' must be a positive integer")
  } else {
    TRUE
  }
}

#' @title Roll
#' @description Computes the results of rolling a specified die a specificied number of times
#' @param z a die of class "die"
#' @param times number of times the die is being rolled
#' @return The results from rolling the die the specified number of times
#' @export
roll <- function(z, times = 1) {
  check_times(times)
  if(class(z) !="die") {
    stop("\nroll() requires an object 'die'")
  }
  rolls <- sample(z$sides, size = times, replace = TRUE, prob = z$prob)
  res <- list(
    sides = z$sides,
    rolls = rolls,
    prob = z$prob,
    total = length(rolls))
  class(res) <- "roll"
  res
}
#' @title Print Die Method
#' @description A print method for the roll function
#' @param q a roll function
#' @return A transformed version of the results from the roll function
#' @export
print.roll <- function(q) {
  cat('object "roll"\n')
  print(q$rolls)
}

set.seed(123)
fair50 <- roll(fair_die, times = 50)
fair50

names(fair50)
fair50$rolls
fair50$sides
fair50$prob
fair50$total

str_die <- die(
  sides = c('a', 'b', 'c', 'd', 'e', 'f'),
  prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35))
set.seed(123)
str_rolls <- roll(str_die, times = 20)
names(str_rolls)
str_rolls

#3) Summary Method for "Roll" Objects
#' @title Roll Summary
#' @description Gives the summary of a sample of rolls.
#' @param w a "roll" object that contains a sample of rolls with a specified die
#' @return A data table listing a summary of the sample
#' @export
summary.roll <- function(w) {
  if(class(w) !="roll") {
    stop("\nsummary() requires an object 'roll'")
  }
  side <- w$sides
  count <- as.data.frame(table(w$rolls))[ ,2]
  prop <- (count/w$total)

  freqs <- data.frame(side, count, prop)
  res2 <- list(freqs = freqs)
  class(res2) <- "summary.roll"
  res2
}

#' @title Print Summary Roll
#' @description A print method for summary.roll
#' @param e a summary.roll function
#' @return A modified data table listing a summary of the sample
#' @export
print.summary.roll <- function(e) {
  cat('summary "roll" \n\n')
  print(as.data.frame(e$freqs))
}

set.seed(123)
fair_50rolls <- roll(fair_die, times = 50)
fair50_sum <- summary(fair_50rolls)
fair50_sum
class(fair50_sum)
names(fair50_sum)
fair50_sum$freqs

#4) Plot method for "roll" objects
one_freqs <- function(x) {
  (cumsum(x$rolls == x$sides[1]) / 1:x$total)[x$total]
}
two_freqs <- function(x) {
  (cumsum(x$rolls == x$sides[2]) / 1:x$total)[x$total]
}
three_freqs <- function(x) {
  (cumsum(x$rolls == x$sides[3]) / 1:x$total)[x$total]
}
four_freqs <- function(x) {
  (cumsum(x$rolls == x$sides[4]) / 1:x$total)[x$total]
}
five_freqs <- function(x) {
  (cumsum(x$rolls == x$sides[5]) / 1:x$total)[x$total]
}
six_freqs <- function(x) {
  (cumsum(x$rolls == x$sides[6]) / 1:x$total)[x$total]
}

#' @title Frequencies
#' @description Determines what relative frequency to call upon
#' @param x the roll function
#' @param side the number of side to call upon
#' @return The relative frequency of the called upon side
#' @export
frequencies <- function(x, side = 1) {
  if(side == 1) {
    return(one_freqs(x))
  }
  if(side == 2) {
    return(two_freqs(x))
  }
  if(side == 3) {
    return(three_freqs(x))
  }
  if(side == 4) {
    return(four_freqs(x))
  }
  if(side == 5) {
    return(five_freqs(x))
  }
  if(side == 6) {
    return(six_freqs(x))
  }
}

#' @title Plot Roll
#' @description Creates a bar plot of a roll function
#' @param x a sample of rolls
#' @return A barplot for the sample of rolls
#' @export
plot.roll <- function(x) {
  barplot(c(frequencies(x), frequencies(x, 2), frequencies(x, 3),
            frequencies(x, 4), frequencies(x, 5), frequencies(x, 6)),
          as.numeric(x$side), type = 'n'
          , ylim = c(0,.2), las = 1, xlab = "sides of die"
          , bty = 'n', ylab = sprintf("relative frequencies")
          , names.arg = c(x$side[1], x$side[2], x$side[3],
                          x$side[4], x$side[5], x$side[6]))
}
plot(fair_50rolls)

#5) Additional Methods
#' @title Roll Extraction
#' @description Extracts a specific roll value from a sample of rolls
#' @param x the sample of rolls
#' @param i the value of the sample to extract
#' @return the value in the desired place of the sample of rolls
#' @export
"[.roll" <- function(x, i) {
  x$rolls[i]
}

set.seed(123)
fair500 <- roll(fair_die, times = 500)
fair500[500]

#' @title Roll Replacement
#' @description Replaces the a roll value with the desired value
#' @param x the sample of rolls
#' @param i the place value for the desired value to replace
#' @param value the desired value to replace x[i] with
#' @export
"[<-.roll" <- function(x, i, value) {
  x$rolls[i] <- value
  return(x$rolls)
}
fair500[500] <- 1
fair500
fair500[500]
summary(fair500)

#5) Additional Methods
"[.roll" <- function(x, i) {
  x$rolls[i]
}

set.seed(123)
fair500 <- roll(fair_die, times = 500)
fair500[500]

"[<-.roll" <- function(x, i, value) {
  x$rolls[i] <- value
  return(x$rolls)
}
fair500[500] <- 1
fair500
fair500[500]

"+.roll" <- function(obj, incr) {
  if (length(incr) != 1 | incr <= 0) {
    stop("\ninvalid increament (must be positive)")
  }
  more_rolls <- roll(obj$x, times = incr)
  rolls <- sample(x$sides, size = times, replace = TRUE, prob = x$prob)
  res <- list(
    sides = x$sides,
    rolls = rolls,
    prob = x$prob,
    total = length(rolls))
  class(res) <- "roll"
  res
}
fair600 <- fair500 + 100

