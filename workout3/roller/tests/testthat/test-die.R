context("Check die arguments")

library(testthat)
#die
test_that("check_sides with ok vectors", {
  expect_true(check_sides(c('i', 'ii', 'iii', 'iv', 'v', 'vi')))
})

# create a loaded die
test_that("check_probility with ok vectors", {
  expect_true(check_prob(c(0.1, 0.1, 0.1, 0.3, 0.20, 0.3)))
})

# bad sides
test_that("check_sides fails with invalid lengths", {
  expect_error(check_sides(c('a', 'b', 'c', 'd', 'e')))
})

#bad sides
test_that("check_sides and check_prob fails with invalid lengths", {
  expect_true(check_sides(c('a', 'b', 'c', 'd', 'e', 'f')))
  expect_error(check_prob(c(0.3, 0.2, 0.1, 0.1, 0.2, 0.1)))
})
