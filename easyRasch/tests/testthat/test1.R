context("Calculating probabilty")
test_that("caculated probability correctly", {
  a <- c(-3, 0, 1, 2, 2)
  Harry <- new("Rasch", name = 'Harry', a = a, 
               y = c(1, 1, 0, 0, 0))
  expect_that(Probability(Harry, theta = 1)[3,],
              equals(c(0.9820138, 0.7310586, 0.5000000, 0.2689414, 0.2689414)))
})


