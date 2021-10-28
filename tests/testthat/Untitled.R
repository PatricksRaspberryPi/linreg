library(testthat)
test_that("Same output as the lm.ridge model.", {
  ridge_1 <- ridgereg$new(formula = Petal.Length ~ Species, data=iris, lambda = 24)
  ridge_2 <- lm.ridge(formula = Petal.Length ~ Species, data=iris, lambda = 24)
  expect_equal(ridge_1$coef, ridge_2$coef)
})

