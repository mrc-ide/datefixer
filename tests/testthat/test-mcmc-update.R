test_that("gibbs_update_prob_error works correctly", {
  
  ## error_indicators - here there are 
  error_indicators <- data.frame(date1 = c(TRUE, FALSE, NA, FALSE, TRUE),
                                 date2 = c(NA, NA, FALSE, FALSE, TRUE),
                                 date3 = c(TRUE, TRUE, FALSE, FALSE, FALSE),
                                 date4 = c(TRUE, FALSE, FALSE, NA, TRUE))
  hyperparameters <- list(prob_error_shape1 = 2,
                          prob_error_shape2 = 3)
  rng <- monty::monty_rng_create(1)
  
  n <- 1000
  
  x <- replicate(n, gibbs_update_prob_error(error_indicators,
                                            hyperparameters, rng))
  
  a <- sum(error_indicators, na.rm = TRUE) + hyperparameters$prob_error_shape1
  b <- sum(!error_indicators, na.rm = TRUE) + hyperparameters$prob_error_shape2
  
  expect_equal(mean(x), a / (a + b), tolerance = 1e-2)
  expect_equal(var(x), a * b / ((a + b)^2 * (a + b + 1)), tolerance = 1e-1)
  
  
  ## edge cases
  ## No non-errors (FALSE), shape2 = 0 must result in 1
  error_indicators[, ] <- TRUE
  hyperparameters <- list(prob_error_shape1 = 2,
                          prob_error_shape2 = 0)
  expect_equal(
    gibbs_update_prob_error(error_indicators, hyperparameters, rng), 1)
  
  ## No errors (TRUE), shape1 = 0 must result in 0
  error_indicators[, ] <- FALSE
  hyperparameters <- list(prob_error_shape1 = 0,
                          prob_error_shape2 = 3)
  expect_equal(
    gibbs_update_prob_error(error_indicators, hyperparameters, rng), 0)
})
