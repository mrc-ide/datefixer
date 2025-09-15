test_that("update_prob_error_parameters works correctly", {
  
  ## error_indicators - here there are 
  error_indicators <- data.frame(date1 = c(TRUE, FALSE, NA, FALSE, TRUE),
                                 date2 = c(NA, NA, FALSE, FALSE, TRUE),
                                 date3 = c(TRUE, TRUE, FALSE, FALSE, FALSE),
                                 date4 = c(TRUE, FALSE, FALSE, NA, TRUE))
  hyperparameters <- list(prob_error_shape1 = 2,
                          prob_error_shape2 = 3)
  
  beta_pars <- update_prob_error_parameters(error_indicators, hyperparameters)
  
  a <- sum(error_indicators, na.rm = TRUE) + hyperparameters$prob_error_shape1
  b <- sum(!error_indicators, na.rm = TRUE) + hyperparameters$prob_error_shape2
  
  expect_identical(beta_pars, list(shape1 = a, shape2 = b))
})

test_that("update_prob_error updates correctly", {
  error_indicators <- data.frame(date1 = c(TRUE, FALSE, NA, FALSE, TRUE),
                                 date2 = c(NA, NA, FALSE, FALSE, TRUE),
                                 date3 = c(TRUE, TRUE, FALSE, FALSE, FALSE),
                                 date4 = c(TRUE, FALSE, FALSE, NA, TRUE))
  hyperparameters <- list(prob_error_shape1 = 2,
                          prob_error_shape2 = 3)
  
  n_delays <- 5
  delay_ids <- seq_len(n_delays)
  parameters <- c("prob_error", paste0("mean_delay", delay_ids),
                  paste0("cv_delay", delay_ids))
  state_chain <- list(pars = rep(0, length(parameters)))
  model <- list(parameters = parameters,
                error_indicators = error_indicators,
                hyperparameters = hyperparameters)
  
  rng <- monty::monty_rng_create(1)
  new_state_chain <- update_prob_error(state_chain, model, rng)
  
  ## check the correct parameter is updated
  i <- which(parameters == "prob_error")
  expect_true(new_state_chain$pars[i] != state_chain$pars[i])
  expect_equal(new_state_chain$pars[-i], state_chain$pars[-i])
  
  
  ## edge cases
  ## No non-errors (FALSE), shape2 = 0 must result in 1
  model$error_indicators[, ] <- TRUE
  model$hyperparameters <- list(prob_error_shape1 = 2,
                                prob_error_shape2 = 0)
  new_state_chain <- update_prob_error(state_chain, model, rng)
  expect_equal(new_state_chain$pars[i], 1)
  
  ## No errors (TRUE), shape1 = 0 must result in 0
  model$error_indicators[, ] <- FALSE
  model$hyperparameters <- list(prob_error_shape1 = 0,
                                prob_error_shape2 = 3)
  new_state_chain <- update_prob_error(state_chain, model, rng)
  expect_equal(new_state_chain$pars[i], 0)
})