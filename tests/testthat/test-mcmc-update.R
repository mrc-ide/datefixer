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
  
  model <- toy_model()$model
  
  parameters <- model$parameters
  pars <- numeric(length(parameters))
  pars[parameters == "prob_error"] <- 0.1
  pars[grepl("mean_delay", parameters)] <- 7
  pars[grepl("cv_delay", parameters)] <- 0.2
  
  rng <- monty::monty_rng_create(1L)
  
  augmented_data <- model$augmented_data_update(pars, rng)
  attr(pars, "data") <- augmented_data$data
  state_chain <- list(pars = pars,
                      density = augmented_data$density)
  
  new_state_chain <- update_prob_error(state_chain, model, rng)
  
  ## check the correct parameter is updated
  i <- which(parameters == "prob_error")
  expect_true(new_state_chain$pars[i] != state_chain$pars[i])
  expect_equal(new_state_chain$pars[-i], state_chain$pars[-i])
  
  
  ## edge cases
  ## No non-errors (FALSE), shape2 = 0 must result in 1
  augmented_data <- model$data_packer$unpack(attr(state_chain$pars, "data"))
  augmented_data$error_indicators[, ] <- TRUE
  attr(state_chain$pars, "data") <- model$data_packer$pack(augmented_data)
  model$hyperparameters <- list(prob_error_shape1 = 2,
                                prob_error_shape2 = 0)
  new_state_chain <- update_prob_error(state_chain, model, rng)
  expect_equal(new_state_chain$pars[i], 1)
  
  ## No errors (TRUE), shape1 = 0 must result in 0
  augmented_data <- model$data_packer$unpack(attr(state_chain$pars, "data"))
  augmented_data$error_indicators[, ] <- FALSE
  attr(state_chain$pars, "data") <- model$data_packer$pack(augmented_data)
  model$hyperparameters <- list(prob_error_shape1 = 0,
                                prob_error_shape2 = 3)
  new_state_chain <- update_prob_error(state_chain, model, rng)
  expect_equal(new_state_chain$pars[i], 0)
})
