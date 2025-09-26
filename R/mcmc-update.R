update_prob_error <- function(state_chain, model, rng) {
  i <- model$parameters == "prob_error"
  
  augmented_data <- attr(state_chain$pars, "augmented_data")
  
  beta_pars <- 
    update_prob_error_parameters(augmented_data$error_indicators,
                                 model$hyperparameters)
  
  state_chain$pars[i] <- 
    monty::monty_random_beta(beta_pars$shape1, beta_pars$shape2, rng)
    
  state_chain$density <- model$density(state_chain$pars)
  
  state_chain
}

update_prob_error_parameters <- function(error_indicators, hyperparameters) {
  n_errors <- sum(error_indicators, na.rm = TRUE)
  n_non_errors <- sum(!error_indicators, na.rm = TRUE)
  
  shape1 <- n_errors + hyperparameters$prob_error_shape1  
  shape2 <- n_non_errors + hyperparameters$prob_error_shape2
  
  list(shape1 = shape1,
       shape2 = shape2)
}
