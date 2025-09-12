update_prob_error <- function(state_chain, model, rng) {
  i <- model$parameters == "prob_error"
  
  
  state_chain$pars[i] <- 
    gibbs_update_prob_error(model$error_indicators, model$hyperparameters, rng)
    
  state_chain
}

gibbs_update_prob_error <- function(error_indicators, hyperparameters, rng) {
  n_errors <- sum(error_indicators, na.rm = TRUE)
  n_non_errors <- sum(!error_indicators, na.rm = TRUE)
  
  shape1 <- hyperparameters$prob_error_shape1  
  shape2 <- hyperparameters$prob_error_shape2
  
  monty::monty_random_beta(n_errors + shape1, n_non_errors + shape2, rng)
}
