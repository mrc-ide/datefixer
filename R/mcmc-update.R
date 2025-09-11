update_prob_error <- function(state_chain, model, rng) {
  i <- model$parameters == "prob_error"
  n_errors <- sum(model$error_indicators, na.rm = TRUE)
  n_non_errors <- sum(!model$error_indicators, na.rm = TRUE)
  
  shape1 <- model$hyperparameters$prob_error_shape1  
  shape2 <- model$hyperparameters$prob_error_shape2
  
  state_chain$pars[i] <- 
    monty::monty_random_beta(n_errors + shape1, n_non_errors + shape2, rng)
  
  state_chain
}
