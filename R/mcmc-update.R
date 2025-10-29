update_pars_delay <- function(state_chain, control, model, rng) {
  
  pars_delay <- grep("^mean_delay|^cv_delay", model$parameters, value = TRUE)
  
  for (nm in pars_delay) {
    state_chain <- update_pars_delay1(nm, state_chain, control, model, rng)
  }
  
  state_chain
}


update_pars_delay1 <- function(nm, state_chain, control, model, rng) {
  
  pars_new <- propose_pars_delay(nm, state_chain$pars, control, model, rng)
  
  density_new <- model$density(pars_new)
  
  lproposal_ratio <- lproposal_ratio_delay(nm, state_chain$pars, pars_new,
                                           control, model)
  
  log_accept_prob <- density_new - state_chain$density + lproposal_ratio
  
  accept <- log_accept_prob > 0 || 
    log_accept_prob > log(monty::monty_random_real(rng))
  
  if (accept) {
    state_chain$pars <- pars_new
    state_chain$density <- density_new
  }
  
  state_chain
}


propose_pars_delay <- function(nm, pars, control, model, rng) {
  
  i <- which(model$parameters == nm)
  
  sdlog <- if (grepl("^mean", nm)) control$mean_sdlog else control$cv_sdlog
  
  pars[[i]] <- 
    monty::monty_random_log_normal(pars[[i]], sdlog, rng)
  
  pars
}


lproposal_ratio_delay <- function(nm, pars, pars_new, control, model) {
  i <- which(model$parameters == nm)
  
  sdlog <- if (grepl("^mean", nm)) control$mean_sdlog else control$cv_sdlog
  
  dlnorm(pars[[i]], log(pars_new[[i]]), sdlog, log = TRUE) -
    dlnorm(pars_new[[i]], log(pars[[i]]), sdlog, log = TRUE)
}


update_prob_error <- function(state_chain, model, rng) {
  i <- model$parameters == "prob_error"
  
  augmented_data <- model$data_packer$unpack(attr(state_chain$pars, "data"))
  
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
