update_pars_delay <- function(state_chain, control, model, rng) {
  
  n_delays <- nrow(model$delays)
  
  for (i in seq_len(n_delays)) {
    j_mean <- which(model$parameters == paste0("mean_delay", i))
    j_cv <- which(model$parameters == paste0("cv_delay", i))
    
    pars_delay <- state_chain$pars[c(j_mean, j_cv)]
    names(pars_delay) <- c("mean", "cv")
    
    delay_individuals <- model$groups %in% model$delays$group[[i]]
    delay_values <- model$true_dates[delay_individuals, model$delays$to[i]] -
      model$true_dates[delay_individuals, model$delays$from[i]]
    
    ## Update mean
    for (what in c("mean", "cv")) {
      pars_delay <- update_pars_delay1(what, pars_delay, delay_values,
                                       control, model$hyperparameters, rng)
    }
    
    state_chain$pars[c(j_mean, j_cv)] <- pars_delay
    
  }
  
  state_chain
}

update_pars_delay1 <- function(what, curr_pars_delay, delay_values,
                               control, hyperparameters, rng) {
  
  prop_pars_delay <- 
    propose_pars_delay(what, curr_pars_delay, control, rng)
  
  log_accept_prob <-
    log_accept_prob_pars_delay(curr_pars_delay, prop_pars_delay,
                               delay_values, control, hyperparameters)
  
  accept <- log_accept_prob > 0 || 
    log_accept_prob > log(monty::monty_random_real(rng))
  
  curr_pars_delay <- if (accept) prop_pars_delay else curr_pars_delay
  
  curr_pars_delay
  
}

propose_pars_delay <- function(what, pars_delay, control, rng) {
  sdlog <- if (what == "mean") control$mean_sdlog else control$cv_sdlog
  
  pars_delay[[what]] <- 
    monty::monty_random_log_normal(pars_delay[[what]], sdlog, rng)
  pars_delay
}

log_accept_prob_pars_delay <- function(curr_pars_delay, prop_pars_delay,
                                       delay_values, control, hyperparameters) {
  llikelihood_ratio <- llikelihood_delays(curr_pars_delay, delay_values) -
    llikelihood_delays(prop_pars_delay, delay_values)
  
  lprior_ratio <- lprior_delays(curr_pars_delay, hyperparameters) - 
    lprior_delays(prop_pars_delay, hyperparameters)
  
  lproposal_ratio <- 
    lproposal_delays(curr_pars_delay, prop_pars_delay, control) -
    lproposal_delays(prop_pars_delay, curr_pars_delay, control)
  
  llikelihood_ratio + lprior_ratio + lproposal_ratio
}

llikelihood_delays <- function(pars_delay, delay_values) {
  shape <- (1 / pars_delay[["cv"]])^2
  scale <- pars_delay[["mean"]] / shape
  
  sum(dgamma(delay_values, shape, scale = scale, log = TRUE))
}

lprior_delays <- function(pars_delay, hyperparameters) {
  dexp(pars_delay[["mean"]], 1 / hyperparameters$mean_delay_scale, log = TRUE) +
    dexp(pars_delay[["cv"]], 1 / hyperparameters$cv_delay_scale, log = TRUE)
}

lproposal_delays <- function(from_pars_delay, to_pars_delay, control) {
  dlnorm(to_pars_delay[["mean"]], log(from_pars_delay[["mean"]]), 
         control$mean_sdlog, log = TRUE) +
    dlnorm(to_pars_delay[["cv"]], log(from_pars_delay[["cv"]]), 
           control$cv_sdlog, log = TRUE)
}


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
