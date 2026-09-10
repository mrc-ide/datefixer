update_pars_delay <- function(state_chain, control, model, rng) {
  
  augmented_data <- model$data_packer$unpack(attr(state_chain$pars, "data"))
  
  for (i in seq_along(model$info$delay_info$distribution)) {
    state_chain$pars <- update_pars_delay1(i, state_chain$pars, augmented_data,
                                           control, model, rng)
  }
  
  state_chain
}


update_pars_delay1 <- function(i, pars, augmented_data, control, model, rng) {
  
  delay_from <- model$info$delay_info$from[i]
  delay_to <- model$info$delay_info$to[i]
  is_delay_in_group <- 
    unlist(lapply(model$info$group_info, function (x) x$is_delay_in_group[i]))
  
  k <- model$groups_data %in% which(is_delay_in_group)
  
  delay_values <- augmented_data$estimated_dates[k, delay_to] - 
    augmented_data$estimated_dates[k, delay_from]
  
  if (model$info$delay_info$distribution[i] == "gamma") {
    j_mean <- model$parameters == paste0("delay", i, "_mean")
    j_shape <- model$parameters == paste0("delay", i, "_shape")
    
    shape <- pars[j_shape]
    mean <- update_gamma_mean(shape, delay_values, model$hyperparameters, rng)
    shape <-
      update_gamma_shape(shape, mean, delay_values, model$hyperparameters, rng)
    
    pars[j_mean] <- mean
    pars[j_shape] <- shape
  } else if (model$info$delay_info$distribution[i] == "log-normal") {
    j_meanlog <- model$parameters == paste0("delay", i, "_meanlog")
    j_precisionlog <- model$parameters == paste0("delay", i, "_precisionlog")
    
    precisionlog <- pars[j_precisionlog]
    meanlog <- update_log_normal_meanlog(precisionlog, delay_values,
                                         model$hyperparameters, rng)
    precisionlog <- update_log_normal_precisionlog(meanlog, delay_values, 
                                                   model$hyperparameters, rng)
    
    pars[j_meanlog] <- meanlog
    pars[j_precisionlog] <- precisionlog
  }
  
  pars
}


update_gamma_mean <- function(shape, delay_values, hyperparameters, rng) {
  
  sample_pars <- update_gamma_mean_parameters(shape, delay_values, 
                                              hyperparameters)
  
  random_inverse_gamma(sample_pars$shape, sample_pars$scale, rng)
}


update_gamma_mean_parameters <- function(shape, delay_values, hyperparameters) {
  
  sample_shape <- 
    hyperparameters$gamma_mean_prior_shape + length(delay_values) * shape
  sample_scale <- 
    hyperparameters$gamma_mean_prior_scale + shape * sum(delay_values)
  
  list(shape = sample_shape,
       scale = sample_scale)
}


update_gamma_shape <- function(shape, mean, delay_values, 
                               hyperparameters, rng) {
  
  sample_pars <- update_gamma_shape_parameters(mean, delay_values,
                                               hyperparameters)
  
  shape_new <- 
    monty::monty_random_gamma_rate(sample_pars$shape, sample_pars$rate, rng)
  
  log_accept_prob <- 
    calc_gamma_shape_accept_prob(shape_new, shape, mean, sample_pars,
                                 delay_values, hyperparameters)
  
  accept <- log_accept_prob > 0 || 
    log_accept_prob > log(monty::monty_random_real(rng))
  if (accept) {
    shape <- shape_new
  }
  
  shape
}


update_gamma_shape_parameters <- function(mean, delay_values, hyperparameters) {
  n <- length(delay_values)
  R <- sum(log(delay_values))
  S <- sum(delay_values)
  T <- S / mean - R + n * log(mean) - n
  
  a0 <- hyperparameters$gamma_shape_prior_shape
  b0 <- hyperparameters$gamma_shape_prior_rate
  
  A <- a0 + n / 2
  B <- b0 + T
  
  M <- 10
  eps <- 1e-8
  
  for (i in seq_len(M)) {
    a <- A / B
    A <- a0 - n * a + n * a^2 * trigamma(a)
    B <- b0 + (A - a0) / a - n * log(a) + n * digamma(a) + T
    if (abs(a / (A / B) - 1) < eps || i == M) {
      return(list(shape = A,
                  rate = B))
    }
  }
  
}


calc_gamma_shape_accept_prob <- function(shape_new, shape, mean, sample_pars,
                                         delay_values, hyperparameters) {
  likelihood <- 
    log_density_delay(delay_values, list(shape = shape, mean = mean), "gamma")
  likelihood_new <- log_density_delay(delay_values, 
                                      list(shape = shape_new, mean = mean), 
                                      "gamma")
  
  prior <-
    dgamma(shape, hyperparameters$gamma_shape_prior_shape, 
           rate = hyperparameters$gamma_shape_prior_rate, log = TRUE)
  prior_new <-
    dgamma(shape_new, hyperparameters$gamma_shape_prior_shape, 
           rate = hyperparameters$gamma_shape_prior_rate, log = TRUE)
  
  proposal <-
    dgamma(shape, sample_pars$shape, rate = sample_pars$rate, log = TRUE)
  proposal_new <-
    dgamma(shape_new, sample_pars$shape, rate = sample_pars$rate, log = TRUE)
  
  (sum(likelihood_new) - sum(likelihood)) + (prior_new - prior) + 
    (proposal - proposal_new)
}


update_log_normal_meanlog <- function(precisionlog, delay_values, 
                                      hyperparameters, rng) {
  
  sample_pars <- update_log_normal_meanlog_parameters(precisionlog, delay_values, 
                                                      hyperparameters)
  
  monty::monty_random_normal(sample_pars$mean, sample_pars$sd, rng)
}


update_log_normal_meanlog_parameters <- function(precisionlog, delay_values,
                                                 hyperparameters) {
  n <- length(delay_values)
  
  mean0 <- hyperparameters$log_normal_meanlog_prior_mean
  precision0 <- hyperparameters$log_normal_meanlog_prior_precision
  
  sample_mean <- 
    (precision0 * mean0 + precisionlog * sum(log(delay_values))) /
    (precision0 + n * precisionlog)
  
  sample_sd <- 1 / sqrt(precision0 + n * precisionlog)
  
  list(mean = sample_mean,
       sd = sample_sd)
}


update_log_normal_precisionlog <- function(meanlog, delay_values, 
                                           hyperparameters, rng) {
  
  sample_pars <- update_log_normal_precisionlog_parameters(meanlog, delay_values, 
                                                           hyperparameters)
  
  monty::monty_random_gamma_rate(sample_pars$shape, sample_pars$rate, rng)
}


update_log_normal_precisionlog_parameters <- function(meanlog, delay_values, 
                                                      hyperparameters) {
  
  n <- length(delay_values)
  
  sample_shape <- hyperparameters$log_normal_precisionlog_prior_shape + n / 2
  sample_rate <- hyperparameters$log_normal_precisionlog_prior_rate +
    sum((log(delay_values) - meanlog)^2) / 2
  
  list(shape = sample_shape,
       rate = sample_rate)
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


random_inverse_gamma <- function(shape, scale, rng) {
  ## we sample from inverse gamma distribution with shape and scale
  ## by sampling from gamma distribution with the same shape and
  ## rate = scale of the inverse gamma, and then inverting it
  1 / monty::monty_random_gamma_rate(shape, scale, rng)
}
