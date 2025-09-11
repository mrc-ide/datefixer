##' Run MCMC
##'
##' @title Run MCMC
##'
##' @param observed_data Observed data
##'
##' @param delays Delays information
##'
##' @param control List of control parameters
##'
##' @param hyperparameters List of hyperparameters
##'
##' @return Output
##'
##' @export
mcmc_run <- function(observed_data,
                     delays,
                     control = mcmc_control(),
                     hyperparameters = mcmc_hyperparameters()) {
  
  ## Delay parameters will be numbered according to their row number in
  ## the delays data frame
  delay_ids <- seq_len(nrow(delays))
  parameters <- c("prob_error", paste0("mean_delay", delay_ids),
                  paste0("cv_delay", delay_ids))
  
  model <- monty::monty_model(list(
    parameters = parameters,
    density = function(pars) 0))
  
  model$observed_data <- observed_data 
  model$delays <- delays
  model$hyperparameters <- hyperparameters
  
  model <- list2env(model, parent = emptyenv())
  class(model) <- "monty_model"
  
  sampler <- monty::monty_sampler(
    "Sampler",
    "sampler",
    control,
    mcmc_initialise,
    mcmc_step)
  
  runner <- 
    if (control$parallel) monty::monty_runner_callr(control$n_workers) else
      monty::monty_runner_serial()
  
  samples <- monty::monty_sample(model, sampler, control$n_steps,
                                 initial = rep(0, length(parameters)),
                                 n_chains = control$n_chains, runner = runner,
                                 burnin = control$burnin, 
                                 thinning_factor = control$thinning_factor)
  
  
}

mcmc_step <- function(state_chain, state_sampler, control, model, rng) {
 
  state_chain <- update_prob_error(state_chain, model, rng)
  
  state_chain 
}

mcmc_initialise <- function(state_chain, control, model, rng) {
  initialise_augmented_data(model, control)
  
  return(NULL)
}

##' Create control parameters
##'
##' @title Create control parameters
##'
##' @param n_steps
##' 
##' @param burnin
##'
##' @param thinning_factor
##'
##' @param n_chains
##' 
##' @param parallel
##'
##' @param n_workers
##'
##' @param lower_quantile
##' 
##' @param upper_quantile
##'
##' @return List of control parameters
##'
##' @export
mcmc_control <- function(n_steps = 1000,
                         burnin = 0,
                         thinning_factor = 1,
                         n_chains = 1,
                         parallel = FALSE,
                         n_workers = 1,
                         lower_quantile = 0.01,
                         upper_quantile = 0.99) {
  
  list(n_steps = n_steps,
       burnin = burnin,
       thinning_factor = thinning_factor,
       n_chains = n_chains,
       parallel = parallel,
       n_workers = n_workers,
       lower_quantile = lower_quantile,
       upper_quantile = upper_quantile)
}

##' Create hyperparameters
##'
##' @title Create hyperparameters
##'
##' @param prob_error_shape1 The first shape parameter of the beta prior
##'   distribution for the probability of error
##'
##' @param prob_error_shape1 The second shape parameter of the beta prior
##'   distribution for the probability of error
##'
##' @param mean_delay_scale The scale parameter (mean) of the exponential prior
##'   distribution for the means of the delays
##'
##' @param cv_delay_scale The scale parameter (mean) of the exponential prior
##'   distribution for the coefficients of variations of the delays
##'
##' @return List of hyperparameters
##'
##' @export
mcmc_hyperparameters <- function(prob_error_shape1 = 1,
                                 prob_error_shape2 = 1,
                                 mean_delay_scale = 10,
                                 cv_delay_scale = 10) {
  list(prob_error_shape1 = prob_error_shape1,
       prob_error_shape2 = prob_error_shape2,
       mean_delay_scale = mean_delay_scale,
       cv_delay_scale = cv_delay_scale)
}
