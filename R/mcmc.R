##' Run MCMC
##'
##' @title Run MCMC
##'
##' @param model Model
##'
##' @param sampler Sampler
##'
##' @param control List of control parameters
##' 
##' @param initial Initial values
##'
##' @return Output
##'
##' @export
mcmc_run <- function(model,
                     sampler,
                     initial = mcmc_initial(model),
                     control = mcmc_control()) {
  
  parameters <- model$parameters
  
  runner <- 
    if (control$parallel) monty::monty_runner_callr(control$n_workers) else
      monty::monty_runner_serial()
  
  samples <- monty::monty_sample(model, sampler, control$n_steps,
                                 initial = initial,
                                 n_chains = control$n_chains, runner = runner,
                                 burnin = control$burnin, 
                                 thinning_factor = control$thinning_factor)
  
  ## Unpack augmented data
  samples$data <- unpack_augmented_data(samples$data, model$data_packer)
  ## Convert estimated dates to dates
  samples$data$estimated_dates <- 
    int_to_date(floor(samples$data$estimated_dates))
  
  samples
  
}

##' Create control parameters
##'
##' @title Create control parameters
##'
##' @param n_steps The number of steps to run in each MCMC chain
##' 
##' @param burnin The number of steps at the beginning of each chain to discard
##'   as burnin
##'
##' @param thinning_factor A thinning factor applied to the chains. If given,
##'   every`thinning_factor`'th step is retained 
##'
##' @param n_chains The number of chains to run
##' 
##' @param parallel Logical, indicating whether or not to run chains in parallel
##'
##' @param n_workers Number of workers to use for parallelisation
##'
##' @param lower_quantile Lower quantile used for initialisation of true dates
##' 
##' @param upper_quantile Upper quantile used for initialisation of true dates
##' 
##' @param mean_sdlog The sdlog proposal parameter for the delay means
##' 
##' @param cv_sdlog The sdlog proposal parameter for the delay coefficients of
##'   variation
##'   
##' @param prob_update_estimated_dates The probability of proposing an update
##'   to each estimated date at each iteration in the MCMC
##'
##' @param prob_update_error_indicators The probability of proposing an update
##'   to each error indicator (with the corresponding estimated date updated
##'   accordingly) at each iteration in the MCMC
##'   
##' @param prob_error_swap The probability of proposing to swap all errors to
##' non-errors and vice versa (excluding missing dates) for individuals with at
##' least one error and non-error at each iteration in the MCMC
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
                         upper_quantile = 0.99,
                         mean_sdlog = 1,
                         cv_sdlog = 1,
                         prob_update_estimated_dates = 0.1,
                         prob_update_error_indicators = 0.1,
                         prob_error_swap = 1) {
  
  list(n_steps = n_steps,
       burnin = burnin,
       thinning_factor = thinning_factor,
       n_chains = n_chains,
       parallel = parallel,
       n_workers = n_workers,
       lower_quantile = lower_quantile,
       upper_quantile = upper_quantile, 
       mean_sdlog = mean_sdlog,
       cv_sdlog = cv_sdlog,
       prob_update_estimated_dates = prob_update_estimated_dates,
       prob_update_error_indicators = prob_update_error_indicators,
       prob_error_swap = prob_error_swap)
}

##' Create initial parameter values
##'
##' @title Create initial parameter values
##'
##' @param model Model
##' 
##' @param initial_mean_delay The initial value for the mean delays
##' 
##' @param initial_cv_delay The initial value for the coefficient of variation
##'   of delays
##'   
##' @param initial_prob_error The initial value for the probability of error  
##' 
##' @return Vector of initial parameter values
##'
##' @export
mcmc_initial <- function(model,
                         initial_mean_delay = 7,
                         initial_cv_delay = 0.2,
                         initial_prob_error = 1) {
  initial <- numeric(length(model$parameters))
  initial[model$parameters == "prob_error"] <- 0.1
  initial[grepl("mean_delay", model$parameters)] <- initial_mean_delay
  initial[grepl("cv_delay", model$parameters)] <- initial_cv_delay
  
  initial
}
