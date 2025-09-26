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
##' @return Output
##'
##' @export
mcmc_run <- function(model,
                     sampler,
                     control = mcmc_control()) {
  
  parameters <- model$parameters
  initial <- numeric(length(parameters))
  initial[parameters == "prob_error"] <- 0.1
  initial[grepl("mean_delay", parameters)] <- 7
  initial[grepl("cv_delay", parameters)] <- 0.2
  
  runner <- 
    if (control$parallel) monty::monty_runner_callr(control$n_workers) else
      monty::monty_runner_serial()
  
  samples <- monty::monty_sample(model, sampler, control$n_steps,
                                 initial = initial,
                                 n_chains = control$n_chains, runner = runner,
                                 burnin = control$burnin, 
                                 thinning_factor = control$thinning_factor)
  
  
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
