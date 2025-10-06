##' Create a datefixer model
##'
##' @title Create a datefixer model
##'
##' @param data Observed data
##'
##' @param delay_map Delays information
##' 
##' @param hyperparameters List of hyperparameters
##'
##' @return A datefixer model
##'
##' @export
datefixer_model <- function(data, delay_map, hyperparameters, control) {
  validate_data_and_delays(data, delays)
  
  groups <- data$group
  observed_dates <- observed_dates_to_int(data)
  
  n_delays <- nrow(delay_map)
  delay_ids <- seq_len(n_delays)
  parameters <- c("prob_error",
                  paste0("mean_delay", delay_ids),
                  paste0("cv_delay", delay_ids))
  
  domain <- cbind(rep(0, 1 + 2 * n_delays), c(1, rep(Inf, 2 * n_delays)))
  
  density <- create_datefixer_density(parameters, groups, delay_map,
                                      hyperparameters)
  
  augmented_data_update <- create_augmented_data_update(observed_dates, groups,
                                                        delay_map, control,
                                                        density)
  
  model <- monty::monty_model(
    list(parameters = parameters,
         domain = domain,
         density = density,
         augmented_data_update = augmented_data_update))
  
  model$hyperparameters <- hyperparameters
  
  model$data_packer <- create_augmented_data_packer(observed_dates)
  
  model
  
}

##' Create hyperparameters
##'
##' @title Create hyperparameters
##'
##' @param prob_error_shape1 The first shape parameter of the beta prior
##'   distribution for the probability of error
##'
##' @param prob_error_shape2 The second shape parameter of the beta prior
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
datefixer_hyperparameters <- function(prob_error_shape1 = 1,
                                      prob_error_shape2 = 1,
                                      mean_delay_scale = 10,
                                      cv_delay_scale = 10) {
  list(prob_error_shape1 = prob_error_shape1,
       prob_error_shape2 = prob_error_shape2,
       mean_delay_scale = mean_delay_scale,
       cv_delay_scale = cv_delay_scale)
}

validate_data_and_delays <- function(data, delay_map) {
  ## Here we will validate the data and delays and check they are compatible
}

create_datefixer_density <- function(parameters, groups, delay_map,
                                     hyperparameters) {
  
  density <- function(pars) {
    names(pars) <- parameters
    
    log_likelihood <- datefixer_log_likelihood(pars, groups, delay_map)
    log_prior <- datefixer_log_prior(pars, hyperparameters)
    
    log_likelihood + log_prior
  }
  
  density
}

datefixer_log_prior <- function(pars, hyperparameters) {
  lp_prob_error <- 
    dbeta(pars["prob_error"], hyperparameters$prob_error_shape1, 
          hyperparameters$prob_error_shape2, log = TRUE)
  
  lp_mean_delays <-
    dexp(pars[grepl("^mean_delay", names(pars))], 
         1 / hyperparameters$mean_delay_scale, log = TRUE)
  
  lp_cv_delays <-
    dexp(pars[grepl("^cv_delay", names(pars))], 
         1 / hyperparameters$cv_delay_scale, log = TRUE)
  
  lp_prob_error + sum(lp_mean_delays) + sum(lp_cv_delays)
}

datefixer_log_likelihood <- function(pars, groups, delay_map) {
  augmented_data <- model$data_packer$unpack(attr(pars, "data"))
  
  ll_errors <- datefixer_log_likelihood_errors(pars["prob_error"], 
                                               augmented_data$error_indicators)
  
  ll_delays <- 
    datefixer_log_likelihood_delays(augmented_data$true_dates,
                                    groups,
                                    pars[grepl("^mean_delay", names(pars))],
                                    pars[grepl("^cv_delay", names(pars))],
                                    delay_map)
  
  ll_errors + ll_delays
                                               
}

datefixer_log_likelihood_errors <- function(prob_error, error_indicators) {
  n_errors <- sum(error_indicators, na.rm = TRUE)
  n_non_errors <- sum(!error_indicators, na.rm = TRUE)
  
  n_errors * log(prob_error) + n_non_errors * log(1 - prob_error)
}

datefixer_log_likelihood_delays <- function(true_dates, groups, mean_delays,
                                            cv_delays, delay_map) {

  ll_by_delay <-
    vapply(seq_len(nrow(delay_map)), 
           function(i) datefixer_log_likelihood_delays1(true_dates,
                                                        groups,
                                                        mean_delays[i],
                                                        cv_delays[i],
                                                        delay_map[i, ]),
           numeric(1))
  
  sum(ll_by_delay)
  
}

datefixer_log_likelihood_delays1 <- function(true_dates, groups, mean_delay,
                                             cv_delay, delay_info) {
  
  shape <- (1 / cv_delay)^2
  scale <- mean_delay / shape 
  
  delay_individuals <- groups %in% unlist(delay_info$group)
  delay_values <- true_dates[delay_individuals, delay_info$to] -
    true_dates[delay_individuals, delay_info$from]
  
  sum(dgamma(delay_values, shape, scale = scale, log = TRUE))
}


create_augmented_data_update <- function(observed_dates, groups, delay_map,
                                         control, density) {
  augmented_data_update <- function(pars, rng) {
    augmented_data <- attr(pars, "data")
    
    if (is.null(augmented_data)) {
      augmented_data <- initialise_augmented_data(observed_dates, groups,
                                                  delay_map, control, rng)
      attr(pars, "data") <- model$data_packer$pack(augmented_data)
    }
    
    density <- density(pars)
    
    list(data = augmented_data, density = density)
  } 
  augmented_data_update  
}

observed_dates_to_int <- function(data) {
  dates <- setdiff(names(data), c("id", "group"))
  
  observed_dates <- data[, dates]
  
  as.data.frame(apply(observed_dates, c(1, 2), date_to_int))
}
  

create_augmented_data_packer <- function(observed_dates) {
  
  monty::monty_packer(array = list(true_dates = dim(observed_dates),
                                   error_indicators = dim(observed_dates)))
}
