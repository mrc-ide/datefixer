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
##' @param control List of control parameters
##'
##' @return A datefixer model
##'
##' @export
datefixer_model <- function(data, delay_map, hyperparameters, control) {
  
  delay_map <- validate_data_and_delays(data, delay_map)
  
  groups <- data$group
  observed_dates <- observed_dates_to_int(data)
  
  n_delays <- nrow(delay_map)
  delay_ids <- seq_len(n_delays)
  parameters <- c("prob_error",
                  paste0("mean_delay", delay_ids),
                  paste0("cv_delay", delay_ids))
  
  domain <- cbind(rep(0, 1 + 2 * n_delays), c(1, rep(Inf, 2 * n_delays)))
  
  data_packer <- make_augmented_data_packer(observed_dates)
  
  density <- make_datefixer_density(parameters, groups, delay_map,
                                    hyperparameters, data_packer)
  
  augmented_data_update <- make_augmented_data_update(observed_dates, 
                                                      parameters,
                                                      groups, delay_map,
                                                      control, density,
                                                      data_packer)
  
  likelihood <- monty::monty_model(
    list(parameters = parameters,
         domain = domain,
         density = density,
         augmented_data_update = augmented_data_update))
  
  prior <- make_prior(parameters, hyperparameters, domain)
  
  model <- likelihood + prior
  
  model$hyperparameters <- hyperparameters
  
  model$data_packer <- data_packer
  
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
  
  dates <- setdiff(names(data), c("id", "group"))
  
  delay_map$from <- match(delay_map$from, dates)
  delay_map$to <- match(delay_map$to, dates)
  
  delay_map
}

make_datefixer_density <- function(parameters, groups, delay_map,
                                   hyperparameters, data_packer) {
  
  density <- function(pars) {
    names(pars) <- parameters
    
    log_likelihood <- datefixer_log_likelihood(pars, groups, delay_map,
                                               data_packer)
  }
  
  density
}

#' @importFrom stats dbeta dexp
make_prior <- function(parameters, hyperparameters, domain) {
  monty::monty_model(
    list(
      parameters = parameters,
      density = function (pars) {
        names(pars) <- parameters
        
        lp_prob_error <- 
          dbeta(pars[["prob_error"]], hyperparameters$prob_error_shape1, 
                hyperparameters$prob_error_shape2, log = TRUE)
        
        lp_mean_delays <-
          dexp(pars[grepl("^mean_delay", names(pars))], 
               1 / hyperparameters$mean_delay_scale, log = TRUE)
        
        lp_cv_delays <-
          dexp(pars[grepl("^cv_delay", names(pars))], 
               1 / hyperparameters$cv_delay_scale, log = TRUE)
        
        lp_prob_error + sum(lp_mean_delays) + sum(lp_cv_delays)
        
      },
      domain = domain
    ))
}

datefixer_log_likelihood <- function(pars, groups, delay_map, data_packer) {
  augmented_data <- data_packer$unpack(attr(pars, "data"))
  
  ll_errors <- datefixer_log_likelihood_errors(pars["prob_error"], 
                                               augmented_data$error_indicators)
  
  ll_delays <- 
    datefixer_log_likelihood_delays(augmented_data$estimated_dates,
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

datefixer_log_likelihood_delays <- function(estimated_dates, groups, mean_delays,
                                            cv_delays, delay_map) {

  ll_by_delay <-
    vapply(seq_len(nrow(delay_map)), 
           function(i) datefixer_log_likelihood_delays1(estimated_dates,
                                                        groups,
                                                        mean_delays[i],
                                                        cv_delays[i],
                                                        delay_map[i, ]),
           numeric(1))
  
  sum(ll_by_delay)
  
}

#' @importFrom stats dgamma
datefixer_log_likelihood_delays1 <- function(estimated_dates, groups, mean_delay,
                                             cv_delay, delay_info) {
  
  shape <- (1 / cv_delay)^2
  scale <- mean_delay / shape 
  
  delay_individuals <- groups %in% unlist(delay_info$group)
  delay_values <- estimated_dates[delay_individuals, delay_info$to] -
    estimated_dates[delay_individuals, delay_info$from]
  
  sum(dgamma(delay_values, shape, scale = scale, log = TRUE))
}


make_augmented_data_update <- function(observed_dates, parameters, groups,
                                       delay_map, control, density_fn,
                                       data_packer) {
  augmented_data_update <- function(pars, rng) {
    augmented_data <- attr(pars, "data")
    
    if (is.null(augmented_data)) {
      ## augmented data does not exist, so we initialise it
      names(pars) <- parameters
      augmented_data <- initialise_augmented_data(observed_dates, pars, groups,
                                                  delay_map, control, rng)
      augmented_data <- data_packer$pack(augmented_data)
      attr(pars, "data") <- augmented_data
      
      density <- density_fn(pars)
    } else {
      augmented_data <- data_packer$unpack(augmented_data)
      augmented_data <- update_augmented_data(augmented_data, observed_dates,
                                              pars, groups, delay_map, control,
                                              rng)
      augmented_data <- data_packer$pack(augmented_data)
      attr(pars, "data") <- augmented_data
      density <- density_fn(pars)
    }
    
    list(data = augmented_data, density = density)
  } 
  augmented_data_update  
}

observed_dates_to_int <- function(data) {
  dates <- setdiff(names(data), c("id", "group"))
  
  observed_dates <- data_frame_to_array(data[, dates])
  
  date_to_int(observed_dates)
}
  

make_augmented_data_packer <- function(observed_dates) {
  monty::monty_packer(array = list(estimated_dates = dim(observed_dates),
                                   error_indicators = dim(observed_dates)))
}
