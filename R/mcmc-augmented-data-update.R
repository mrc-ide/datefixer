update_augmented_data <- function(augmented_data, observed_dates, pars, groups,
                                  delay_info, control, rng) {

  for (i in seq_len(nrow(observed_dates))) {
    augmented_data_i <- 
      update_augmented_data1(augmented_data$estimated_dates[i, ],
                             augmented_data$error_indicators[i, ],
                             observed_dates[i, ], pars, groups[i], delay_info,
                             control, rng)
    augmented_data$estimated_dates[i, ] <- augmented_data_i$estimated_dates
    augmented_data$error_indicators[i, ] <- augmented_data_i$error_indicators
  }
  
  augmented_data
}

# Updating the augmented data for one individual
update_augmented_data1 <- function(estimated_dates, error_indicators,
                                   observed_dates, pars, group, delay_info,
                                   control, rng) {
  
  estimated_dates <- update_estimated_dates(estimated_dates, error_indicators,
                                            observed_dates, pars, group,
                                            delay_info, control, rng)
  
  #TODO: Add update_error_indicators()
  #TODO: Consider having moveE/swapE here?
  
  list(estimated_dates = estimated_dates,
       error_indicators = error_indicators)
  
}

# Updating all the relevant estimated dates for one individual
update_estimated_dates <- function(estimated_dates, error_indicators,
                                   observed_dates, pars, group, delay_info,
                                   control, rng) {
  
  mean_delays <- pars[grepl("^mean_delay", names(pars))]
  cv_delays <- pars[grepl("^cv_delay", names(pars))] 

  for (i in seq_along(observed_dates)) {
    estimated_dates <- update_estimated_dates1(i, estimated_dates,
                                               error_indicators, observed_dates,
                                               mean_delays, cv_delays, group,
                                               delay_info, control, rng)
  }
  
  estimated_dates
}

# Updating one of the estimated dates for an individual
update_estimated_dates1 <- function(i, estimated_dates, error_indicators,
                                    observed_dates, mean_delays, cv_delays,
                                    group, delay_info, control, rng) {
  
  ## TRUE/FALSE is each delay relevant to the group
  is_delay_in_group <- delay_info$is_delay_in_group[, group]
  ## TRUE/FALSE is date i for the given group involved in each relevant delay
  is_date_in_delay <- is_delay_in_group &
    (i == delay_info$from | i == delay_info$to)

  if (!any(is_date_in_delay)) {
    ## date is not associated with any delays for that group, so no update
    return(estimated_dates)
  }
  
  ## current log likelihood
  ll_current <- datefixer_log_likelihood_delays1(
    estimated_dates, mean_delays, cv_delays, delay_info$from, delay_info$to,
    is_delay_in_group)
  
  
  estimated_dates_new <- 
    propose_estimated_date(i, estimated_dates, error_indicators, observed_dates,
                           mean_delays, cv_delays, delay_info, is_date_in_delay,
                           rng)
  
  if (isTRUE(error_indicators[i]) & floor(estimated_dates_new[i]) == observed_dates[i]) {
    estimated_dates_new[i] <- estimated_dates[i]
  }
  
  ## proposed log likelihood
  ll_proposed <- datefixer_log_likelihood_delays1(
    estimated_dates_new, mean_delays, cv_delays, delay_info$from, delay_info$to,
    is_delay_in_group)
  
  ## accept/reject
  ratio_post <- sum(ll_proposed[is_date_in_delay]) - sum(ll_current[is_date_in_delay])
  
  ## handle invalid proposals
  if (is.infinite(ratio_post) & ratio_post > 0) {
    return(estimated_dates)
  }
  
  accept <- log(monty::monty_random_real(rng)) < ratio_post
  if (accept) {
    ## reject -> restore original date
    estimated_dates <- estimated_dates_new
  }
  
  estimated_dates
}


# Sample new date using randomly selected delay
sample_from_delay <- function(i, estimated_dates, mean_delays, cv_delays,
                              delay_info, is_date_in_delay, rng) {
  
  ## Which delays involve this date
  which_delays <- which(is_date_in_delay)
  
  ## If it is involved in several delays, randomly select one
  if (length(which_delays) > 1) {
    delay_idx <- ceiling(length(which_delays) * monty::monty_random_real(rng))
    selected_delay <- which_delays[delay_idx]
  } else {
    selected_delay <- which_delays
  }

  ## Is date i the 'from' or 'to' in this delay
  is_from <- (i == delay_info$from[selected_delay])
  
  ## Find the other date in this date pair
  if (is_from) {
    other_date_idx <- delay_info$to[selected_delay]
    other_date <- estimated_dates[other_date_idx]
    ## proposed date = other_date - delay
    ## so delay = other_date - proposed_date
    sign <- -1
  } else {
    other_date_idx <- delay_info$from[selected_delay]
    other_date <- estimated_dates[other_date_idx]
    ## proposed date = other_date + delay  
    ## so delay = proposed_date - other_date
    sign <- 1
  }
  
  ## Sample a delay from the marginal posterior (gamma distribution)
  mean_delay <- mean_delays[selected_delay]
  cv_delay <- cv_delays[selected_delay]
  
  shape <- 1 / (cv_delay^2)
  rate <- shape / mean_delay
  
  sampled_delay <- monty::monty_random_gamma_rate(shape, rate, rng)
  
  ## Calculate proposed date based on the sampled delay
  proposed_date <- other_date + sign * sampled_delay
  
  proposed_date
  
}

## proposed estimated date i for an individual
propose_estimated_date <- function(i, estimated_dates, error_indicators,
                                   observed_dates, mean_delays, cv_delays,
                                   delay_info, is_date_in_delay, rng) {
  
  if (isFALSE(error_indicators[i])) {
    ## non-error - propose new value uniformly over observed date
    estimated_dates[i] <- observed_dates[i] + monty::monty_random_real(rng)
  } else {
    ## error or missing - propose new value using delays
    estimated_dates[i] <- 
      sample_from_delay(i, estimated_dates, mean_delays, cv_delays,
                        delay_info, is_date_in_delay, rng)
  }
  
  estimated_dates
}
