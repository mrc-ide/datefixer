update_augmented_data <- function(augmented_data, observed_dates, pars, groups,
                                  delay_info, control, rng) {
  
  n_delays <- length(delay_info$from)
  delay_info$mean <- pars[paste0("mean_delay", seq_len(n_delays))]
  delay_info$cv <- pars[paste0("cv_delay", seq_len(n_delays))]

  for (i in seq_len(nrow(observed_dates))) {
    augmented_data_i <- lapply(augmented_data, function(x) x[i, ])
    augmented_data_i <- 
      update_augmented_data1(augmented_data_i, observed_dates[i, ], groups[i],
                             delay_info, control, rng)
    augmented_data$estimated_dates[i, ] <- augmented_data_i$estimated_dates
    augmented_data$error_indicators[i, ] <- augmented_data_i$error_indicators
  }
  
  augmented_data
}


# Updating the augmented data for one individual
update_augmented_data1 <- function(augmented_data, observed_dates, group,
                                   delay_info, control, rng) {
  
  augmented_data <- update_estimated_dates(augmented_data, observed_dates,
                                           group, delay_info, control, rng)
  
  #TODO: Add update_error_indicators()
  #TODO: Consider having moveE/swapE here?
  
  augmented_data
}


# Updating all the relevant estimated dates for one individual
update_estimated_dates <- function(augmented_data, observed_dates, group,
                                   delay_info, control, rng) {

  for (i in seq_along(observed_dates)) {
    augmented_data <- 
      update_estimated_dates1(i, augmented_data, observed_dates, group,
                              delay_info, control, rng)
  }
  
  augmented_data
}


# Updating one of the estimated dates for an individual
update_estimated_dates1 <- function(i, augmented_data, observed_dates, group,
                                    delay_info, control, rng) {
  
  ## TRUE/FALSE is each delay relevant to the group
  is_delay_in_group <- delay_info$is_delay_in_group[, group]
  ## TRUE/FALSE is date i for the given group involved in each relevant delay
  is_date_in_delay <- is_delay_in_group &
    (i == delay_info$from | i == delay_info$to)

  if (!any(is_date_in_delay)) {
    ## date is not associated with any delays for that group, so no update
    return(augmented_data)
  }
  
  augmented_data_new <- 
    propose_estimated_date(i, augmented_data, observed_dates, delay_info,
                           is_date_in_delay, rng)
  
  accept_prob <-
    calc_accept_prob_estimated_date(i, augmented_data_new, augmented_data,
                                    observed_dates, delay_info,
                                    is_delay_in_group, is_date_in_delay)
  
  accept <- log(monty::monty_random_real(rng)) < accept_prob
  if (accept) {
    augmented_data <- augmented_data_new
  }
  
  augmented_data
}


# Sample new date using randomly selected delay
sample_from_delay <- function(i, estimated_dates, delay_info, is_date_in_delay,
                              rng) {
  
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
  mean_delay <- delay_info$mean[selected_delay]
  cv_delay <- delay_info$cv[selected_delay]
  
  shape <- 1 / (cv_delay^2)
  rate <- shape / mean_delay
  
  sampled_delay <- monty::monty_random_gamma_rate(shape, rate, rng)
  
  ## Calculate proposed date based on the sampled delay
  proposed_date <- other_date + sign * sampled_delay
  
  proposed_date
  
}


## proposed estimated date i for an individual
propose_estimated_date <- function(i, augmented_data, observed_dates,
                                   delay_info, is_date_in_delay, rng) {
  
  if (isFALSE(augmented_data$error_indicators[i])) {
    ## non-error - propose new value uniformly over observed date
    proposed_date <- observed_dates[i] + monty::monty_random_real(rng)
  } else {
    ## error or missing - propose new value using delays
    proposed_date <- 
      sample_from_delay(i, augmented_data$estimated_dates, delay_info,
                        is_date_in_delay, rng)
  }
  
  augmented_data$estimated_dates[i] <- proposed_date
  
  augmented_data
}


## calculate the (log) acceptance probability for updating estimated_dates to
## estimated_dates_new where i is the updated date index
calc_accept_prob_estimated_date <- function(i, augmented_data_new,
                                            augmented_data, observed_dates,
                                            delay_info, is_delay_in_group,
                                            is_date_in_delay) {
  
  ## if error indicator is TRUE, and proposed estimated date is on observed date
  ## we will automatically reject
  reject <- isTRUE(augmented_data_new$error_indicators[i]) & 
    floor(augmented_data_new$estimated_dates[i]) == observed_dates[i]
  if (reject) {
    return(-Inf)
  }
  
  ## current log likelihood
  ll_current <- datefixer_log_likelihood_delays1(
    augmented_data$estimated_dates, delay_info$mean, delay_info$cv,
    delay_info$from, delay_info$to, is_delay_in_group)
  
  ## new log likelihood
  ll_new <- datefixer_log_likelihood_delays1(
    augmented_data_new$estimated_dates, delay_info$mean, delay_info$cv,
    delay_info$from, delay_info$to, is_delay_in_group)
  
  if (any(ll_new == Inf)) {
    ## ended up in a situation that such a small delay has been drawn that
    ## when recalculated from the dates it is essentially 0, let's reject for
    ## the moment
    return(-Inf)
  }
  
  ratio_post <- sum(ll_new) - sum(ll_current)

  ## No need to calculate proposal correction if ratio_post is -Inf
  if (ratio_post == -Inf) {
    return(-Inf)
  }
  
  prop_current <- calc_proposal_density(
    i, augmented_data, observed_dates, delay_info, is_date_in_delay)
  prop_new <- calc_proposal_density(
    i, augmented_data_new, observed_dates, delay_info, is_date_in_delay)
  ratio_prop <- prop_current - prop_new
  
  ratio_post + ratio_prop
}


calc_proposal_density <- function(i, augmented_data, observed_dates,
                                  delay_info, is_date_in_delay) {
  
  if (isFALSE(augmented_data$error_indicators[i])) {
    ## non-error - proposal is uniform over one day so log-density is 0
    d <- 0
  } else {
    ## error or missing - proposal is based on delay(s)
    shape <- 1 / (delay_info$cv[is_date_in_delay]^2)
    rate <- shape / delay_info$mean[is_date_in_delay]
    delay_from <- delay_info$from[is_date_in_delay]
    delay_to <- delay_info$to[is_date_in_delay]
    delay_values <- augmented_data$estimated_dates[delay_to] - 
      augmented_data$estimated_dates[delay_from]
    if (sum(is_date_in_delay) == 1) {
      ## single delay involving date i
      d <- dgamma(delay_values, shape, rate, log = TRUE)
    } else {
      ## multiple delays involving date i, so delay selected at random
      d <- log(sum(dgamma(delay_values, shape, rate))) - 
        log(sum(is_date_in_delay))
    }
  }
  
  d
}
