update_augmented_data <- function(augmented_data, observed_dates, pars, groups,
                                  delay_info, date_range, control, rng) {
  
  n_delays <- length(delay_info$from)
  delay_info$mean <- unname(pars[paste0("mean_delay", seq_len(n_delays))])
  delay_info$cv <- unname(pars[paste0("cv_delay", seq_len(n_delays))])
  prob_error <- pars[["prob_error"]]

  for (i in seq_len(nrow(observed_dates))) {
    augmented_data_i <- lapply(augmented_data, function(x) x[i, ])
    augmented_data_i <- 
      update_augmented_data1(augmented_data_i, observed_dates[i, ], groups[i],
                             prob_error, delay_info, date_range, control, rng)
    augmented_data$estimated_dates[i, ] <- augmented_data_i$estimated_dates
    augmented_data$error_indicators[i, ] <- augmented_data_i$error_indicators
  }
  
  augmented_data
}


# Updating the augmented data for one individual
update_augmented_data1 <- function(augmented_data, observed_dates, group,
                                   prob_error, delay_info, date_range,
                                   control, rng) {

  augmented_data <- update_estimated_dates(augmented_data, observed_dates,
                                           group, prob_error, delay_info,
                                           date_range, control, rng)
  
  augmented_data <- update_error_indicators(augmented_data, observed_dates,
                                            group, prob_error, delay_info,
                                            date_range, control, rng)
  
  augmented_data <- swap_error_indicators(augmented_data, observed_dates,
                                          group, prob_error, delay_info,
                                          date_range, control, rng)

  augmented_data
}


# Updating all the relevant estimated dates for one individual
update_estimated_dates <- function(augmented_data, observed_dates, group,
                                   prob_error, delay_info, date_range,
                                   control, rng) {

  for (i in seq_along(observed_dates)) {
    augmented_data <- 
      update_estimated_dates1(i, augmented_data, observed_dates, group,
                              prob_error, delay_info, date_range, control, rng)
  }
  
  augmented_data
}


# Updating one of the estimated dates for an individual
update_estimated_dates1 <- function(i, augmented_data, observed_dates, group,
                                    prob_error, delay_info, date_range, 
                                    control, rng) {
  
  ## we check if date i is in the given group
  ## if FALSE, no update
  ## if TRUE, update with probability prob_update_estimated_dates
  update <- delay_info$is_date_in_group[i, group] &&
    monty::monty_random_real(rng) < control$prob_update_estimated_dates
  if (!update) {
    return(augmented_data)
  }
  
  augmented_data_new <- 
    propose_estimated_dates(i, augmented_data, observed_dates, group,
                            delay_info, rng)
  
  accept_prob <-
    calc_accept_prob(i, augmented_data_new, augmented_data, observed_dates,
                     group, prob_error, delay_info, date_range)

  accept <- log(monty::monty_random_real(rng)) < accept_prob
  if (accept) {
    augmented_data <- augmented_data_new
  }
  
  augmented_data
}


# Updating all the relevant error indicators (and corresponding estimated dates)
# for one individual
update_error_indicators <- function(augmented_data, observed_dates, group,
                                    prob_error, delay_info, date_range,
                                    control, rng) {
  
  for (i in seq_along(observed_dates)) {
    augmented_data <- 
      update_error_indicators1(i, augmented_data, observed_dates, group,
                               prob_error, delay_info, date_range, control, rng)
  }
  
  augmented_data
}


# Updating one of the error indicators (and corresponding estimated date) for an
# individual
update_error_indicators1 <- function(i, augmented_data, observed_dates, group,
                                     prob_error, delay_info, date_range,
                                     control, rng) {
  
  ## we check if error indicator is non-NA (so date is non-missing)
  ## if FALSE, no update
  ## if TRUE, update with probability prob_update_error_indicators
  update <- !is.na(augmented_data$error_indicators[i]) &&
    monty::monty_random_real(rng) < control$prob_update_error_indicators
  if (!update) {
    return(augmented_data)
  }
  
  augmented_data_new <- 
    propose_estimated_dates(i, augmented_data, observed_dates, group,
                            delay_info, rng, TRUE)
  
  accept_prob <-
    calc_accept_prob(i, augmented_data_new, augmented_data, observed_dates,
                     group, prob_error, delay_info, date_range)

  accept <- log(monty::monty_random_real(rng)) < accept_prob
  if (accept) {
    augmented_data <- augmented_data_new
  }
  
  augmented_data
}


# Sample new date using randomly selected delay
sample_from_delay <- function(i, estimated_dates, group, delay_info,
                              rng) {
  
  is_date_in_delay <- delay_info$is_date_in_delay[i, , group]
  
  ## Which delays involve this date
  which_delays <- which(is_date_in_delay)
  other_date_idx <- ifelse(delay_info$from[which_delays] != i,
                           delay_info$from[which_delays], 
                           delay_info$to[which_delays])
  
  ## Filter to only delays where the other date is available (needed for swap)
  valid_delays <- which_delays[!is.na(estimated_dates[other_date_idx])]
  other_date_idx <- other_date_idx[!is.na(estimated_dates[other_date_idx])]
  
  
  ## If it is involved in several delays, randomly select one
  delay_idx <- if (length(valid_delays) == 1) 1 else 
    ceiling(length(valid_delays) * monty::monty_random_real(rng))
  selected_delay <- valid_delays[delay_idx]
  
  ## Find the other date in this date pair
  other_date <- estimated_dates[other_date_idx[delay_idx]]
  
  ## Is date i the 'from' or 'to' in this delay
  is_from <- (i == delay_info$from[selected_delay])
  
  if (is_from) {
    ## proposed date = other_date - delay
    ## so delay = other_date - proposed_date
    sign <- -1
  } else {
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



# propose new estimated dates for date indices in to_update
propose_estimated_dates <- function(to_update, augmented_data, observed_dates,
                                    group, delay_info, rng, 
                                    update_errors = FALSE) {
  
  if (update_errors) {
    augmented_data$error_indicators[to_update] <- 
      !augmented_data$error_indicators[to_update]
  }
  
  augmented_data$estimated_dates[to_update] <- NA
  
  resampling_order <- 
    calc_resampling_order(to_update, augmented_data$error_indicators,
                          delay_info$is_date_connected[, , group])
  
  for (i in resampling_order) {
    if (isFALSE(augmented_data$error_indicators[i])) {
      augmented_data$estimated_dates[i] <-
        observed_dates[i] + monty::monty_random_real(rng)
    } else {
      augmented_data$estimated_dates[i] <-
        sample_from_delay(i, augmented_data$estimated_dates, group,
                          delay_info, rng)
    }
  }
  
  augmented_data
}


## calculate the (log) acceptance probability for updating augmented_data to
## augmented_data_new where updated is the indices of the updated date(s)
calc_accept_prob <- function(updated, augmented_data_new, augmented_data,
                             observed_dates, group, prob_error, delay_info,
                             date_range) {
  
  is_delay_in_group <- delay_info$is_delay_in_group[, group]

  ## are error indicators TRUE with estimated date matching observed date
  incompatible_error_and_date <-
    !is.na(augmented_data_new$error_indicators[updated]) &
    augmented_data_new$error_indicators[updated] == TRUE &
    (floor(augmented_data_new$estimated_dates[updated]) == 
       observed_dates[updated])
  ## are estimated dates outside the date range 
  date_outside_range <- 
    augmented_data_new$estimated_dates[updated] < date_range[1] |
    augmented_data_new$estimated_dates[updated] >= date_range[2]
  reject <- any(incompatible_error_and_date) || any(date_outside_range)
  if (reject) {
    return(-Inf)
  }
  
  ## current delays log likelihood
  ll_delays_current <- datefixer_log_likelihood_delays1(
    augmented_data$estimated_dates, delay_info$mean, delay_info$cv,
    delay_info$from, delay_info$to, is_delay_in_group)
  ## new delays log likelihood
  ll_delays_new <- datefixer_log_likelihood_delays1(
    augmented_data_new$estimated_dates, delay_info$mean, delay_info$cv,
    delay_info$from, delay_info$to, is_delay_in_group)


  if (any(ll_delays_new == Inf)) {
    ## ended up in a situation that such a small delay has been drawn that
    ## when recalculated from the dates it is essentially 0, let's reject for
    ## the moment
    return(-Inf)
  }
  
  ## current errors log likelihood
  ll_errors_current <- datefixer_log_likelihood_errors(
    prob_error, augmented_data$error_indicators, date_range)
  ## new errors log likelihood
  ll_errors_new <- datefixer_log_likelihood_errors(
    prob_error, augmented_data_new$error_indicators, date_range)
  
  ratio_ll_delays <- sum(ll_delays_new) - sum(ll_delays_current)
  ratio_ll_errors <- ll_errors_new - ll_errors_current
  ratio_post <- ratio_ll_delays + ratio_ll_errors

  ## No need to calculate proposal correction if ratio_post is -Inf
  if (ratio_post == -Inf) {
    return(-Inf)
  }
  
  prop_current <- 
    calc_proposal_density(updated, augmented_data, group, delay_info)
  prop_new <- 
    calc_proposal_density(updated, augmented_data_new, group, delay_info)
  ratio_prop <- prop_current - prop_new
  
  ratio_post + ratio_prop
}


calc_proposal_density <- function(updated, augmented_data, group, delay_info) {
  
  is_date_in_delay <- delay_info$is_date_in_delay[, , group]
  is_date_in_group <- delay_info$is_date_in_group[, group]
  is_date_connected <- delay_info$is_date_connected[, , group]
  
  resampling_order <- 
    calc_resampling_order(updated, augmented_data$error_indicators,
                          is_date_connected)
  
  dates <- which(is_date_in_group)
  
  d <- rep(0, length(updated))
  
  for (j in seq_along(updated)) {
    
    i <- resampling_order[j]
    
    ## if non-error (FALSE) - proposal is uniform over one day so log-density 
    ## is 0, hence only need to calculate for error (TRUE) or missing (NA)
    
    if (!isFALSE(augmented_data$error_indicators[i])) {
      ## which dates were available for sampling
      available_dates <- 
        setdiff(dates, resampling_order[j:length(resampling_order)])
      is_delay_available <- 
        apply(is_date_in_delay[available_dates, , drop = FALSE], 2, any)
      ## which delays could be sampled from
      can_sample_from_delay <- is_date_in_delay[i, ] & 
        is_delay_available
      
      ## error or missing - proposal is based on delay(s)
      shape <- 1 / (delay_info$cv[can_sample_from_delay]^2)
      rate <- shape / delay_info$mean[can_sample_from_delay]
      delay_from <- delay_info$from[can_sample_from_delay]
      delay_to <- delay_info$to[can_sample_from_delay]
      delay_values <- augmented_data$estimated_dates[delay_to] - 
        augmented_data$estimated_dates[delay_from]
      
      if (sum(can_sample_from_delay) == 1) {
        ## single delay involving date i
        d[j] <- dgamma(delay_values, shape, rate, log = TRUE)
      } else {
        ## multiple delays involving date i, so delay selected at random
        d[j] <- log(sum(dgamma(delay_values, shape, rate))) - 
          log(sum(can_sample_from_delay))
      }
      
    }
    
  }
  
  sum(d)
}


## Swap -----------------------------------------------------------------------

# Check for individuals with at least one error and non-error (exclude missing)
has_mixed_errors <- function(error_indicators) {
  length(unique(error_indicators[!is.na(error_indicators)])) == 2
}


# Swap error indicators for one eligible individual
swap_error_indicators <- function(augmented_data, observed_dates, group,
                                  prob_error, delay_info, date_range,
                                  control, rng) {

  ## we check if individual has mixed errors (at least one error and non-error)
  ## if FALSE, no update
  ## if TRUE, update with probability prob_error_swap
  update <- has_mixed_errors(augmented_data$error_indicators) &&
    monty::monty_random_real(rng) < control$prob_error_swap
  if (!update) {
    return(augmented_data)
  } 
  
  event_order <- delay_info$event_order[[group]]

  ## TRUE/FALSE is date i for the given group involved in each relevant delay
  is_date_in_delay <- delay_info$is_date_in_delay[, , group]
  
  # systematically sample new errors and missing dates based on new non-errors
  augmented_data_new <- 
    propose_estimated_dates(event_order, augmented_data, observed_dates, group,
                            delay_info, rng, TRUE)

  accept_prob <-
    calc_accept_prob(event_order, augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, delay_info, date_range)
  
  accept <- log(monty::monty_random_real(rng)) < accept_prob
  if (accept) {
    augmented_data <- augmented_data_new
  }

  augmented_data

}


calc_resampling_order <- function(to_resample, error_indicators,
                                  is_date_connected) {
  
  if (length(to_resample) == 1) {
    return(to_resample)
  }
  
  ## resample non-errors first
  err_ind <- error_indicators[to_resample]
  is_non_error <- !err_ind & !is.na(err_ind)
  resampling_order <- to_resample[is_non_error]
  
  remaining_to_resample <- to_resample[!is_non_error]
  
  if (length(remaining_to_resample) > 0) {
    
    while (length(remaining_to_resample) > 1) {
      # Find all dates connected to available dates
      is_connected <- 
        rowSums(is_date_connected[remaining_to_resample, resampling_order,
                                  drop = FALSE]) > 0
      connected_dates <- remaining_to_resample[is_connected]
      
      # Earliest connected event according to resampling_order
      earliest_idx <- which(remaining_to_resample %in% connected_dates)[1]
      date_to_sample <- remaining_to_resample[earliest_idx]
      
      # Update resampling order and remove from remaining
      resampling_order <- c(resampling_order, date_to_sample)
      remaining_to_resample <- remaining_to_resample[-earliest_idx]
    }
    
    resampling_order <- c(resampling_order, remaining_to_resample)
  }
  
  resampling_order
}
