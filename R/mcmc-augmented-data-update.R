update_augmented_data <- function(augmented_data, observed_dates, pars, groups,
                                  delay_info, control, rng) {
  
  n_delays <- length(delay_info$from)
  delay_info$mean <- unname(pars[paste0("mean_delay", seq_len(n_delays))])
  delay_info$cv <- unname(pars[paste0("cv_delay", seq_len(n_delays))])
  prob_error <- pars[["prob_error"]]

  for (i in seq_len(nrow(observed_dates))) {
    augmented_data_i <- lapply(augmented_data, function(x) x[i, ])
    augmented_data_i <- 
      update_augmented_data1(augmented_data_i, observed_dates[i, ], groups[i],
                             prob_error, delay_info, control, rng)
    augmented_data$estimated_dates[i, ] <- augmented_data_i$estimated_dates
    augmented_data$error_indicators[i, ] <- augmented_data_i$error_indicators
  }
  
  augmented_data
}


# Updating the augmented data for one individual
update_augmented_data1 <- function(augmented_data, observed_dates, group,
                                   prob_error, delay_info, control, rng) {

  augmented_data <- update_estimated_dates(augmented_data, observed_dates,
                                           group, prob_error, delay_info,
                                           control, rng)
  
  augmented_data <- update_error_indicators(augmented_data, observed_dates,
                                            group, prob_error, delay_info,
                                            control, rng)
  
  #TODO: Consider having moveE/swapE here?
  
  augmented_data
}


# Updating all the relevant estimated dates for one individual
update_estimated_dates <- function(augmented_data, observed_dates, group,
                                   prob_error, delay_info, control, rng) {

  for (i in seq_along(observed_dates)) {
    augmented_data <- 
      update_estimated_dates1(i, augmented_data, observed_dates, group,
                              prob_error, delay_info, control, rng)
  }
  
  augmented_data
}


# Updating one of the estimated dates for an individual
update_estimated_dates1 <- function(i, augmented_data, observed_dates, group,
                                    prob_error, delay_info, control, rng) {
  
  ## TRUE/FALSE is each delay relevant to the group
  is_delay_in_group <- delay_info$is_delay_in_group[, group]
  ## TRUE/FALSE is date i for the given group involved in each relevant delay
  is_date_in_delay <- is_delay_in_group &
    (i == delay_info$from | i == delay_info$to)

  if (!any(is_date_in_delay)) {
    ## date is not associated with any delays for that group, so no update
    return(augmented_data)
  }
  
  update <- monty::monty_random_real(rng) < control$prob_update_estimated_dates
  if (!update) {
    return(augmented_data)
  }
  
  augmented_data_new <- 
    propose_estimated_date(i, augmented_data, observed_dates, delay_info,
                           is_date_in_delay, rng)
  
  accept_prob <-
    calc_accept_prob(i, augmented_data_new, augmented_data, observed_dates,
                     prob_error, delay_info, is_delay_in_group,
                     is_date_in_delay)

  accept <- log(monty::monty_random_real(rng)) < accept_prob
  if (accept) {
    augmented_data <- augmented_data_new
  }
  
  augmented_data
}


# Updating all the relevant error indicators (and corresponding estimated dates)
# for one individual
update_error_indicators <- function(augmented_data, observed_dates, group,
                                    prob_error, delay_info, control, rng) {
  
  for (i in seq_along(observed_dates)) {
    augmented_data <- 
      update_error_indicators1(i, augmented_data, observed_dates, group,
                               prob_error, delay_info, control, rng)
  }
  
  augmented_data
}


# Updating one of the error indicators (and corresponding estimated date) for an
# individual
update_error_indicators1 <- function(i, augmented_data, observed_dates, group,
                                     prob_error, delay_info, control, rng) {
  
  if (is.na(augmented_data$error_indicators[1])) {
    ## missing date, no error indicator update
    return(augmented_data)
  }
  
  ## TRUE/FALSE is each delay relevant to the group
  is_delay_in_group <- delay_info$is_delay_in_group[, group]
  ## TRUE/FALSE is date i for the given group involved in each relevant delay
  is_date_in_delay <- is_delay_in_group &
    (i == delay_info$from | i == delay_info$to)
  
  if (!any(is_date_in_delay)) {
    ## date is not associated with any delays for that group, so no update
    return(augmented_data)
  }
  
  update <- monty::monty_random_real(rng) < control$prob_update_error_indicators
  if (!update) {
    return(augmented_data)
  }
  
  augmented_data_new <- 
    propose_estimated_date(i, augmented_data, observed_dates, delay_info,
                           is_date_in_delay, rng, TRUE)
  
  accept_prob <-
    calc_accept_prob(i, augmented_data_new, augmented_data, observed_dates,
                     prob_error, delay_info, is_delay_in_group,
                     is_date_in_delay)

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


## propose estimated date i for an individual
propose_estimated_date <- function(i, augmented_data, observed_dates,
                                   delay_info, is_date_in_delay, rng,
                                   update_error = FALSE) {
  
  if (update_error) {
    augmented_data$error_indicators[i] <- !augmented_data$error_indicators[i]
  }

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


## calculate the (log) acceptance probability for updating augmented_data to
## augmented_data_new where i is the updated date index
calc_accept_prob <- function(i, augmented_data_new, augmented_data,
                             observed_dates, prob_error, delay_info, 
                             is_delay_in_group, is_date_in_delay) {

  ## if error indicator is TRUE, and proposed estimated date is on observed date
  ## we will automatically reject
  reject <- isTRUE(augmented_data_new$error_indicators[i]) & 
    floor(augmented_data_new$estimated_dates[i]) == observed_dates[i]
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
    prob_error, augmented_data$error_indicators)
  ## new errors log likelihood
  ll_errors_new <- datefixer_log_likelihood_errors(
    prob_error, augmented_data_new$error_indicators)
  
  ratio_ll_delays <- sum(ll_delays_new) - sum(ll_delays_current)
  ratio_ll_errors <- ll_errors_new - ll_errors_current
  ratio_post <- ratio_ll_delays + ratio_ll_errors

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

## Swap -----------------------------------------------------------------------

# Check for individuals with at least one error and non-error (exclude missing)
has_mixed_errors <- function(error_indicators) {
  length(unique(na.omit(error_indicators))) == 2
}

# Swap error indicators for one eligible individual
swap_error_indicators <- function(augmented_data, observed_dates,
                                  group, prob_error, delay_info,
                                  control, rng) {

  if (!has_mixed_errors(augmented_data$error_indicators)) return(augmented_data)
  
  update <- monty::monty_random_real(rng) < control$prob_error_swap
  if (!update) return(augmented_data)
  
  # identify relevant delays and event dates for a group
  delays_in_group <- delay_info$is_delay_in_group[, group]
  dates_from <- delay_info$from[delays_in_group]
  dates_to <- delay_info$to[delays_in_group]
  
  relevant_dates <- unique(c(dates_from, dates_to))
  delay_df <- data.frame(from = dates_from, to = dates_to)
  
  event_graph <- igraph::graph_from_data_frame(delay_df,
                                               directed = TRUE,
                                               vertices = relevant_dates)

  event_order <- as.numeric(names(igraph::topo_sort(event_graph)))

  # idx for dates to change
  new_non_errors <- which(augmented_data$error_indicators == TRUE)
  new_errors <- which(augmented_data$error_indicators == FALSE)
  missing <- relevant_dates[is.na(augmented_data$error_indicators[relevant_dates])]
  
  # errors to non-errors (i.e. observed dates correct)
  augmented_data_new <- swap_to_non_errors(augmented_data, observed_dates,
                                           new_non_errors, rng)
  
  # systematically sample new errors and missing dates based on new non-errors
  augmented_data_new <- resample_dates(augmented_data_new, new_errors, missing,
                                       event_order, delay_info, delays_in_group,
                                       rng)

  # TODO: accept/reject

  augmented_data_new

}

# Swap erroneous dates to non-error dates (i.e. observed dates)
swap_to_non_errors <- function(augmented_data, observed_dates,
                               new_non_errors, rng) {
  
  for (i in new_non_errors) {
  augmented_data$estimated_dates[i] <-
    observed_dates[i] + monty::monty_random_real(rng)
  }
  
  augmented_data$error_indicators[new_non_errors] <-
    !augmented_data$error_indicators[new_non_errors]
  
  augmented_data
}


# Swap non-error dates to erroneous dates and resample missing dates
resample_dates <- function(augmented_data, new_errors, missing, event_order,
                           delay_info, delays_in_group, rng) {

  indices_to_resample <- c(new_errors, missing)
  resampling_order <- event_order[event_order %in% indices_to_resample]
  augmented_data$estimated_dates[indices_to_resample] <- NA
  
  # Which dates do we have?
  available_dates <- which(!is.na(augmented_data$estimated_dates))
  remaining_to_resample <- indices_to_resample
  
  while (length(remaining_to_resample) > 0) {
    
    # Find all dates connected to available dates
    connected_dates <- c()
    for (date_idx in available_dates) {
      
      involving_delays <-
        which(delays_in_group &
                (date_idx == delay_info$from | date_idx == delay_info$to))
      
      # What are the "other" dates in those delay pairs
      for (d in involving_delays) {
        
        is_from <- (date_idx == delay_info$from[d])
        other_idx <- if (is_from) delay_info$to[d] else delay_info$from[d]
        
        if (other_idx %in% remaining_to_resample) {
          connected_dates <- c(connected_dates, other_idx)
        }
        
      }
    }
    
    connected_dates <- unique(connected_dates)
    
    # Earliest connected event according to resampling_order
    earliest_idx <- which(resampling_order %in% connected_dates)[1]
    date_to_sample <- resampling_order[earliest_idx]
    
    is_date_in_delay <- delays_in_group & 
      (date_to_sample == delay_info$from | date_to_sample == delay_info$to)
    
    augmented_data$estimated_dates[date_to_sample] <-
      sample_from_delay(date_to_sample, augmented_data$estimated_dates, 
                        delay_info, is_date_in_delay, rng)
    
    # After sampling update available dates and remove from remaining
    available_dates <- c(available_dates, date_to_sample)
    remaining_to_resample <- setdiff(remaining_to_resample, date_to_sample)
    resampling_order <- resampling_order[-earliest_idx]
  }
  
  augmented_data$error_indicators[new_errors] <-
    !augmented_data$error_indicators[new_errors]

  augmented_data
}




