update_augmented_data <- function(augmented_data, observed_dates, pars, groups,
                                  model_info, date_range, control, rng) {
  
  delay_pars <- unpack_delay_pars(pars, model_info$delay_info$distribution)
  prob_error <- pars[["prob_error"]]

  for (g in seq_along(model_info$group_info)) {
    i_group <- which(groups == g)
    group_info <- model_info$group_info[[g]]
    augmented_data <- 
      update_augmented_data1(i_group, augmented_data, observed_dates, 
                             prob_error, delay_pars, model_info$delay_info,
                             group_info, date_range, control, rng)
  }
  
  augmented_data
}


# Updating the augmented data for one group
update_augmented_data1 <- function(i_group, augmented_data, observed_dates,
                                   prob_error, delay_pars, delay_info,
                                   group_info, date_range, control, rng) {

  augmented_data <-
    update_estimated_dates(i_group, augmented_data, observed_dates, prob_error,
                           delay_pars, delay_info, group_info, date_range,
                           control, rng)
  
  augmented_data <-
    update_error_indicators(i_group, augmented_data, observed_dates, prob_error,
                            delay_pars, delay_info, group_info, date_range,
                            control, rng)
  
  augmented_data <-
    swap_error_indicators(i_group, augmented_data, observed_dates, prob_error,
                          delay_pars, delay_info, group_info, date_range,
                          control, rng)

  augmented_data
}


# Updating all the relevant estimated dates for one individual
update_estimated_dates <- function(i_group, augmented_data, observed_dates,
                                   prob_error, delay_pars, delay_info,
                                   group_info, date_range, control, rng) {

  for (d in seq_len(ncol(observed_dates))) {
    update <- group_info$is_date_in_group[d]
    if (update) {
      augmented_data <- 
        update_estimated_dates1(d, i_group, augmented_data, observed_dates,
                                prob_error, delay_pars, delay_info,
                                group_info, date_range, control, rng)
    }
  }
  
  augmented_data
}


# Updating one of the estimated dates for a group
update_estimated_dates1 <- function(date, i_group, augmented_data,
                                    observed_dates, prob_error, delay_pars, 
                                    delay_info, group_info, date_range, 
                                    control, rng) {
  
  group_size <- length(i_group)
  ## update each with probability prob_update_estimated_dates
  update <- monty::monty_random_n_real(group_size, rng) < 
    control$prob_update_estimated_dates
  i_update <- i_group[update]
  if (length(i_update) == 0) {
    return(augmented_data)
  }
  
  if (control$cascade_sampling) {
    sampling_order <- 
      lapply(i_update, 
             function(i) {
               calc_cascade_sampling_order(date, group_info$event_order,
                                           augmented_data$error_indicators[i, ],
                                           group_info$is_date_connected,
                                           group_info$shortest_paths)
             })
      
  } else {
    sampling_order <- rep(list(date), length(i_update))
  }
  sampling_order_reverse <- sampling_order
  
  estimated_dates_new <- 
    t(vapply(seq_along(i_update),
           function(i) {
             propose_estimated_dates(
               sampling_order[[i]], 
               augmented_data$estimated_dates[i_update[i], ], 
               augmented_data$error_indicators[i_update[i], ],
               observed_dates[i_update[i], ], delay_pars,
               delay_info, group_info, date_range, rng)
           }, numeric(ncol(observed_dates))))
    
  
  accept_prob <-
    calc_accept_prob(sampling_order, sampling_order_reverse, 
                     estimated_dates_new, 
                     augmented_data$estimated_dates[i_update, , drop = FALSE], 
                     augmented_data$error_indicators[i_update, , drop = FALSE],
                     augmented_data$error_indicators[i_update, , drop = FALSE],
                     observed_dates[i_update, , drop = FALSE], prob_error, 
                     delay_pars, delay_info, group_info, date_range)

  accept <- log(monty::monty_random_n_real(length(i_update), rng)) < accept_prob
  i_accept <- i_update[accept]
  if (length(i_accept) > 0) {
    augmented_data$estimated_dates[i_accept, ] <- estimated_dates_new[accept, ]
  }
  
  augmented_data
}


# Updating all the relevant error indicators (and corresponding estimated dates)
# for one individual
update_error_indicators <- function(i_group, augmented_data, observed_dates,
                                    prob_error, delay_pars, delay_info,
                                    group_info, date_range, control, rng) {
  
  for (d in seq_len(ncol(observed_dates))) {
    update <- group_info$is_date_in_group[d]
    if (update) {
      augmented_data <- 
        update_error_indicators1(d, i_group, augmented_data, observed_dates, 
                                 prob_error, delay_pars, delay_info, group_info,
                                 date_range, control, rng)
    }
  }
  
  augmented_data
}


# Updating one of the error indicators (and corresponding estimated date) for an
# individual
update_error_indicators1 <- function(date, i_group, augmented_data, 
                                     observed_dates, prob_error, delay_pars, 
                                     delay_info, group_info, date_range,
                                     control, rng) {
  
  group_size <- length(i_group)
  
  ## we check if error indicator is non-NA (so date is non-missing)
  ## if FALSE, no update
  ## if TRUE, update with probability prob_update_error_indicators
  
  update <- monty::monty_random_n_real(group_size, rng) < 
    control$prob_update_error_indicators &
    !is.na(augmented_data$error_indicators[i_group, date])
  i_update <- i_group[update]
  if (length(i_update) == 0) {
    return(augmented_data)
  }
 
  error_indicators_new <- 
    change_error_indicators(augmented_data$error_indicators, date, i_update)
  
  if (control$cascade_sampling) {
    sampling_order <- 
      lapply(seq_along(i_update), 
             function(i) {
               calc_cascade_sampling_order(date, group_info$event_order,
                                           error_indicators_new[i, ],
                                           group_info$is_date_connected,
                                           group_info$shortest_paths)
             })
    sampling_order_reverse <- 
      lapply(i_update, 
             function(i) {
               calc_cascade_sampling_order(date, group_info$event_order,
                                           augmented_data$error_indicators[i, ],
                                           group_info$is_date_connected,
                                           group_info$shortest_paths)
             })
    
  } else {
    sampling_order <- rep(list(date), length(i_update))
    sampling_order_reverse <- sampling_order
  }
  
  estimated_dates_new <- 
    t(vapply(seq_along(i_update),
             function(i) {
               propose_estimated_dates(
                 sampling_order[[i]], 
                 augmented_data$estimated_dates[i_update[i], ], 
                 error_indicators_new[i, ],
                 observed_dates[i_update[i], ], delay_pars, 
                 delay_info, group_info, date_range, rng)
             }, numeric(ncol(observed_dates))))
  
  
  accept_prob <-
    calc_accept_prob(sampling_order, sampling_order_reverse, 
                     estimated_dates_new, 
                     augmented_data$estimated_dates[i_update, , drop = FALSE], 
                     error_indicators_new,
                     augmented_data$error_indicators[i_update, , drop = FALSE],
                     observed_dates[i_update, , drop = FALSE], prob_error, 
                     delay_pars, delay_info, group_info, date_range)
  
  accept <- log(monty::monty_random_n_real(length(i_update), rng)) < accept_prob
  i_accept <- i_update[accept]
  if (length(i_accept) > 0) {
    augmented_data$estimated_dates[i_accept, ] <- estimated_dates_new[accept, ]
    augmented_data$error_indicators[i_accept, ] <- 
      error_indicators_new[accept, ]
  }
  
  augmented_data
}


# Sample new date using randomly selected delay
sample_from_delay <- function(i, estimated_dates, error_indicators, delay_pars, 
                              delay_info, group_info, date_range, rng) {
  is_date_connected <- group_info$is_date_connected[i, ]
  delay_connecting_dates <- group_info$delay_connecting_dates[i, ]
  is_date_available <- !is.na(estimated_dates)
  
  delays_for_sampling <- 
    delay_connecting_dates[is_date_available & is_date_connected]
  
  ## Filter to only delays where the other date is available (needed for swap)
  if (length(delays_for_sampling) == 0) {
    ## not possible to sample from a delay so sample a random date
    d <- as.numeric(date_range)
    proposed_date <- monty::monty_random_uniform(d[1L], d[2L], rng)
  } else {
    ## If it is involved in several delays, randomly select one
    delay_idx <- if (length(delays_for_sampling) == 1) 1 else 
      ceiling(length(delays_for_sampling) * monty::monty_random_real(rng))
    selected_delay <- delays_for_sampling[delay_idx]
    
    delay_from <- delay_info$from[selected_delay]
    
    ## Is date i the 'from' or 'to' in this delay
    is_from <- (i == delay_from)
    
    if (is_from) {
      ## proposed date = other_date - delay
      ## so delay = other_date - proposed_date
      sign <- -1
      ## Find the other date in this date pair
      delay_to <- delay_info$to[selected_delay]
      other_date <- estimated_dates[delay_to]
    } else {
      ## proposed date = other_date + delay  
      ## so delay = proposed_date - other_date
      sign <- 1
      ## Find the other date in this date pair
      other_date <- estimated_dates[delay_from]
    }
    
    ## Sample a delay from the marginal posterior
    pars <- delay_pars[[selected_delay]]
    distribution <- delay_info$distribution[selected_delay]
    
    sampled_delay <- sample_from_delay1(pars, distribution, rng)
    
    ## Calculate proposed date based on the sampled delay
    proposed_date <- other_date + sign * sampled_delay
  }
  
  
  proposed_date
  
}


sample_from_delay1 <- function(pars, distribution, rng) {
  
  if (distribution == "gamma") {
    rate <- pars$shape / pars$mean
    x <- monty::monty_random_gamma_rate(pars$shape, rate, rng)
  } else if (distribution == "log-normal") {
    x <- monty::monty_random_log_normal(pars$meanlog, 
                                        1 / sqrt(pars$precisionlog), rng)
  } 
  
  x
}


# propose new estimated dates for date indices in to_update
propose_estimated_dates <- function(sampling_order, estimated_dates,
                                    error_indicators, observed_dates, 
                                    delay_pars, delay_info, group_info, 
                                    date_range, rng) {
  
  estimated_dates[sampling_order] <- NA
  
  for (i in sampling_order) {
    if (isFALSE(error_indicators[i])) {
      estimated_dates[i] <- observed_dates[i] + monty::monty_random_real(rng)
    } else {
      estimated_dates[i] <- 
        sample_from_delay(i, estimated_dates, error_indicators, delay_pars, 
                          delay_info, group_info, date_range, rng)
    }
  }
  
  estimated_dates
}


## calculate the (log) acceptance probability for updating augmented_data to
## augmented_data_new where updated is the indices of the updated date(s)
calc_accept_prob <- function(sampling_order, sampling_order_reverse,
                             estimated_dates_new, estimated_dates,
                             error_indicators_new, error_indicators,
                             observed_dates, prob_error, delay_pars,
                             delay_info, group_info, date_range) {
  
  is_delay_in_group <- group_info$is_delay_in_group

  accept_prob <- rep(-Inf, length(sampling_order))
  
  ## are error indicators TRUE with estimated date matching observed date
  incompatible_error_and_date <-
    !is.na(error_indicators_new) & error_indicators_new == TRUE &
    (floor(estimated_dates_new) == observed_dates)
  ## are estimated dates outside the date range 
  date_outside_range <- 
    estimated_dates_new < date_range[1] | estimated_dates_new >= date_range[2]
  reject <- 
    rowSums(incompatible_error_and_date | date_outside_range, na.rm = TRUE) > 0
  
  if (all(reject)) {
    return(accept_prob)
  }
  
  ## new delays log likelihood
  ll_delays_new <- chronofix_log_likelihood_delays1(
    estimated_dates_new[!reject, , drop = FALSE], delay_pars,
    delay_info, is_delay_in_group)
  
  ## Covering two cases here:
  ## 1. a proposed delay is negative so we want to auto-reject
  ## 2. we haveended up in a situation that such a small delay has been drawn
  ##    that when recalculated from the dates it is essentially 0 and 
  ##    distribution has infinite density at 0 (CV > 1). Let's reject for
  ##    the moment
  is_ll_infinite <- rowSums(is.infinite(ll_delays_new)) > 0
  reject[!reject] <- is_ll_infinite
  ll_delays_new <- ll_delays_new[!is_ll_infinite, , drop = FALSE]
  if (nrow(ll_delays_new) == 0) {
    return(accept_prob)
  }
  
  ## current delays log likelihood
  ll_delays_current <- chronofix_log_likelihood_delays1(
    estimated_dates[!reject, , drop = FALSE], delay_pars, 
    delay_info, is_delay_in_group)
  
  ratio_ll_delays <- rowSums(ll_delays_new - ll_delays_current)
  
  if (identical(error_indicators, error_indicators_new)) {
    ## can skip errors log likelihood calculation
    ratio_ll_errors <- 0 
  } else {
    ## current errors log likelihood
    ll_errors_current <- chronofix_log_likelihood_errors(
      prob_error, error_indicators[!reject, , drop = FALSE], date_range)
    ## new errors log likelihood
    ll_errors_new <- chronofix_log_likelihood_errors(
      prob_error, error_indicators_new[!reject, , drop = FALSE], date_range)
    
    ratio_ll_errors <- ll_errors_new - ll_errors_current
  }
  
  ratio_post <- ratio_ll_delays + ratio_ll_errors

  ## No need to calculate proposal correction if ratio_post is -Inf
  is_ratio_post_neg_inf <- ratio_post == -Inf
  reject[!reject] <- is_ratio_post_neg_inf
  ratio_post <- ratio_post[!is_ratio_post_neg_inf]
  if (length(ratio_post) == 0) {
    return(accept_prob)
  }
  
  ll_delays_new <- ll_delays_new[!is_ratio_post_neg_inf, , drop = FALSE]
  ll_delays_current <- ll_delays_current[!is_ratio_post_neg_inf, , drop = FALSE]
  
  i_prop_to_calc <- which(!reject)
  prop_current <- 
    vapply(seq_along(i_prop_to_calc),
           function(i) {
             calc_proposal_density(sampling_order_reverse[[i_prop_to_calc[i]]],
                                   error_indicators[i_prop_to_calc[i], ],
                                   ll_delays_current[i, ], 
                                   group_info, date_range)
           }, numeric(1))
  prop_new <- 
    vapply(seq_along(i_prop_to_calc),
           function(i) {
             calc_proposal_density(sampling_order[[i_prop_to_calc[i]]],
                                   error_indicators_new[i_prop_to_calc[i], ],
                                   ll_delays_new[i, ], 
                                   group_info, date_range)
           }, numeric(1))
  ratio_prop <- prop_current - prop_new
  
  accept_prob[!reject] <- ratio_post + ratio_prop
  
  accept_prob
}


calc_proposal_density <- function(sampling_order, error_indicators, ll_delays,
                                  group_info, date_range) {
  is_date_in_group <- group_info$is_date_in_group
  is_date_connected <- group_info$is_date_connected
  delay_connecting_dates <- group_info$delay_connecting_dates
  
  is_resampled <- seq_along(error_indicators) %in% sampling_order
  is_date_available <- is_date_in_group & !is_resampled
  
  d <- rep(0, length(sampling_order))
  
  for (j in seq_along(sampling_order)) {
    
    i <- sampling_order[j]
    
    ## if non-error (FALSE) - proposal is uniform over one day so log-density 
    ## is 0, hence only need to calculate for error (TRUE) or missing (NA)
    
    if (!isFALSE(error_indicators[i])) {
      ## which dates were available for sampling
      delays_for_sampling <- 
        delay_connecting_dates[i, is_date_available & is_date_connected[i, ]]
      
      if (length(delays_for_sampling) == 0) {
        ## no connected date available so just sampled from date range
        d[j] <- -log(date_range[2L] - date_range[1L])
      } else {
        ## error or missing - proposal is based on delay(s)
        if (length(delays_for_sampling) == 1) {
          ## single delay involving date i
          d[j] <- ll_delays[delays_for_sampling]
        } else {
          ## multiple delays involving date i, so delay selected at random
          d[j] <- log(sum(exp(ll_delays[delays_for_sampling]))) - 
            log(length(delays_for_sampling))
        }
      }
      
    }
    
    is_date_available[i] <- TRUE
  }
  
  sum(d)
}


## Swap -----------------------------------------------------------------------

# Check for individuals with at least one error and non-error (exclude missing)
has_mixed_errors <- function(error_indicators) {
  length(unique(error_indicators[!is.na(error_indicators)])) == 2
}


# Swap error indicators for one group
swap_error_indicators <- function(i_group, augmented_data, observed_dates,
                                  prob_error, delay_pars, delay_info,
                                  group_info, date_range, control, rng) {

  group_size <- length(i_group)
  event_order <- group_info$event_order
  
  ## we check if individual has mixed errors (at least one error and non-error)
  ## if FALSE, no update
  ## if TRUE, update with probability prob_error_swap
  
  update <- monty::monty_random_n_real(group_size, rng) < 
    control$prob_error_swap &
    vapply(i_group, 
           function (i) has_mixed_errors(augmented_data$error_indicators[i, ]),
           logical(1))
  i_update <- i_group[update]
  if (length(i_update) == 0) {
    return(augmented_data)
  }
  
  error_indicators_new <- 
    change_error_indicators(augmented_data$error_indicators, 
                            event_order, i_update)
  
  sampling_order <- 
    lapply(seq_along(i_update), 
           function(i) {
             calc_batch_sampling_order(
               event_order, error_indicators_new[i, ],
               group_info$is_date_connected)
           })
  
  sampling_order_reverse <- 
    lapply(i_update, 
           function(i) {
             calc_batch_sampling_order(
               event_order, augmented_data$error_indicators[i, ],
               group_info$is_date_connected)
           })
  
  # systematically sample new errors and missing dates based on new non-errors
  estimated_dates_new <- 
    t(vapply(seq_along(i_update),
             function(i) {
               propose_estimated_dates(
                 sampling_order[[i]], 
                 augmented_data$estimated_dates[i_update[i], ], 
                 error_indicators_new[i, ],
                 observed_dates[i_update[i], ], delay_pars, 
                 delay_info, group_info, date_range, rng)
             }, numeric(ncol(observed_dates))))
  
  
  accept_prob <-
    calc_accept_prob(sampling_order, sampling_order_reverse, 
                     estimated_dates_new, 
                     augmented_data$estimated_dates[i_update, , drop = FALSE], 
                     error_indicators_new,
                     augmented_data$error_indicators[i_update, , drop = FALSE],
                     observed_dates[i_update, , drop = FALSE], prob_error, 
                     delay_pars, delay_info, group_info, date_range)
  
  accept <- log(monty::monty_random_n_real(length(i_update), rng)) < accept_prob
  i_accept <- i_update[accept]
  if (length(i_accept) > 0) {
    augmented_data$estimated_dates[i_accept, ] <- estimated_dates_new[accept, ]
    augmented_data$error_indicators[i_accept, ] <- 
      error_indicators_new[accept, ]
  }
  
  augmented_data

}


calc_batch_sampling_order <- function(to_resample, error_indicators,
                                      is_date_connected) {
  
  if (length(to_resample) == 1) {
    return(to_resample)
  }
  
  ## resample non-errors first
  err_ind <- error_indicators[to_resample]
  is_non_error <- !err_ind & !is.na(err_ind)
  sampling_order <- to_resample[is_non_error]
  
  remaining_to_resample <- to_resample[!is_non_error]
  
  if (length(remaining_to_resample) > 0) {
    
    while (length(remaining_to_resample) > 1) {
      # Find all dates connected to available dates
      is_connected <- 
        rowSums(is_date_connected[remaining_to_resample, sampling_order,
                                  drop = FALSE]) > 0
      connected_dates <- remaining_to_resample[is_connected]
      
      # Earliest connected event according to sampling_order
      earliest_idx <- which(remaining_to_resample %in% connected_dates)[1]
      date_to_sample <- remaining_to_resample[earliest_idx]
      
      # Update resampling order and remove from remaining
      sampling_order <- c(sampling_order, date_to_sample)
      remaining_to_resample <- remaining_to_resample[-earliest_idx]
    }
    
    sampling_order <- c(sampling_order, remaining_to_resample)
  }
  
  sampling_order
}


calc_cascade_sampling_order <- function(i, event_order,
                                        error_indicators, is_date_connected,
                                        shortest_paths) {
  
  ## Check if there are any correct dates, if not then we do not cascade
  is_possible_anchor <- visFALSE(error_indicators[event_order])
  
  if (is_possible_anchor[event_order == i]) {
    ## Date i is correct so use itself as an anchor
    sampling_order <- i
  } else {
    has_anchor <- any(is_date_connected[i, event_order] & is_possible_anchor)
    if (has_anchor) {
      ## Date is connected to an anchor
      sampling_order <- i
    } else {
      possible_anchors <- event_order[is_possible_anchor]
      shortest_path_lengths <- 
        vnapply(shortest_paths[[i]][possible_anchors], length)
      if (all(shortest_path_lengths == 0)) {
        ## No path to an anchor so cascade from i
        sampling_order <- i
      } else {
        possible_anchors <- possible_anchors[shortest_path_lengths > 0]
        shortest_path_lengths <- 
          shortest_path_lengths[shortest_path_lengths > 0]
        anchor <- possible_anchors[which.min(shortest_path_lengths)]
        sampling_order <- rev(shortest_paths[[i]][[anchor]])[-1L]
      }
    }
  }
  
  ## cascade_candidates are dates not already in sampling_order that are
  ## missing or erroneous
  cascade_candidates <- event_order[!(event_order %in% sampling_order)]
  is_cascade_candidate <- !visFALSE(error_indicators[cascade_candidates])
  cascade_candidates <- cascade_candidates[is_cascade_candidate]
  
  while (length(cascade_candidates) > 0) {
    ## identify which of cascade_candidates are connected to dates in
    ## sampling_order
    is_cascadable <- rowSums(
      is_date_connected[cascade_candidates, sampling_order, drop = FALSE]) > 0
    to_cascade <- cascade_candidates[is_cascadable]
    if (length(to_cascade) == 0) {
      ## none connected so return
      return(sampling_order)
    } else {
      ## add connected cascade candidates to sampling_order and remove
      ## from cascade candidates
      sampling_order <- c(sampling_order, to_cascade)
      cascade_candidates <- cascade_candidates[!is_cascadable]
    }
  }
  
  sampling_order
}


change_error_indicators <- function(error_indicators, d, i) {
  error_indicators <- error_indicators[i, , drop = FALSE]
  error_indicators[, d] <- !error_indicators[, d]
  error_indicators
}
