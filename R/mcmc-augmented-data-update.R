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

update_augmented_data1 <- function(estimated_dates, error_indicators,
                                   observed_dates, pars, group, delay_info,
                                   control, rng) {
  
  estimated_dates <- update_estimated_dates(estimated_dates, error_indicators,
                                            observed_dates, pars, group,
                                            delay_info, control, rng)
  
  list(estimated_dates = estimated_dates,
       error_indicators = error_indicators)
  
}

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

update_estimated_dates1 <- function(i, estimated_dates, error_indicators,
                                    observed_dates, mean_delays, cv_delays,
                                    group, delay_info, control, rng) {
  
  ## TRUE/FALSE is each delay relevant to the group
  is_delay_in_group <- delay_info$is_delay_in_group[, group]
  ## TRUE/FALSE is date i for the given group involved in each delay
  is_date_in_delay <- is_delay_in_group &
    (i == delay_info$from | i == delay_info$to)
  
  if (!any(is_date_in_delay)) {
    ## date is not associated with any delays, so no update
    return(estimated_dates)
  }
  
  ## current log likelihood
  ll_current <- datefixer_log_likelihood_delays1(
    estimated_dates, mean_delays, cv_delays, delay_info$from, delay_info$to,
    is_delay_in_group)
  
  if (is.na(error_indicators[i])) {
    ## update missing date
  } else if (error_indicators[i]) {
    ## update error date
  } else {
    ## update non-error date
  }
  
  estimated_dates
}
