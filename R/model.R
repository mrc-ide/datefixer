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
datefixer_model <- function(data, delay_map, hyperparameters, control,
                            group_col = NULL) {
  
  x <- validate_data_and_delays(data, delay_map)

  groups <- x$groups
  observed_dates <- x$observed_dates
  model_info <- x$model_info
  
  date_range <- calc_date_range(observed_dates, control)
  
  n_delays <- nrow(delay_map)
  delay_ids <- seq_len(n_delays)
  parameters <- c("prob_error",
                  paste0("mean_delay", delay_ids),
                  paste0("cv_delay", delay_ids))
  
  domain <- cbind(rep(0, 1 + 2 * n_delays), c(1, rep(Inf, 2 * n_delays)))
  
  data_packer <- make_augmented_data_packer(observed_dates)
  
  density <- make_datefixer_density(parameters, groups, model_info, date_range,
                                    hyperparameters, data_packer)
  
  augmented_data_update <- 
    make_augmented_data_update(observed_dates, parameters, groups, model_info,
                               date_range, control, density, data_packer)
  
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
  
  if (!("group" %in% names(data))) {
    data$group <- 1
    delay_map$group <- 1
  }
  
  model_info <- make_model_info(delay_map, dates)
  
  observed_dates <- observed_dates_to_int(data)
  
  groups <- match(data$group, model_info$groups)
  
  list(model_info = model_info,
       observed_dates = observed_dates,
       groups = groups)
}


make_model_info <- function(delay_map, dates) {
  
  delay_from <- match(delay_map$from, dates)
  delay_to <- match(delay_map$to, dates)
  
  g <- sort(unique(unlist(delay_map$group)))
  
  ## logical array - is delay i (row) in group j (col)
  is_delay_in_group <- t(vapply(seq_len(nrow(delay_map)),
                                function(i) g %in% unlist(delay_map$group[i]),
                                logical(length(g))))
  
  d <- seq_along(dates)
  
  ## logical array - is date i (row) in delay j (col) for group k (3rd dim) 
  is_date_in_delay <- vapply(seq_len(nrow(delay_map)),
                             function (i) {
                               x <- d %in% c(delay_from[i], delay_to[i])
                               y <- is_delay_in_group[i, ]
                               outer(x, y)
                             }, array(0, c(length(d), length(g))))
  is_date_in_delay <- 
    apply(aperm(is_date_in_delay, c(1, 3, 2)), c(1, 2, 3), as.logical)
  
  ## logical array - is date i (row) in group j (col)
  is_date_in_group <- apply(is_date_in_delay, c(1, 3), any)
  
  ## logical array - is date i (row) connected to date j (col)
  ##                 for group k (3rd dim)
  is_date_connected <- array(FALSE, c(length(d), length(d), length(g)))
  for (i in seq_along(delay_from)) {
    delay_groups <- match(unlist(delay_map$group[i]), g)
    is_date_connected[delay_from[i], delay_to[i], delay_groups] <- TRUE
    is_date_connected[delay_to[i], delay_from[i], delay_groups] <- TRUE
  }
  
  calc_event_order <- function(group) {
    # identify relevant delays and event dates for a group
    dates_from <- delay_from[is_delay_in_group[, group]]
    dates_to <- delay_to[is_delay_in_group[, group]]
    
    relevant_dates <- unique(c(dates_from, dates_to))
    delay_df <- data.frame(from = dates_from, to = dates_to)
    
    event_graph <- igraph::graph_from_data_frame(delay_df,
                                                 directed = TRUE,
                                                 vertices = relevant_dates)
    
    as.numeric(names(igraph::topo_sort(event_graph)))
  }
  event_order <- lapply(seq_along(g), calc_event_order)
  
  list(delay_from = delay_from,
       delay_to = delay_to,
       is_delay_in_group = is_delay_in_group,
       is_date_in_delay = is_date_in_delay,
       is_date_in_group = is_date_in_group,
       is_date_connected = is_date_connected,
       event_order = event_order,
       groups = g)  
}


make_datefixer_density <- function(parameters, groups, model_info, date_range,
                                   hyperparameters, data_packer) {
  
  density <- function(pars) {
    names(pars) <- parameters
    
    log_likelihood <- datefixer_log_likelihood(pars, groups, model_info,
                                               date_range, data_packer)
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


datefixer_log_likelihood <- function(pars, groups, model_info, date_range,
                                     data_packer) {
  augmented_data <- unpack_augmented_data(attr(pars, "data"), data_packer)
  
  ll_errors <- datefixer_log_likelihood_errors(pars[["prob_error"]],
                                               augmented_data$error_indicators,
                                               date_range)
  
  delays <- seq_along(model_info$delay_from)
  
  ll_delays <- 
    datefixer_log_likelihood_delays(augmented_data$estimated_dates,
                                    groups,
                                    pars[paste0("mean_delay", delays)],
                                    pars[paste0("cv_delay", delays)],
                                    model_info)
  
  
  ll_errors + ll_delays
                                               
}


datefixer_log_likelihood_errors <- function(prob_error, error_indicators,
                                            date_range) {
  n_errors <- sum(error_indicators, na.rm = TRUE)
  n_non_errors <- sum(!error_indicators, na.rm = TRUE)
  
  ## prob_error of each error and 1 - prob_error of each non-error
  ## for each error the date is then drawn at random from the range of dates
  ## excluding the observed date
  n_errors * log(prob_error) + n_non_errors * log(1 - prob_error) -
    n_errors * log(diff(date_range) - 1)
}


datefixer_log_likelihood_delays <- function(estimated_dates, groups, mean_delays,
                                            cv_delays, model_info) {
  
  ll_delays <- array(0, c(length(groups), length(mean_delays)))
  for (i in unique(groups)) {
    group_i <- groups == i
    ll_delays[group_i, ] <- 
      datefixer_log_likelihood_delays1(estimated_dates[group_i, , drop = FALSE],
                                       mean_delays,
                                       cv_delays,
                                       model_info$delay_from,
                                       model_info$delay_to,
                                       model_info$is_delay_in_group[, i])
  }
  
  sum(ll_delays)
  
}


#' @importFrom stats dgamma
datefixer_log_likelihood_delays1 <- function(estimated_dates, mean_delays,
                                             cv_delays, delay_from, delay_to,
                                             is_delay_in_group) {
  is_vec <- is.vector(estimated_dates)
  if (is_vec) {
    estimated_dates <- array(estimated_dates, c(1, length(estimated_dates)))
  }
  
  group_size <- nrow(estimated_dates)
  
  shape <- (1 / cv_delays[is_delay_in_group])^2
  rate <- shape / mean_delays[is_delay_in_group]
  
  delay_values <- estimated_dates[, delay_to[is_delay_in_group], drop = FALSE] -
    estimated_dates[, delay_from[is_delay_in_group], drop = FALSE]
  
  ll <- array(0, c(group_size, length(is_delay_in_group)))
  ll[, is_delay_in_group] <- 
    vapply(seq_along(shape),
           function(i) {
             dgamma(delay_values[, i], shape[[i]], rate = rate[[i]], log = TRUE)
           },
           numeric(group_size))
  
  if (is_vec) {
    ll <- drop(ll)
  }
  
  ll
}


make_augmented_data_update <- function(observed_dates, parameters, groups,
                                       model_info, date_range, control,
                                       density_fn, data_packer) {
  augmented_data_update <- function(pars, rng) {
    augmented_data <- attr(pars, "data")
    
    names(pars) <- parameters
    
    if (is.null(augmented_data)) {
      ## augmented data does not exist, so we initialise it
      augmented_data <- 
        initialise_augmented_data(observed_dates, pars, groups, model_info,
                                  date_range, control, rng)
      augmented_data <- data_packer$pack(augmented_data)
      
      attr(pars, "data") <- augmented_data
      density <- density_fn(pars)
    } else {
      augmented_data <- unpack_augmented_data(augmented_data, data_packer)
      augmented_data <- update_augmented_data(augmented_data, observed_dates,
                                              pars, groups, model_info,
                                              date_range, control, rng)
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


calc_date_range <- function(observed_dates, control) {
  ## convert earliest/latest possible dates to integer or take them from
  ## the data if NULL
  date_min <- 
    if (is.null(control$earliest_possible_date)) 
      min(observed_dates, na.rm = TRUE) - control$date_buffer else 
        date_to_int(control$earliest_possible_date)
  date_max <- 
    if (is.null(control$latest_possible_date)) 
      max(observed_dates, na.rm = TRUE) + control$date_buffer else
      date_to_int(control$latest_possible_date)
  
  ## Add 1 to date_max as we allow anything over that one day interval
  date_range <- c(date_min, date_max + 1)
}
  

make_augmented_data_packer <- function(observed_dates) {
  monty::monty_packer(array = list(estimated_dates = dim(observed_dates),
                                   error_indicators = dim(observed_dates)))
}


unpack_augmented_data <- function(augmented_data, data_packer) {
  augmented_data <- data_packer$unpack(augmented_data)
  augmented_data$error_indicators <- 
    apply(augmented_data$error_indicators, 
          seq_along(dim(augmented_data$error_indicators)),
          as.logical)
  augmented_data
}
