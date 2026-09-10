##' Create a chronofix model
##'
##' @title Create a chronofix model
##'
##' @param data Observed data
##'
##' @param delay_map Delays information
##' 
##' @param hyperparameters List of hyperparameters
##' 
##' @param control List of control parameters
##' 
##' @return A chronofix model
##'
##' @export
chronofix_model <- function(data, delay_map, hyperparameters, control) {
  
  x <- validate_data_and_delays(data, delay_map)

  groups <- x$groups
  observed_dates <- x$observed_dates
  model_info <- x$model_info
  
  date_range <- calc_date_range(observed_dates, control)
  
  parameters <- "prob_error"
  domain <- c(0, 1)
  for (i in seq_len(nrow(delay_map))) {
    if (delay_map$distribution[i] == "gamma") {
      parameters <- c(parameters,
                      paste0("delay", i, "_shape"),
                      paste0("delay", i, "_mean"))
      domain <- rbind(domain, c(0, Inf), c(0, Inf))
    } else if (delay_map$distribution[i] == "log-normal") {
      parameters <- c(parameters,
                      paste0("delay", i, "_meanlog"),
                      paste0("delay", i, "_precisionlog"))
      domain <- rbind(domain, c(-Inf, Inf), c(0, Inf))
    }
  }
  
  row.names(domain) <- parameters
  
  data_packer <- make_augmented_data_packer(observed_dates)
  
  density <- make_chronofix_density(parameters, groups, model_info, date_range,
                                    hyperparameters, data_packer)
  
  augmented_data_update <- 
    make_augmented_data_update(observed_dates, parameters, groups, model_info,
                               date_range, control, density, data_packer)
  
  likelihood <- monty::monty_model(
    list(parameters = parameters,
         domain = domain,
         density = density,
         augmented_data_update = augmented_data_update))
  
  prior <- make_prior(parameters, hyperparameters, domain, 
                      delay_map$distribution)
  
  model <- likelihood + prior
  
  model$hyperparameters <- hyperparameters
  model$data_packer <- data_packer
  model$info <- model_info
  model$groups_data <- groups
  
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
##' @param gamma_shape_prior_shape The shape parameter of the gamma prior
##'   distribution for the shape parameter of a Gamma-distributed delay
##'
##' @param gamma_shape_prior_rate The rate parameter of the gamma prior
##'   distribution for the shape parameter of a gamma-distributed delay
##'
##' @param gamma_mean_prior_shape The shape parameter of the inverse-gamma
##'   prior distribution for the mean of a Gamma-distributed delay
##'
##' @param gamma_mean_prior_scale The scale parameter of the inverse-gamma
##'   prior distribution for the mean of a Gamma-distributed delay
##'
##' @param log_normal_meanlog_prior_mean The mean of the normal prior 
##'   distribution for the mean on the log-scale of a log-normal-distributed
##'   delay
##'
##' @param log_normal_meanlog_prior_precision The precision of the normal prior
##'   distribution for the mean on the log-scale of a log-normal-distributed
##'   delay
##'
##' @param log_normal_precisionlog_prior_shape The shape parameter of the gamma
##'   prior distribution for the precision on the log-scale of a
##'   log-normal-distributed delay
##'
##' @param log_normal_precisionlog_prior_rate The rate parameter of the gamma
##'   prior distribution for the precision on the log-scale of a
##'   log-normal-distributed delay
##'
##' @return List of hyperparameters
##'
##' @export
chronofix_hyperparameters <- function(prob_error_shape1 = 1,
                                      prob_error_shape2 = 1,
                                      gamma_shape_prior_shape = 1,
                                      gamma_shape_prior_rate = 1,
                                      gamma_mean_prior_shape = 1,
                                      gamma_mean_prior_scale = 1,
                                      log_normal_meanlog_prior_mean = 0,
                                      log_normal_meanlog_prior_precision = 1,
                                      log_normal_precisionlog_prior_shape = 1,
                                      log_normal_precisionlog_prior_rate = 1) {
  list(prob_error_shape1 = prob_error_shape1,
       prob_error_shape2 = prob_error_shape2,
       gamma_shape_prior_shape = gamma_shape_prior_shape,
       gamma_shape_prior_rate = gamma_shape_prior_rate,
       gamma_mean_prior_shape = gamma_mean_prior_shape,
       gamma_mean_prior_scale = gamma_mean_prior_scale,
       log_normal_meanlog_prior_mean = log_normal_meanlog_prior_mean,
       log_normal_meanlog_prior_precision = log_normal_meanlog_prior_precision,
       log_normal_precisionlog_prior_shape = log_normal_precisionlog_prior_shape,
       log_normal_precisionlog_prior_rate = log_normal_precisionlog_prior_rate)
}


validate_data_and_delays <- function(data, delay_map) {
  
  if (!("id" %in% names(data))) {
    cli::cli_abort(c(
      "x" = "{.arg data} must contain an {.col id} column."
    ))
  }
  
  if (any(is.na(data$id))) {
    na_rows <- which(is.na(data$id))
    
    cli::cli_abort(c(
      "The {.col id} column in {.arg data} cannot contain missing values (`NA`).",
      "x" = "Found missing ID{?s} on row{?s}: {.val {na_rows}}"
    ))
  }
  
  if (any(duplicated(data$id))) {
    duplicate_ids <- unique(data$id[duplicated(data$id)])
    
    cli::cli_abort(c(
      "The {.col id} column in {.arg data} must contain unique values.",
      "x" = "Found duplicate ID{?s}: {.val {duplicate_ids}}"
    ))
  }
  
  validate_groups(data, delay_map)
  
  if (!("group" %in% names(data))) {
    data$group <- 1
    delay_map$group <- 1
  }
  
  validate_events(data, delay_map)
  
  dates <- setdiff(names(data), c("id", "group"))
  model_info <- make_model_info(delay_map, dates)
  
  observed_dates <- observed_dates_to_int(data)
  
  groups <- match(data$group, model_info$groups)
  
  list(model_info = model_info,
       observed_dates = observed_dates,
       groups = groups)
}

validate_groups <- function(data, delay_map) {
  
  is_group_in_data <- "group" %in% names(data)
  is_group_in_delay_map <- "group" %in% names(delay_map)
  if (!is_group_in_data && is_group_in_delay_map) {
    stop("Expected 'group' column in 'data' given it exists in 'delay_map'")
  }
  if (is_group_in_data && !is_group_in_delay_map) {
    stop("Expected 'group' column in 'delay_map' given it exists in 'data'")
  }
  
  if (is_group_in_data && is_group_in_delay_map) {
    groups_data <- sort(unique(data$group))
    groups_delay_map <- sort(unique(unlist(delay_map$group)))
    ## could use identical() here but that will throw an error if groups
    ## are the same but one set is numeric and one is integer type
    is_same_groups <- length(groups_data) == length(groups_delay_map) &&
      all(groups_data == groups_delay_map)
    if (!is_same_groups) {
      cli::cli_abort(
        c("Groups in 'data' do not match those in 'delay_map'",
          i = "'data' has: {.val {groups_data}}",
          x = "'delay_map' has: {.val {groups_delay_map}}"))
    }
  }
}

validate_events <- function(data, delay_map) {
  
  event_cols <- setdiff(names(data), c("id", "group"))
  delay_events <- unique(c(delay_map$from, delay_map$to))
  
  # from/to event in delay_map does not match a column in data
  missing_in_data <- setdiff(delay_events, event_cols)
  if (length(missing_in_data) > 0) {
    cli::cli_abort(c(
      "All events in {.arg delay_map} must exist as columns in {.arg data}.",
      "x" = "Missing column{?s} in data: {.val {missing_in_data}}"
    ))
  }
  
  # event column in data has no associated delay in delay_map
  missing_in_map <- setdiff(event_cols, delay_events)
  if (length(missing_in_map) > 0) {
    cli::cli_abort(c(
      "All event columns in {.arg data} must be mapped in {.arg delay_map}.",
      "x" = "Unmapped event column{?s} found in data: {.val {missing_in_map}}"
    ))
  }
  
  # individual has all NA dates
  all_na_row <- rowSums(is.na(data[, event_cols, drop = FALSE])) == length(event_cols)
  if (any(all_na_row)) {
    problem_data <- data[all_na_row, ]
    print(problem_data)
    
    cli::cli_abort(c(
      "Individuals cannot have `NA` for all event dates.",
      "x" = "Found {nrow(problem_data)} individual{?s} with no recorded dates (see printed data above)."
    ))
  }
  
  # non-NA dates for events not associated with the individual's group
  groups_in_data <- unique(data$group)
  has_invalid_date <- rep(FALSE, nrow(data))
  
  for (grp in groups_in_data) {
    if (is.list(delay_map$group)) {
      map_idx <- sapply(delay_map$group, function(x) grp %in% x)
      } else {
        map_idx <- delay_map$group == grp
        }
    
    valid_events <- unique(c(delay_map$from[map_idx], delay_map$to[map_idx]))
    invalid_events <- setdiff(event_cols, valid_events)
    
    if (length(invalid_events) > 0) {
      for (ev in invalid_events) {
        current_bad_rows <- data$group == grp & !is.na(data[[ev]])
        has_invalid_date <- has_invalid_date | current_bad_rows
      }
    }
  }
  
  if (any(has_invalid_date)) {
    problem_data <- data[has_invalid_date, ]
    print(problem_data)
    
    cli::cli_abort(c(
      "Individuals have dates for events not associated with their group in {.arg delay_map}.",
      "i" = "This could indicate an error in the grouping assignment or data entry.",
      "x" = "Found {nrow(problem_data)} invalid record{?s} (see printed data above)."
    ))
  }
}


make_model_info <- function(delay_map, dates) {
  delay_from <- match(delay_map$from, dates)
  delay_to <- match(delay_map$to, dates)
  delay_distribution <- delay_map$distribution
  
  delay_info <- list(from = delay_from,
                     to = delay_to,
                     distribution = delay_distribution)
  
  groups <- sort(unique(unlist(delay_map$group)))
  
  d <- seq_along(dates)
  
  make_group_info <- function(g) {
    ## logical vector - is delay i in group g
    is_delay_in_group <- 
      vapply(seq_len(nrow(delay_map)),
             function(i) g %in% unlist(delay_map$group[i]),
             logical(1L))
    
    ## logical array - is date i (row) in delay j (col) for group g
    is_date_in_delay <- vapply(seq_len(nrow(delay_map)),
                               function (i) {
                                 (d %in% c(delay_from[i], delay_to[i])) & 
                                   is_delay_in_group[i]
                              }, logical(length(d)))
    
    ## logical vector - is date i in group g
    is_date_in_group <- apply(is_date_in_delay, 1, any)
    
    ## logical array - is date i (row) connected to date j (col)
    ##                 for group g
    is_date_connected <- array(FALSE, c(length(d), length(d)))
    delay_connecting_dates <- array(NA, c(length(d), length(d)))
    for (i in seq_along(delay_from)) {
      if (is_delay_in_group[i]) {
        is_date_connected[delay_from[i], delay_to[i]] <- TRUE
        is_date_connected[delay_to[i], delay_from[i]] <- TRUE
        delay_connecting_dates[delay_from[i], delay_to[i]] <- i
        delay_connecting_dates[delay_to[i], delay_from[i]] <- i
      }
    }
    
  
    # order of events in group
    # identify relevant delays and event dates for a group
    dates_from <- delay_from[is_delay_in_group]
    dates_to <- delay_to[is_delay_in_group]
    
    relevant_dates <- unique(c(dates_from, dates_to))
    delay_df <- data.frame(from = dates_from, to = dates_to)
    
    event_graph <- igraph::graph_from_data_frame(delay_df,
                                                 directed = TRUE,
                                                 vertices = relevant_dates)
    
    event_order <- as.numeric(names(igraph::topo_sort(event_graph)))  
    
    ## shortest paths
    event_graph <- igraph::graph_from_data_frame(delay_df,
                                                 directed = FALSE,
                                                 vertices = relevant_dates)
    shortest_paths <- rep(list(NULL), length(dates))
    for (i in relevant_dates) {
      paths_i <- rep(list(NULL), length(dates))
      for (j in relevant_dates[relevant_dates != i]) {
        p <- suppressWarnings(
          igraph::shortest_paths(event_graph, 
                                 as.character(i), 
                                 as.character(j))$vpath[[1]])
        paths_i[[j]] <- relevant_dates[as.integer(p)]
      }
      shortest_paths[[i]] <- paths_i
    }
    
    list(is_delay_in_group = is_delay_in_group,
         is_date_in_delay = is_date_in_delay,
         is_date_in_group = is_date_in_group,
         is_date_connected = is_date_connected,
         event_order = event_order,
         shortest_paths = shortest_paths,
         delay_connecting_dates = delay_connecting_dates
         )
  }
  
  group_info <- lapply(groups, make_group_info)
  
  
  list(delay_info = delay_info,
       group_info = group_info,
       groups = groups)  
}


make_chronofix_density <- function(parameters, groups, model_info, date_range,
                                   hyperparameters, data_packer) {
  
  density <- function(pars) {
    names(pars) <- parameters
    
    log_likelihood <- chronofix_log_likelihood(pars, groups, model_info,
                                               date_range, data_packer)
  }
  
  density
}


#' @importFrom stats dbeta dexp dgamma dnorm
make_prior <- function(parameters, hyperparameters, domain,
                       delay_distributions) {
  monty::monty_model(
    list(
      parameters = parameters,
      density = function (pars) {
        names(pars) <- parameters
        
        lp_prob_error <- 
          dbeta(pars[["prob_error"]], hyperparameters$prob_error_shape1, 
                hyperparameters$prob_error_shape2, log = TRUE)
        
        n_delays <- length(delay_distributions)
        lp_delays <- rep(0, n_delays)
        
        for (i in seq_len(n_delays)) {
          if (delay_distributions[i] == "gamma") {
            lp_delays[i] <-
              dgamma(pars[[paste0("delay", i, "_shape")]],
                     hyperparameters$gamma_shape_prior_shape,
                     rate = hyperparameters$gamma_shape_prior_rate, 
                     log = TRUE) +
              dinvgamma(pars[[paste0("delay", i, "_mean")]],
                        hyperparameters$gamma_mean_prior_shape,
                        hyperparameters$gamma_mean_prior_scale,
                        log = TRUE)
          } else if (delay_distributions[i] == "log-normal") {
            lp_delays[i] <-
              dnorm(pars[[paste0("delay", i, "_meanlog")]],
                     hyperparameters$log_normal_meanlog_prior_mean,
                     1 / sqrt(hyperparameters$log_normal_meanlog_prior_precision),
                    log = TRUE) +
              dgamma(pars[[paste0("delay", i, "_precisionlog")]],
                     hyperparameters$log_normal_precisionlog_prior_shape,
                     rate = hyperparameters$log_normal_precisionlog_prior_rate, 
                     log = TRUE)
          }
        }
        
        lp_prob_error + sum(lp_delays)
        
      },
      domain = domain
    ))
}


chronofix_log_likelihood <- function(pars, groups, model_info, date_range,
                                     data_packer) {
  
  augmented_data <- unpack_augmented_data(attr(pars, "data"), data_packer)
  
  ll_errors <- 
    sum(chronofix_log_likelihood_errors(pars[["prob_error"]],
                                        augmented_data$error_indicators,
                                        date_range))
  
  delay_pars <- unpack_delay_pars(pars, model_info$delay_info$distribution)
  
  ll_delays <- 
    chronofix_log_likelihood_delays(augmented_data$estimated_dates,
                                    groups, delay_pars, model_info)
  
  ll_errors + ll_delays
                                               
}


chronofix_log_likelihood_errors <- function(prob_error, error_indicators,
                                            date_range) {
  n_errors <- rowSums(error_indicators, na.rm = TRUE)
  n_non_errors <- rowSums(!error_indicators, na.rm = TRUE)
  
  ## prob_error of each error and 1 - prob_error of each non-error
  ## for each error the date is then drawn at random from the range of dates
  ## excluding the observed date
  n_errors * log(prob_error) + n_non_errors * log(1 - prob_error) -
    n_errors * log(date_range[2L] - date_range[1L] - 1)
}


chronofix_log_likelihood_delays <- function(estimated_dates, groups, delay_pars,
                                            model_info) {
  
  ll_delays <- array(0, c(length(groups), length(delay_pars)))
  for (i in unique(groups)) {
    group_i <- groups == i
    ll_delays[group_i, ] <- 
      chronofix_log_likelihood_delays1(
        estimated_dates[group_i, , drop = FALSE], delay_pars, 
        model_info$delay_info, model_info$group_info[[i]]$is_delay_in_group)
  }
  
  sum(ll_delays)
  
}


chronofix_log_likelihood_delays1 <- function(estimated_dates, delay_pars,
                                             delay_info, is_delay_in_group) {
  
  is_vec <- is.vector(estimated_dates)
  if (is_vec) {
    estimated_dates <- array(estimated_dates, c(1, length(estimated_dates)))
  }
  
  group_size <- nrow(estimated_dates)
  
  group_delay_pars <- delay_pars[is_delay_in_group]
  delay_from <- delay_info$from[is_delay_in_group]
  delay_to <- delay_info$to[is_delay_in_group]
  delay_distribution <- delay_info$distribution[is_delay_in_group]
  
  delay_values <- estimated_dates[, delay_to, drop = FALSE] -
    estimated_dates[, delay_from, drop = FALSE]
  
  ll <- array(0, c(group_size, length(is_delay_in_group)))
  ll[, is_delay_in_group] <- 
    vapply(seq_along(group_delay_pars),
           function(i) {
             log_density_delay(delay_values[, i], group_delay_pars[[i]],
                               delay_distribution[[i]])
           },
           numeric(group_size))
  
  if (is_vec) {
    ll <- drop(ll)
  }
  
  ll
}


#' @importFrom stats dgamma dlnorm
log_density_delay <- function(values, params, distribution) {
  
  if (distribution == "gamma") {
    d <- dgamma(values, params$shape, 
                rate = params$shape / params$mean, log = TRUE)
  } else if (distribution == "log-normal") {
    d <- dlnorm(values, params$meanlog, 
                1 / sqrt(params$precisionlog), log = TRUE)
  }
  
  d
}


convert_to_distribution_params <- function(mean, cv, distribution) {
  
  if (distribution == "gamma") {
    shape <- (1 / cv)^2
    rate <- shape / mean
    
    params <- list(shape = shape,
                   rate = rate)
  } else if (distribution == "log-normal") {
    sdlog <- sqrt(log(cv^2 + 1))
    meanlog <- log(mean) - sdlog^2 / 2
    
    params <- list(meanlog = meanlog,
                   sdlog = sdlog)
  } else {
    stop(sprintf('Distribution "%s" is not supported', distribution))
  }
  
  params
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


unpack_delay_pars <- function(pars, delay_distributions) {
  unpack1 <- function(i) {
    if (delay_distributions[i] == "gamma") {
      list(shape = pars[[paste0("delay", i, "_shape")]],
           mean = pars[[paste0("delay", i, "_mean")]])
    } else if (delay_distributions[i] == "log-normal") {
      list(meanlog = pars[[paste0("delay", i, "_meanlog")]],
           precisionlog = pars[[paste0("delay", i, "_precisionlog")]])
    }
  }
  
  lapply(seq_along(delay_distributions), unpack1)
}


dinvgamma <- function(x, shape, scale, log = FALSE) {
  
  d <- shape * log(scale) - lgamma(shape) - (shape + 1) * log(x) - scale / x
  
  if (!log) {
    d <- exp(d)
  }
  d
}
