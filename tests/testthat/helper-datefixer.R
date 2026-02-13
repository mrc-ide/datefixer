toy_model_params <- function(named_groups = TRUE) {
  
  if (named_groups) {
    group_names <- c("community-alive", "community-dead",
                     "hospitalised-alive", "hospitalised-dead")
  } else {
    group_names <- seq_len(4)
  }
  
  # Define the delay_map data frame
  delay_map <- data.frame(
    from = c("onset", "onset", "onset",
             "hospitalisation", "onset", "hospitalisation"),
    to = c("report", "death", "hospitalisation",
           "discharge", "hospitalisation", "death"),
    group = I(list(
      group_names,      ## all groups
      group_names[2],   ## comunity-dead
      group_names[3],   ## hospitalised-alive
      group_names[3],   ## hospitalised-alive
      group_names[4],   ## hospitalised-dead
      group_names[4]    ## hospitalised-dead
    )),
    distribution = c("gamma", "gamma", "gamma", "gamma",
                     "log-normal", "log-normal")
  )
      
  # Define the delay parameters data frame
  delay_params <- delay_map
  delay_params$mean <- c(10, 15, 7, 20, 7, 12)
  delay_params$cv <- c(0.3, 0.4, 0.2, 0.5, 0.2, 0.3)
  
  # Define other parameters
  n_per_group <- rep(10, length(unique(delay_params$group)))
  error_params <- list(prop_missing_data = 0.2, prob_error = 0.05)
  date_range <- as.integer(as.Date(c("2025-03-01", "2025-09-01")))
  
  list(n_per_group = n_per_group,
       group_names = group_names,
       delay_map = delay_map,
       delay_params = delay_params,
       error_params = error_params,
       date_range = date_range)
}

toy_model <- function(named_groups = TRUE, control = mcmc_control()) {
  params <- toy_model_params(named_groups)
  
  # Run simulation
  sim_result <- simulate_data(
    n_per_group = params$n_per_group,
    group_names = params$group_names,
    delay_params = params$delay_params,
    error_params = params$error_params,
    date_range = params$date_range
  )
  
  ## setup model
  hyperparameters <- datefixer_hyperparameters()
  
  model <- datefixer_model(sim_result$observed_data, params$delay_map, 
                           hyperparameters, control)
  
  list(model = model,
       delay_map = params$delay_map,
       data = sim_result)
  
}
