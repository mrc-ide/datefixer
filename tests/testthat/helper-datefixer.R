toy_model <- function(control = mcmc_control()) {
  ## setup data
  
  delay_map <- data.frame(
    from = c("onset", "onset", "onset",
             "hospitalisation", "onset", "hospitalisation"),
    to = c("report", "death", "hospitalisation",
           "discharge", "hospitalisation", "death"),
    group = I(list(1:4, 2, 3, 3, 4, 4))
  )
  
  # Define the delay parameters data frame
  delay_params <- data.frame(
    group = c(1:4, 2, 3, 3, 4, 4),
    from = c("onset", "onset", "onset", "onset", "onset", "onset",
             "hospitalisation", "onset", "hospitalisation"),
    to = c("report", "report", "report", "report", "death", "hospitalisation",
           "discharge", "hospitalisation", "death"),
    delay_mean = c(10, 10, 10, 10, 15, 7, 20, 7, 12),
    delay_cv = c(0.3, 0.3, 0.3, 0.3, 0.4, 0.2, 0.5, 0.2, 0.3)
  )
  
  # Define other parameters
  n_per_group <- rep(10, max(delay_params$group))
  group_names <- c(1, 2, 3, 4)
  error_params <- list(prop_missing_data = 0.2, prob_error = 0.05)
  date_range <- as.integer(as.Date(c("2025-03-01", "2025-09-01")))
  
  # Run simulation
  sim_result <- simulate_data(
    n_per_group = n_per_group,
    group_names = group_names,
    delay_map = delay_map,
    delay_params = delay_params,
    error_params = error_params,
    date_range = date_range,
    simul_error = TRUE
  )
  
  ## setup model
  hyperparameters <- datefixer_hyperparameters()
  
  model <- datefixer_model(sim_result$observed_data, delay_map, hyperparameters, control)
  
  list(model = model,
       delay_map = delay_map,
       data = sim_result)
  
}
