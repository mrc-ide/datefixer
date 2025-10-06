toy_model <- function() {
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
  
  dates_by_group <- data.frame(
    group = c(1, 2, 3, 4),
    onset = rep(TRUE, 4),
    hospitalisation = c(FALSE, FALSE, TRUE, TRUE),
    discharge = c(FALSE, FALSE, TRUE, FALSE),
    death = c(FALSE, TRUE, FALSE, TRUE),
    report = rep(TRUE, 4)
  )
  
  # Define other parameters
  n_per_group <- rep(10, max(delay_params$group))
  error_params <- list(prop_missing_data = 0.2, prob_error = 0.05)
  range_dates <- as.integer(as.Date(c("2025-03-01", "2025-09-01")))
  
  # Run simulation
  set.seed(1)
  sim_result <- simulate_data(
    n_per_group = n_per_group,
    delay_map = delay_map,
    delay_params = delay_params,
    error_params = error_params,
    range_dates = range_dates,
    simul_error = TRUE
  )
  
  ## setup model
  hyperparameters <- datefixer_hyperparameters()
  control <- mcmc_control()
  
  datefixer_model(sim_result$true_data, delay_map, hyperparameters, control)
}
