test_that("validate_delay_inputs catches all missing inputs and bad structures", {
  
  mock_delay_map <- data.frame(
    from = "onset", to = "report",
    distribution = "gamma", group = "1", stringsAsFactors = FALSE
  )
  
  mock_mcmc_output <- list(
    pars = array(1, dim = c(2, 10, 1),
                 dimnames = list(c("delay_mean1", "delay_shape1"), NULL, NULL))
  )
  
  # missing mcmc_output or delay_map
  expect_error(validate_delay_inputs(delay_map = mock_delay_map), "is missing")
  expect_error(validate_delay_inputs(mcmc_output = mock_mcmc_output), "is missing")
  
  # missing pars array 
  expect_error(validate_delay_inputs(list(wrong_name = 1), mock_delay_map), 
               "must contain a pars array")
  
  # missing parameter names
  bad_pars <- mock_mcmc_output
  dimnames(bad_pars$pars) <- NULL
  expect_error(validate_delay_inputs(bad_pars, mock_delay_map), 
               "Parameter names are missing")
  
  # missing columns in delay_map
  bad_map <- mock_delay_map[, c("from", "to")] 
  expect_error(validate_delay_inputs(mock_mcmc_output, bad_map), 
               "missing required column")
  
  # missing specific delay parameters
  bad_pars_names <- mock_mcmc_output
  dimnames(bad_pars_names$pars)[[1]] <- c("wrong_name1", "wrong_name2")
  expect_error(validate_delay_inputs(bad_pars_names, mock_delay_map), 
               "Missing parameter")
  
  # unsupported distribution
  bad_dist_map <- mock_delay_map
  bad_dist_map$distribution <- "weibull"
  expect_error(validate_delay_inputs(mock_mcmc_output, bad_dist_map), 
               "Unsupported distribution")
})
