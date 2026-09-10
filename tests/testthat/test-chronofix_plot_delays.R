test_that("chronofix_plot_delays calls validation helper", {
  expect_error(chronofix_plot_delays(), "is missing")
})

test_that("chronofix_plot_delays generates a correct standard ggplot (facet_by_group = FALSE)", {
  
  mock_delay_map <- data.frame(
    from = c("onset", "onset", "hospitalisation", "onset", "hospitalisation", "onset"),
    to = c("report", "hospitalisation", "discharge", "hospitalisation", "death", "death"),
    distribution = c("gamma", "log-normal", "gamma", "gamma", "log-normal", "gamma"),
    stringsAsFactors = FALSE
  )
  mock_delay_map$group <- as.list(c(
    "hospitalised-alive", "hospitalised-alive", "hospitalised-alive",
    "hospitalised-dead", "hospitalised-dead",
    "community-dead"
  ))
  
  param_names <- c(
    "delay1_mean", "delay1_shape",
    "delay2_meanlog", "delay2_precisionlog",
    "delay3_mean", "delay3_shape",
    "delay4_mean", "delay4_shape",
    "delay5_meanlog", "delay5_precisionlog",
    "delay6_mean", "delay6_shape"
  )
  
  set.seed(1)
  mock_pars <- matrix(
    data = NA, 
    nrow = length(param_names),
    ncol = 100, 
    dimnames = list(param_names, NULL)
  )
  
  mock_pars["delay1_mean", ] <- runif(100, min = 4, max = 6)
  mock_pars["delay1_shape", ] <- runif(100, min = 2, max = 4)
  mock_pars["delay2_meanlog", ] <- runif(100, min = 1, max = 2)
  mock_pars["delay2_precisionlog", ] <- runif(100, min = 2, max = 5)
  mock_pars["delay3_mean", ] <- runif(100, min = 5, max = 10)
  mock_pars["delay3_shape", ] <- runif(100, min = 1.5, max = 3)
  mock_pars["delay4_mean", ] <- runif(100, min = 3, max = 7)
  mock_pars["delay4_shape", ] <- runif(100, min = 2, max = 5)
  mock_pars["delay5_meanlog", ] <- runif(100, min = 1.5, max = 2.5)
  mock_pars["delay5_precisionlog", ] <- runif(100, min = 2, max = 4)
  mock_pars["delay6_mean", ] <- runif(100, min = 10, max = 15)
  mock_pars["delay6_shape", ] <- runif(100, min = 3, max = 6)
  
  mock_mcmc_output <- list(pars = mock_pars)
  
  p <- chronofix_plot_delays(
    mcmc_output = mock_mcmc_output, 
    delay_map = mock_delay_map, 
    n_points = 30,
    facet_by_group = FALSE,
    share_x_axis = FALSE
  )
  
  expect_s3_class(p, "ggplot")
  expect_length(p$layers, 2) # 2 geom layers (ribbon, line)
  expect_identical(p$labels$x, "Delay (Days)")
  expect_identical(p$labels$y, "Probability Density")
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("chronofix_plot_delays generates a correct patchwork object (facet_by_group = TRUE)", {
  
  mock_delay_map <- data.frame(
    from = c("onset", "onset", "hospitalisation", "onset", "hospitalisation", "onset"),
    to = c("report", "hospitalisation", "discharge", "hospitalisation", "death", "death"),
    distribution = c("gamma", "log-normal", "gamma", "gamma", "log-normal", "gamma"),
    stringsAsFactors = FALSE
  )
  mock_delay_map$group <- as.list(c(
    "hospitalised-alive", "hospitalised-alive", "hospitalised-alive",
    "hospitalised-dead", "hospitalised-dead",
    "community-dead"
  ))
  
  param_names <- c(
    "delay1_mean", "delay1_shape",
    "delay2_meanlog", "delay2_precisionlog",
    "delay3_mean", "delay3_shape",
    "delay4_mean", "delay4_shape",
    "delay5_meanlog", "delay5_precisionlog",
    "delay6_mean", "delay6_shape"
  )
  
  set.seed(1)
  mock_pars <- matrix(
    data = NA, 
    nrow = length(param_names),
    ncol = 100, 
    dimnames = list(param_names, NULL)
  )
  
  mock_pars["delay1_mean", ] <- runif(100, min = 4, max = 6)
  mock_pars["delay1_shape", ] <- runif(100, min = 2, max = 4)
  mock_pars["delay2_meanlog", ] <- runif(100, min = 1, max = 2)
  mock_pars["delay2_precisionlog", ] <- runif(100, min = 2, max = 5)
  mock_pars["delay3_mean", ] <- runif(100, min = 5, max = 10)
  mock_pars["delay3_shape", ] <- runif(100, min = 1.5, max = 3)
  mock_pars["delay4_mean", ] <- runif(100, min = 3, max = 7)
  mock_pars["delay4_shape", ] <- runif(100, min = 2, max = 5)
  mock_pars["delay5_meanlog", ] <- runif(100, min = 1.5, max = 2.5)
  mock_pars["delay5_precisionlog", ] <- runif(100, min = 2, max = 4)
  mock_pars["delay6_mean", ] <- runif(100, min = 10, max = 15)
  mock_pars["delay6_shape", ] <- runif(100, min = 3, max = 6)
  mock_mcmc_output <- list(pars = mock_pars)
  
  p <- chronofix_plot_delays(
    mcmc_output = mock_mcmc_output, 
    delay_map = mock_delay_map, 
    n_points = 30,
    facet_by_group = TRUE,
    share_x_axis = TRUE
  )
  
  expect_s3_class(p, "patchwork")
  expect_s3_class(p, "ggplot") 
})

test_that("chronofix_plot_delays handles edge cases in group names gracefully", {
  
  mock_delay_map <- data.frame(
    from = "onset",
    to = "report",
    distribution = "gamma",
    stringsAsFactors = FALSE
  )
  mock_delay_map$group <- list("c(\"complex_group_name\")")
  
  param_names <- c("delay1_mean", "delay1_shape")
  mock_pars <- matrix(
    data = runif(100, min = 0.5, max = 5),
    nrow = 2,
    ncol = 50, # 50 iter * 1 chain
    dimnames = list(param_names, NULL)
  )
  mock_mcmc_output <- list(pars = mock_pars)
  
  p <- chronofix_plot_delays(mock_mcmc_output, mock_delay_map, n_points = 10,
                             facet_by_group = FALSE)
  
  # "c("complex_group_name")" should become "Complex Group Name"
  panel_titles <- unique(p$data$Panel_Title)
  expect_true(any(grepl("Complex Group Name", panel_titles)))
})

test_that("chronofix_plot_delays handles multiple groups in a single facet", {
  
  mock_delay_map <- data.frame(
    from = "onset",
    to = "report",
    distribution = "gamma",
    stringsAsFactors = FALSE
  )
  # Pass multiple groups into a single row
  mock_delay_map$group <- I(list(c("community_alive", "hospitalised_alive")))
  
  param_names <- c("delay1_mean", "delay1_shape")
  set.seed(1)
  mock_pars <- matrix(
    data = runif(100, min = 0.5, max = 5), 
    nrow = 2,
    ncol = 50,
    dimnames = list(param_names, NULL)
  )
  mock_mcmc_output <- list(pars = mock_pars)
  
  p <- chronofix_plot_delays(mock_mcmc_output, mock_delay_map, n_points = 20,
                             facet_by_group = FALSE)
  
  # Check if the title correctly pasted and capitalised both groups
  panel_titles <- unique(p$data$Panel_Title)
  expect_true(any(grepl("Community Alive, Hospitalised Alive", panel_titles)))
})

