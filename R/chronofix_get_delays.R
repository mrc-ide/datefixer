#' @title Get Summary Table of Estimated Delays
#'
#' @description
#' Summarises posterior samples for each delay distribution described in
#' `delay_map`, returning the derived mean/CV and the native distribution
#' parameters (shape/scale for Gamma, meanlog/sdlog for Log-Normal), each
#' with posterior mean, posterior median and 95% credible interval.
#' 
#' @param mcmc_output Output list from `chronofix_mcmc_run()`.
#' @param delay_map The delay map used for the model setup.
#'
#' @return A data frame with one row per delay x parameter
#'   (`Mean`, `CV`, and the two native distribution parameters (shape/scale for
#'   Gamma, meanlog/sdlog for Log-Normal), giving the `Posterior_Mean`,
#'   `Posterior_Median` and 95% credible interval (`Lower_95_CrI`, `Upper_95_CrI`).
#'
#' @importFrom stats quantile
#' @export
chronofix_get_delays <- function(mcmc_output, delay_map) {
  
  validate_delay_inputs(mcmc_output, delay_map)
  
  pars_flat <- mcmc_output$pars
  
  calc_summ <- function(samps) {
    qs <- unname(round(stats::quantile(samps, probs = c(0.025, 0.5, 0.975), na.rm = TRUE), 3))
    post_mean <- round(mean(samps, na.rm = TRUE), 3)
    c(qs, post_mean)
  }
  
  results_list <- vector("list", nrow(delay_map))
  
  for (i in seq_len(nrow(delay_map))) {
    
    raw_dist <- as.character(delay_map$distribution[i])
    is_gamma <- grepl("gamma", raw_dist, ignore.case = TRUE)
    dist_clean <- if (is_gamma) "Gamma" else "Log-Normal"
    
    clean_group <- clean_group_name(delay_map$group[[i]])
    from_name <- clean_event_name(delay_map$from[i])
    to_name <- clean_event_name(delay_map$to[i])
    delay_name <- paste(from_name, "to", to_name)
    
    if (is_gamma) {
      mean_samps <- pars_flat[paste0("delay", i, "_mean"), ]
      shape_samps <- pars_flat[paste0("delay", i, "_shape"), ]
      cv_samps <- 1 / sqrt(shape_samps)
      params <- list(
        Mean  = calc_summ(mean_samps),
        CV    = calc_summ(cv_samps),
        Shape = calc_summ(shape_samps),
        Scale = calc_summ(mean_samps / shape_samps)
      )
      
    } else {
      meanlog_samps <- pars_flat[paste0("delay", i, "_meanlog"), ]
      prec_samps <- pars_flat[paste0("delay", i, "_precisionlog"), ]
      sdlog_samps <- sqrt(1 / prec_samps)
      params <- list(
        Mean    = calc_summ(exp(meanlog_samps + (sdlog_samps^2) / 2)),
        CV      = calc_summ(sqrt(exp(sdlog_samps^2) - 1)),
        Meanlog = calc_summ(meanlog_samps),
        Sdlog   = calc_summ(sdlog_samps)
      )
    }
    
    row <- do.call(rbind, lapply(names(params), function(pname) {
      s <- params[[pname]]
      data.frame(
        Group = clean_group,
        Delay = delay_name,
        Distribution = dist_clean,
        Parameter = pname,
        Posterior_Mean = s[4],
        Posterior_Median = s[2],
        Lower_95_CrI = s[1],
        Upper_95_CrI = s[3],
        row.names = NULL
        )
      }))
    
    results_list[[i]] <- row
  }
  
  do.call(rbind, results_list)
}
