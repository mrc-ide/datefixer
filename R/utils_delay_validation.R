#' Validate inputs for delay summary and plotting functions
#'
#' @param mcmc_output Output list from `chronofix_mcmc_run()`.
#' @param delay_map The delay map used for the model setup.
#'
#' @importFrom cli cli_abort
#' @noRd
validate_delay_inputs <- function(mcmc_output, delay_map) {
  
  if (missing(mcmc_output)) {
    cli::cli_abort(c(
      "x" = "{.arg mcmc_output} is missing.",
      "i" = "Please provide the output list from {.fn chronofix_mcmc_run}."
    ))
  }
  if (missing(delay_map)) {
    cli::cli_abort(c(
      "x" = "{.arg delay_map} is missing.",
      "i" = "Please provide the delay map used for the model setup."
    ))
  }
  if (is.null(mcmc_output$pars)) {
    cli::cli_abort(c(
      "x" = "{.arg mcmc_output} must contain a {.field pars} array."
    ))
  }
  
  param_names <- rownames(mcmc_output$pars)
  
  if (is.null(param_names)) {
    cli::cli_abort(c(
      "x" = "Parameter names are missing from the {.code mcmc_output$pars} array.",
      "i" = "Ensure the array has row names corresponding to your model parameters."
    ))
  }
  
  required_cols <- c("group", "from", "to", "distribution")
  missing_cols <- setdiff(required_cols, names(delay_map))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = "{.arg delay_map} is missing required column{?s}: {.val {missing_cols}}."
    ))
  }
  
  # Validate supported distributions
  supported_dists <- grepl("gamma|log", delay_map$distribution, ignore.case = TRUE)
  if (!all(supported_dists)) {
    bad_dists <- unique(delay_map$distribution[!supported_dists])
    cli::cli_abort(c(
      "x" = "Unsupported distribution{?s} found in {.arg delay_map}: {.val {bad_dists}}.",
      "i" = "Currently, only {.val gamma} and {.val log-normal} are supported."
    ))
  }
  
  # check for all required delay parameters
  required_pars <- character()
  for (i in seq_len(nrow(delay_map))) {
    dist <- tolower(as.character(delay_map$distribution[i]))
    if (grepl("gamma", dist)) {
      required_pars <- c(required_pars, 
                         paste0("delay", i, "_mean"), 
                         paste0("delay", i, "_shape"))
    } else {
      required_pars <- c(required_pars, 
                         paste0("delay", i, "_meanlog"), 
                         paste0("delay", i, "_precisionlog"))
    }
  }
  
  missing_pars <- setdiff(required_pars, param_names)
  if (length(missing_pars) > 0) {
    cli::cli_abort(c(
      "x" = "Missing parameter{?s} in {.code mcmc_output$pars}: {.val {missing_pars}}."
    ))
  }
  
  invisible(TRUE)
}
