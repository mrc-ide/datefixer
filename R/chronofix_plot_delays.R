#' @title Plot Estimated Delay Distributions
#' 
#' @param mcmc_output Output list from `chronofix_mcmc_run()`
#' @param delay_map The delay_map used for the model setup
#' @param n_points Number of points along the x-axis to evaluate (default 200)
#' @param facet_by_group Logical, if TRUE, creates a grid with a row per group
#'  (default TRUE)
#' @param share_x_axis Logical, if TRUE, plots all distributions on the same
#'  x-axis scale (default TRUE)
#' 
#' @import ggplot2
#' @importFrom stats median quantile dgamma dlnorm qgamma qlnorm
#' @importFrom ggtext element_markdown element_textbox_simple
#' @importFrom grid unit
#' @import patchwork
#' @export
chronofix_plot_delays <- function(mcmc_output,
                                  delay_map,
                                  n_points = 200,
                                  facet_by_group = TRUE,
                                  share_x_axis = TRUE) {
  
  validate_delay_inputs(mcmc_output, delay_map)
  
  pars_flat <- mcmc_output$pars
  
  local_max_x <- numeric(nrow(delay_map))
  
  for (i in seq_len(nrow(delay_map))) {
    raw_dist <- as.character(delay_map$distribution[i])
    is_gamma <- grepl("gamma", raw_dist, ignore.case = TRUE)
    
    if (is_gamma) {
      mean_samps <- pars_flat[paste0("delay", i, "_mean"), ]
      shape_samps <- pars_flat[paste0("delay", i, "_shape"), ]
      scale_samps <- mean_samps / shape_samps
      local_max_x[i] <- stats::qgamma(0.99,
                                      shape = mean(shape_samps, na.rm = TRUE),
                                      scale = mean(scale_samps, na.rm = TRUE))
      } else {
      meanlog_samps <- pars_flat[paste0("delay", i, "_meanlog"), ]
      prec_samps <- pars_flat[paste0("delay", i, "_precisionlog"), ]
      sdlog_samps <- sqrt(1 / prec_samps)
      local_max_x[i] <- stats::qlnorm(0.99,
                                      meanlog = mean(meanlog_samps, na.rm = TRUE),
                                      sdlog = mean(sdlog_samps, na.rm = TRUE))
      }
    }
    
  global_max_x <- max(local_max_x, na.rm = TRUE)
  
  plot_data_list <- list()
  
  for (i in seq_len(nrow(delay_map))) {
    raw_dist <- as.character(delay_map$distribution[i])
    is_gamma <- grepl("gamma", raw_dist, ignore.case = TRUE)
    dist_clean <- if (is_gamma) "Gamma" else "Log-Normal"
    
    clean_group <- clean_group_name(delay_map$group[[i]])
    clean_group_wrapped <- paste(strwrap(clean_group, width = 40), collapse = "<br>")
    
    clean_from <- clean_event_name(delay_map$from[i])
    clean_to <- clean_event_name(delay_map$to[i])
    
    group_title <- sprintf("Group: %s", clean_group)
    delay_title <- sprintf("%s to %s", clean_from, clean_to)
    
    # for facet_by_group = FALSE
    panel_title <- sprintf(
      "<span style='color: #1F77B4;'>Group: %s</span><br><span style='color: #000000;'>%s</span>",
      clean_group_wrapped, delay_title
    )
    
    current_max_x <- if (share_x_axis) global_max_x else local_max_x[i]
    x_seq <- seq(0.01, current_max_x, length.out = n_points)
    
    if (is_gamma) {
      mean_samps <- pars_flat[paste0("delay", i, "_mean"), ]
      shape_samps <- pars_flat[paste0("delay", i, "_shape"), ]
      scale_samps <- mean_samps / shape_samps
      
      dens_matrix <- t(sapply(x_seq, function(x) {
        stats::dgamma(x, shape = shape_samps, scale = scale_samps)
      }))
      
    } else {
      meanlog_samps <- pars_flat[paste0("delay", i, "_meanlog"), ]
      prec_samps <- pars_flat[paste0("delay", i, "_precisionlog"), ]
      sdlog_samps <- sqrt(1 / prec_samps)
      
      dens_matrix <- t(sapply(x_seq, function(x) {
        stats::dlnorm(x, meanlog = meanlog_samps, sdlog = sdlog_samps)
      }))
    }
  
    mean_line <- rowMeans(dens_matrix, na.rm = TRUE)
    quants <- apply(dens_matrix, 1, stats::quantile,
                    probs = c(0.025, 0.975), na.rm = TRUE)
    
    plot_data_list[[i]] <- data.frame(
      Panel_Title = panel_title,
      Group_Title = group_title,
      Delay_Title = delay_title,
      Distribution = dist_clean, 
      x = x_seq,
      lower = quants[1, ], # 2.5%
      mean_density = mean_line,
      upper = quants[2, ] # 97.5%
    )
  }
  
  plot_data <- do.call(rbind, plot_data_list)
  plot_data$Distribution <- factor(plot_data$Distribution, levels = c("Gamma", "Log-Normal"))
  plot_data$Group_Title <- factor(plot_data$Group_Title, levels = unique(plot_data$Group_Title))
  plot_data$Panel_Title <- factor(plot_data$Panel_Title, levels = unique(plot_data$Panel_Title))
  plot_data$Delay_Title <- factor(plot_data$Delay_Title, levels = unique(plot_data$Delay_Title))
  
  dist_colours <- c("Gamma" = "#A7C1E1", "Log-Normal" = "#B7E4C7")
  line_colours <- c("Gamma" = "#4B7BB6", "Log-Normal" = "#52B788")
  
  facet_scales <- if (share_x_axis) "free_y" else "free"
  
  build_base_plot <- function(df) {
    
    ggplot(df, aes(x = x, fill = Distribution, colour = Distribution)) +
      geom_ribbon(aes(ymin = lower, ymax = upper),
                  alpha = 0.55, colour = NA, show.legend = TRUE) +
      geom_line(aes(y = mean_density), linetype = "dashed",
                linewidth = 1, show.legend = TRUE) +
      scale_fill_manual(name = "Distribution", values = dist_colours,
                        limits = names(dist_colours), drop = FALSE) +
      scale_colour_manual(name = "Distribution", values = line_colours,
                         limits = names(line_colours), drop = FALSE) +
      guides(
        fill = guide_legend(override.aes = list(alpha = 0.55, linetype = "dashed")),
        colour = guide_legend()
      ) +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_bw(base_size = 12) +
      theme(
        strip.text = element_markdown(face = "bold", size = 9, lineheight = 1.2,
                                      margin = margin(b = 6, t = 6)),
        strip.background = element_rect(fill = "#f8f9fa", colour = "#cccccc"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
        legend.title = element_text(face = "bold", size = 11),
        legend.position = "bottom"
      )
  }
  
  group_band_theme <- function() {
    theme(plot.title = element_textbox_simple(
      face = "bold",
      size = 11,
      colour = "#1F77B4",
      halign = 0,
      padding = margin(5, 8, 5, 8),
      margin = margin(b = 4),
      r = grid::unit(2, "pt")
    ))
  }
  
  if (facet_by_group) {
    
    groups <- unique(plot_data$Group_Title)
    plot_list <- list()
    n_panels <- integer(length(groups))
    
    for (k in seq_along(groups)) {
      g <- groups[k]
      g_data <- plot_data[plot_data$Group_Title == g, ]
      n_panels[k] <- length(unique(g_data$Delay_Title))
      is_last <- k == length(groups)
      
      plot_list[[k]] <- build_base_plot(g_data) +
        facet_wrap(~ Delay_Title, scales = facet_scales, nrow = 1) +
        labs(title = g, x = "Delay (Days)", y = "Probability Density") +
        group_band_theme()
    }
    
    # one column per delay in the longest row
    design <- Reduce(c, lapply(seq_along(plot_list), function(k) {
      patchwork::area(t = k, l = 1, b = k, r = n_panels[k])
    }))
    
    p <- patchwork::wrap_plots(plot_list, design = design) +
      patchwork::plot_layout(guides = "collect") +
      patchwork::plot_annotation(
        title = "Posterior Estimated Delay Distributions",
        subtitle = "Dashed curve: Posterior Mean. Shaded area: 95% CrI.",
        theme = theme(plot.title = element_text(face = "bold", size = 14),
                      legend.position = "bottom")
      )
    
  } else {
    p <- build_base_plot(plot_data) +
      facet_wrap(~ Panel_Title, scales = facet_scales, ncol = 3) +
      labs(
        x = "Delay (Days)", 
        y = "Probability Density",
        title = "Posterior Estimated Delay Distributions",
        subtitle = "Dashed curve: Posterior Mean. Shaded area: 95% CrI."
      ) +
      theme(
        legend.position = "top",
        legend.justification = "right",
        legend.title = element_text(size = 11)
      )
  }
  
  return(p)
}
