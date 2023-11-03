

YearlyPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate total precipitation by year
  reference_anual_total_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
                     date <= as.Date(paste0(ref_end_year, "-12-31"))) |
                    (date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
                       date <= as.Date(paste0(as.numeric(selected_year), "-12-31")))) |>
    dplyr::filter((year != '1928') & (year != '1938')) |> # WARNING: only for Madrid Retiro. Removing
    # due to very little data
    dplyr::group_by(year) |> 
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE)) |> 
    dplyr::as_tibble() |> 
    dplyr::arrange(sumpcp)
  
  # Calculate histogram data
  h <- hist(reference_anual_total_pcp$sumpcp, breaks = 10, freq = FALSE)
  df_points = data.frame(
    x = unlist(sapply(1:length(h$mids), function(i) rep(h$mids[i], each = h$counts[i]))), # pcp value (x)
    y = unlist(sapply(1:length(h$mids), function(i) seq(0 + 0.0001, h$density[i] - 0.0001, length.out = h$counts[i]))) 
    # position of value (y) inside bar within histogram
  )
  
  # Join data
  plot_data <- cbind(df_points, reference_anual_total_pcp)
  
  # Fit distributions
  # Apparently gamma distribution fits better in arid, semi-arid climates. Normal distribution
  # fit better in wet climates
  fit <- fitdistr(reference_anual_total_pcp$sumpcp, "gamma")
  shape_fit <- fit$estimate[1]
  rate_fit <- fit$estimate[2]
  
  x <- seq(min(h$breaks), max(h$breaks), length = 100)
  y <- dgamma(x, shape_fit, rate_fit)
  data_distr <- cbind(x, y) |> as_tibble()
  
  # For normal distribution
  # fitdistr(reference_anual_total_pcp$sumpcp, "gamma")
  # mean_fit <- fit$estimate[1]
  # sd_fit <- fit$estimate[2]
  # x <- seq(mean_fit - 3 * sd_fit, mean_fit + 3 * sd_fit, length = 100)
  # y <- dnorm(x, mean = mean_fit, sd = sd_fit)
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = sumpcp)) +
    ggplot2::geom_histogram(aes(y = ..density..), breaks = h$breaks, color = "black", fill = "white") +
    ggplot2::geom_line(data = data_distr, aes(x = x, y = y)) +
    ggplot2::geom_label(aes(x = x, y = y, label = year)) +
    # Highlight year of study
    ggplot2::geom_label(data = plot_data[which(plot_data$year == as.character(selected_year)), ],
                        aes(x = x, y = y, label = year), color = "red", fontface = "bold") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "mm"), breaks = h$breaks) +
    ggplot2::annotate(geom = "text", x = mean(h$breaks), y = 0.0035,
                      label = paste("Gamma(alpha==", round(shape_fit, 2), ", lambda==", round(rate_fit, 2), ")"), 
                      parse = TRUE, hjust = -0.05, vjust = 0.5) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation in Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Cumulative annual precipitation vs. historical values (",
        ref_start_year, "-", ref_end_year, ")"
      ),
      caption = paste0(
        "Updated: ", max_date, ", Source: AEMET OpenData, Graph: @Pcontreras95 (Twitter)"
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25), legend.position = "none"
    )
  
  return(p)

}
