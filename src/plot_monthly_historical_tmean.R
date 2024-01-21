MonthlyHistoricalTmeanPlot <- function(data, ref_start_year, ref_end_year, max_date) {
  # Calculate normal mean temp of each month in reference period
  reference_monthly_pcts_tmean <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::group_by(year, month) |> 
    dplyr::summarise(tmean = mean(tmean, na.rm = TRUE), .groups = "keep") |> # .groups to avoid warnings
    dplyr::group_by(month) |> 
    dplyr::summarise(
      p50tmean = round(quantile(tmean, probs = 0.50, na.rm = TRUE), 1),
      .groups = "keep" # .groups to avoid warnings
    ) |> 
    dplyr::as_tibble()
  
  # Calculate mean temperature for each month in all history
  all_monthly_tmean <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::group_by(year, month) |> 
    dplyr::summarise(tmean = round(mean(tmean, na.rm = TRUE), 1), .groups = "keep") |> # .groups to avoid warnings
    dplyr::as_tibble()
  
  # Join data and calculate anomalies
  plot_data <- dplyr::left_join(all_monthly_tmean, reference_monthly_pcts_tmean, by = "month") |> 
    mutate(diffmedian = round(tmean - p50tmean, 1)) |> 
    dplyr::mutate(year = as.numeric(year))
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = year, y = tmean)) +
    ggplot2::geom_line(aes(color = "tmean"), show.legend = FALSE) +
    ggplot2::geom_line(aes(y = p50tmean, color = "q50"), linetype = "dashed") +
    ggplot2::geom_smooth(aes(color = "trend", group = month), linetype = "solid", linewidth = 0.5,
                         method = lm, se = FALSE, na.rm = TRUE, show.legend = FALSE) + 
    ggplot2::facet_wrap(vars(month), labeller = labeller(month = c("01" = "Jan", "02" = "Feb", "03" = "Mar", 
                                                                   "04" = "Apr", "05" = "May", "06" = "Jun", 
                                                                   "07" = "Jul", "08" = "Aug", "09" = "Sep", 
                                                                   "10" = "Oct", "11" = "Nov", "12" = "Dec"))) +
    ggplot2::scale_color_manual(values = c("trend" = "blue", "q50" = "black", "tmean" = "black"), 
                                labels = c("trend" = paste0("Trend (", ref_start_year, "-", ref_end_year, ")"), 
                                           "tmean" = "Monthly mean temp.",
                                           "q50" = paste0("Monthly median mean temp. (", ref_start_year,
                                                          "-", ref_end_year, ")")),
                                breaks = c("tmean", "q50", "trend")) +
    ggplot2::scale_x_continuous(breaks = seq(from = min(plot_data$year), to = max(plot_data$year), by = 10)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "ºC"),
                                breaks = seq(from = round(min(plot_data$tmean, na.rm = TRUE), digits = -1),
                                             to = max(plot_data$tmean, na.rm = TRUE) + 2, by = 5),
                                limits = c(round(min(plot_data$tmean, na.rm = TRUE), digits = -1) - 2,
                                           max(plot_data$tmean, na.rm = TRUE) + 2)) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = "Temperature in Madrid - Retiro",
      subtitle = paste0("Historical monthly mean temperature (", ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/"),
      color = NULL, fill = NULL) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(
        fill = "white", color = "black",
        linewidth = 0.75
      ),
      legend.position = c(0.125, 0.925),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.text = ggtext::element_markdown(),
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, size = 10)
    ) +
    ggplot2::guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed", "solid"),
                                                             linewidth = c(0.5, 0.5, 0.85))))
  
  return(list(p, plot_data, "year", "tmean"))
}
