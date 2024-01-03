YearlyFrostDaysPlot <- function(data, ref_start_year, ref_end_year, max_date) {
  # Calculate number of days with frost per year
  plot_data <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
                     date <= as.Date(paste0(ref_end_year, "-12-31")))) |> # Only ref period data
    dplyr::group_by(year) |> 
    dplyr::summarise(frostdays = sum(tmin < 0, na.rm = TRUE)) |> 
    dplyr::as_tibble()
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = year, y = frostdays)) +
    ggplot2::geom_line(aes(color = "frost")) +
    ggplot2::geom_smooth(aes(color = "trend"), method = lm, se = FALSE) +
    ggplot2::scale_color_manual(
      breaks = c("frost", "trend"), values = c("trend" = "blue", "frost" = "black"), 
      labels = c("trend" = paste0("Trend (", ref_start_year, "-", ref_end_year, ")"), 
                 "frost" = "Days with frost")) +
    ggplot2::scale_x_continuous(
      breaks = seq(from = min(plot_data$year), to = max(plot_data$year), by = 10)) +
    ggplot2::scale_y_continuous(
      breaks = seq(from = min(plot_data$frostdays), to = max(plot_data$frostdays), by = 5)) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Temperature in Madrid - Retiro"),
      subtitle = paste0(
        "Annual number of days with frost (", ref_start_year, "-", ref_end_year, ")"),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)"),
      color = NULL) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.85, 0.9),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5)
    )
  
  return(list(p, plot_data, "year", "frostdays"))
  
}