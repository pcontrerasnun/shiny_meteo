DensityTmeanPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date, title) {
  # Just some renaming
  plot_data <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
                     date <= as.Date(paste0(ref_end_year, "-12-31"))) |
                    (date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
                       date <= as.Date(paste0(as.numeric(selected_year), "-12-31")))) |> # Include year of study
    dplyr::mutate(factor = ifelse(year == selected_year, "Study", "Reference")) |> 
    dplyr::select(factor, tmean) |> 
    dplyr::as_tibble()

  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = tmean, y = factor(factor), fill = after_stat(x))) +
    ggridges::geom_density_ridges_gradient(quantile_lines = TRUE, quantiles = 2, na.rm = TRUE) +
    ggplot2::scale_fill_gradientn(colors = c("#2c7bb6", "white", "#d7191c"), guide = guide_none()) +
    ggplot2::scale_y_discrete(
      limits = c("Study", "Reference"),
      labels = c(selected_year, paste0("(", ref_start_year, "-", ref_end_year, ")"))) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(x, "ºC"),
      breaks = seq(from = round(min(plot_data$tmean, na.rm = TRUE) - 10), to = round(max(plot_data$tmean, na.rm = TRUE) + 10), by = 5),
      limits = c(min(plot_data$tmean, na.rm = TRUE) - 5, max(plot_data$tmean, na.rm = TRUE) + 5)
      ) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Temperature in ", title, " ", selected_year),
      subtitle = paste0("Annual mean temperature density vs. historical density (", ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/")
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25)
    )
  
  return(list(p, plot_data, "tmean", "factor"))
  
}

