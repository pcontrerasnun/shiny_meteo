AnnualDaysWithPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate number of days with precip. in each year and total precip. in each year
  plot_data <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
                     date <= as.Date(paste0(ref_end_year, "-12-31"))) |
                    (date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) & # Include year of study
                       date <= as.Date(paste0(as.numeric(selected_year), "-12-31")))) |> 
    dplyr::group_by(year) |> 
    dplyr::summarise(dayspcp = sum(pcp > 0, na.rm = TRUE),
                     sumpcp = sum(pcp, na.rm = TRUE)) |> 
    dplyr::arrange(-dayspcp) |> 
    dplyr::mutate(rank = rank(-dayspcp, ties.method = "first")) |> 
    dplyr::as_tibble()
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = dayspcp, y = sumpcp)) +
    ggplot2::geom_point() +
   # ggplot2::scale_color_gradientn(colors = wes_palette("Zissou1", 100, type = "continuous")) +
    ggplot2::geom_point(data = subset(plot_data, year == selected_year), fill = "red", size = 3, stroke = 1, 
                        shape = 21) +
    ggrepel::geom_label_repel(data = subset(plot_data, year == selected_year), aes(label = year),
                              fontface = "bold", size = 5, color = "black") +
    ggplot2::geom_rug(col = "blue") +
    ggplot2::scale_x_continuous(
      breaks = seq(round(min(plot_data$dayspcp) - 6, digits = -1), # +-6 is for correct rounding
                   round(max(plot_data$dayspcp) + 6, digits = -1) + 10, by = 5),
      limits = c(round(min(plot_data$dayspcp) - 6, digits = -1), 
                 round(max(plot_data$dayspcp) + 6, digits = -1) + 10, by = 5)) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "mm"),
      breaks = seq(from = round(min(plot_data$sumpcp), digits = -1), 
                   to = round(max(plot_data$sumpcp), digits = -1) + 100, by = 100),
      limits = c(min(plot_data$sumpcp), max(plot_data$sumpcp) + 100)
    ) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      y = "Annual total precip.", x = "Days with precip.", title = "Precipitation in Madrid - Retiro",
      subtitle = paste0(
        "Annual number of days with precipitation (",
        min(plot_data$year), "-", max(plot_data$year), ")"),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/"
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
    )
  
  return(list(p, plot_data, "dayspcp", "sumpcp"))
  
}
