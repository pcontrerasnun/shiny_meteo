AnnualTmeanDistributionPlot <- function(data, max_date, title, selected_year) {
  # Calculate mean temperature by year
  reference_annual_tmean <- data |>
    dtplyr::lazy_dt() |>
    dplyr::group_by(year) |> 
    dplyr::summarise(tmean = mean(tmean, na.rm = TRUE)) |> 
    dplyr::as_tibble() |> 
    dplyr::arrange(tmean)
  
  # Calculate histogram data
  h <- hist(reference_annual_tmean$tmean, breaks = 10, freq = FALSE)
  df_points = data.frame(
    x = unlist(sapply(1:length(h$mids), function(i) rep(h$mids[i], each = h$counts[i]))), # pcp value (x)
    y = unlist(sapply(1:length(h$mids), function(i) seq(0 + 0.009, h$density[i] - 0.009,  length.out = h$counts[i]))) 
    # position of value (y) inside bar within histogram
  )
  
  # Join data
  plot_data <- cbind(df_points, reference_annual_tmean)
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = tmean)) +
    ggplot2::geom_histogram(aes(y = after_stat(density)), breaks = h$breaks, color = "black", fill = "white") +
    ggplot2::geom_label(aes(x = x, y = y, label = year, fill = as.numeric(year)), color = "white",
                        fontface = "bold") +
    #ggplot2::scale_color_viridis_c(option = "B", end = 0.8) +
    ggplot2::scale_fill_gradientn(colors = wes_palette("Zissou1", 100, type = "continuous")) + 
    ggplot2::scale_x_continuous(breaks = seq(round(min(plot_data$tmean)), round(max(plot_data$tmean)),
                                           by = 1), labels = function(x) paste0(x, "ºC")) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Temperature in ", title),
      subtitle = paste0(
        "Annual mean temperatures distribution (",
        min(plot_data$year), "-", max(plot_data$year), ")"
      ),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/"
      ), fill = "Year"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25), 
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = c(0.85, 0.8),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75)
    ) +
    ggplot2::guides(fill = guide_colorbar(ticks.colour = NA))
  
  return(list(p, plot_data, "tmean", "y"))
  
}
