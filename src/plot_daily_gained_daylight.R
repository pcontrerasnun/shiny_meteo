DailyDaylightGainedPlot <- function(data, selected_year, max_date) {
  # Calculate gained or lost daylight minutes per day
  plot_data <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
                    date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::mutate(dayduration = difftime(sunset, sunrise, units = "mins")) |> 
    dplyr::mutate(diffdayduration = round(dayduration - lag(dayduration), 2)) |> 
    dplyr::select(date, diffdayduration) |> 
    dplyr::as_tibble()
  
  # For coloring purposes
  color_data <- plot_data |> 
    dplyr::reframe(x = seq(min(as.Date(plot_data$date), na.rm = TRUE), max(as.Date(plot_data$date), na.rm = TRUE), length = 1000),
                   y1 = approx(as.Date(date), diffdayduration, xout = x)$y,
                   diff = 0 - y1)
  
  # Draw the plot
  p <- ggplot2::ggplot(plot_data, aes(x = date, y = diffdayduration)) + 
    ggplot2::annotate("rect", ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.2,
                      xmin = seq(ymd(paste0(selected_year, "-02-01")), by = "2 month", length = 6),
                      xmax = seq(ymd(paste0(selected_year, "-03-01")), by = "2 month", length = 6)) +
    ggplot2::geom_segment(data = color_data, aes(x = x, y = 0, xend = x, yend = y1, color = diff),
                          linewidth = 1, na.rm = TRUE) +
    ggplot2::geom_point(data = plot_data |> dplyr::filter(date == Sys.Date())) +
    ggrepel::geom_label_repel(
      data = plot_data |> dplyr::filter(date == Sys.Date()),
      aes(y = diffdayduration, label = round(diffdayduration, 2))
    ) +
    ggplot2::scale_color_gradient2(high = "#fff7bc", mid = "#fec44f", low = "#d95f0e", guide = guide_none()) +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd(paste0(selected_year, "-01-01")), ymd(paste0(selected_year, "-12-31")), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd(paste0(selected_year, "-01-01"))), 
                 as.numeric(ymd(paste0(as.numeric(selected_year) + 1), "-01-01")))) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, " mins"),
      limits = c(-3, 3), breaks = seq(from = -3, to = 3, by = 1)
    ) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Sunlight in Madrid - Retiro ", selected_year),
      subtitle = "Gained daylight minutes (daily change)",
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/"
      )) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25), 
    )
  
  # If year of study is current year then plot vertical line
  if (selected_year == year(Sys.Date())) {
    p <- p + ggplot2::geom_vline(xintercept = Sys.Date()) 
  }
  
  return(list(p, plot_data, "date", "diffdayduration"))
  
}
