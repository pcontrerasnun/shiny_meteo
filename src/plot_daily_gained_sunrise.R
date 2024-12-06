DailySunriseGainedPlot <- function(data, selected_year, max_date, title) {
  plot_data <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::mutate(sunrise_time = hms::as_hms(format(sunrise, "%H:%M:%S"))) |> 
    dplyr::mutate(diff_sunrise = difftime(sunrise_time, lag(sunrise_time), units = "mins")) |> 
    dplyr::mutate(diff_sunrise = ifelse(abs(diff_sunrise) > 30, diff_sunrise - 60 * sign(diff_sunrise), diff_sunrise)) |> # Por los cambios de hora de verano e invierno
    dplyr::mutate(diff_sunrise = diff_sunrise * -1) |> 
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) & # Filter after mutate so than Jan 1 is not NA
                    date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::select(date, diff_sunrise) |> 
    dplyr::as_tibble()
  
  # For coloring purposes
  color_data <- plot_data |> 
    dplyr::reframe(x = seq(min(as.Date(plot_data$date), na.rm = TRUE), max(as.Date(plot_data$date), na.rm = TRUE), length = 1000),
                   y1 = approx(as.Date(date), diff_sunrise, xout = x)$y,
                   diff = 0 - y1)
  
  p <- ggplot2::ggplot(plot_data, aes(x = date, y = diff_sunrise)) + 
    ggplot2::annotate("rect", ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.2,
                      xmin = seq(ymd(paste0(selected_year, "-02-01")), by = "2 month", length = 6),
                      xmax = seq(ymd(paste0(selected_year, "-03-01")), by = "2 month", length = 6)) +
    ggplot2::geom_segment(data = color_data, aes(x = x, y = 0, xend = x, yend = y1, color = diff),
                          linewidth = 1, na.rm = TRUE) +
    ggplot2::geom_point(data = plot_data |> dplyr::filter(date == Sys.Date())) +
    ggrepel::geom_label_repel(
      data = plot_data |> dplyr::filter(date == Sys.Date()),
      aes(y = diff_sunrise, label = round(diff_sunrise, 2))
    ) +
    ggplot2::scale_color_gradient2(high = "#0076C0", mid = "#BDDE9B", low = "#2CB11B", guide = guide_none()) +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd(paste0(selected_year, "-01-01")), ymd(paste0(selected_year, "-12-31")), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd(paste0(selected_year, "-01-01"))), 
                 as.numeric(ymd(paste0(as.numeric(selected_year) + 1), "-01-01")))) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, " mins"),
      limits = c(-2, 2), breaks = seq(from = -2, to = 2, by = 1)
    ) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Sunrise in ", title, " ", selected_year),
      subtitle = "Gained daylight minutes in the morning (daily change)",
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
  
  return(list(p, plot_data, "date", "diff_sunrise"))
  
}

