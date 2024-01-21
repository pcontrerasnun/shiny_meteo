DailySunlightTimesPlot <- function(data, selected_year, max_date) {
  # Just select data for selected year and round time
  plot_data <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
                    date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::mutate(
      sunrise = lubridate::round_date(sunrise, unit = "minute"),
      sunset = lubridate::round_date(sunset, unit = "minute"),
      zenith = lubridate::round_date(solarNoon, unit = "minute"),
      dawn = lubridate::round_date(dawn, unit = "minute"),
      dusk = lubridate::round_date(dusk, unit = "minute")
    ) |> 
    dplyr::select(date, dawn, sunrise, zenith, sunset, dusk, -c(solarNoon, lat, lon)) |> 
    dplyr::as_tibble()
  
  # Draw the plot
  p <- ggplot2::ggplot(plot_data, aes(x = date)) + 
    ggplot2::annotate(
      "rect", ymin = as.POSIXct(-Inf), ymax = as.POSIXct(Inf),
      fill = "gray", alpha = 0.2,
      xmin = seq(ymd(paste0(selected_year, "-02-01")), by = "2 month", length = 6),
      xmax = seq(ymd(paste0(selected_year, "-03-01")), by = "2 month", length = 6)
    ) +
    ggplot2::geom_line(aes(y = as.POSIXct(format(sunrise, "%H:%M"), format = "%H:%M"), color = "sunrise"), linewidth = 0.9) +
    ggplot2::geom_line(aes(y = as.POSIXct(format(sunset, "%H:%M"), format = "%H:%M"), color = "sunset"), linewidth = 0.9) +
    ggplot2::geom_line(aes(y = as.POSIXct(format(zenith, "%H:%M"), format = "%H:%M"), color = "zenith"), linewidth = 0.9) +
    ggplot2::geom_line(aes(y = as.POSIXct(format(dawn, "%H:%M"), format = "%H:%M"), color = "dawn"), linewidth = 0.9) +
    ggplot2::geom_line(aes(y = as.POSIXct(format(dusk, "%H:%M"), format = "%H:%M"), color = "dusk"), linewidth = 0.9) +
    ggplot2::scale_color_manual(
      values = c("dawn" = "#bd0026", "sunrise" = "#f03b20", "zenith" = "#fd8d3c", "sunset" = "#e31a1c",
                 "dusk" = "#b10026")
      ) +
    ggrepel::geom_label_repel(
      data = plot_data |> dplyr::filter(date == Sys.Date()) |> tidyr::pivot_longer(cols = c(dawn, sunrise, zenith, sunset, dusk)),
      aes(y = as.POSIXct(format(value, "%H:%M"), format = "%H:%M"), label = format(value, "%H:%M"))
      ) +
    ggplot2::scale_y_datetime(
      date_labels = "%H:%M", date_breaks = "1 hour",
      limits = c(as.POSIXct(min(format(plot_data$dawn, "%H:%M")), format = "%H:%M") - as.difftime(30, units = "mins"),
                 as.POSIXct(max(format(plot_data$dusk, "%H:%M")), format = "%H:%M") + as.difftime(30, units = "mins"))
      ) +  
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd(paste0(selected_year, "-01-01")), ymd(paste0(selected_year, "-12-31")), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd(paste0(selected_year, "-01-01"))), 
                 as.numeric(ymd(paste0(as.numeric(selected_year) + 1), "-01-01")))) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Sunlight in Madrid - Retiro ", selected_year),
      subtitle = "Sunlight core times",
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/"
      )) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.position = "none"
    )
  
  # If year of study is current year then plot vertical line
  if (selected_year == year(Sys.Date())) {
    p <- p + ggplot2::geom_vline(xintercept = Sys.Date()) 
  }
  
  return(list(p, plot_data, "date", "zenith"))
  
}
