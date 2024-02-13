OverviewPcpTempPlot3 <- function(data_temp, data_pcp, selected_year, max_date, title) {
  plot_data_pcp <- data_pcp |>     
    dplyr::filter(date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) & 
                    date <= as.Date(paste0(as.numeric(selected_year), "-12-31"))) |> 
    dplyr::filter(pcp >= 0.1) |> 
    dplyr::arrange(-pcp)
  
  plot_data_temp <- data_temp |> 
    dplyr::filter(date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) & 
                    date <= as.Date(paste0(as.numeric(selected_year), "-12-31")))
  
  plot_data <- dplyr::left_join(plot_data_temp, plot_data_pcp, by = c("date", "day", "month", "year")) |> 
    dplyr::select(-c(day, month, year))
  
  scale <- abs((round(seq(from = round(min(plot_data$tmin, na.rm = TRUE) - 5), to = 45, by = 5) / 5) * 5)[1])
  
  ggplot2::ggplot(data = plot_data, aes(x = date)) +
    ggplot2::geom_col(aes(y = pcp, fill = "pcp"), na.rm = TRUE) +
    ggplot2::geom_line(aes(y = tmean, color = "tmean")) +
    ggplot2::geom_line(aes(y = tmax, color = "tmax")) +
    ggplot2::geom_line(aes(y = tmin, color = "tmin")) +
    ggplot2::scale_fill_manual(
      values = c("pcp" = "#2c7bb6"), 
      label = paste0("Daily pcp."), guide = guide_legend(order = 1)) +
    ggplot2::scale_color_manual(
      values = c("tmean" = "black", "tmin" = "blue", "tmax" = "red"), 
      label = c("tmean" = "Daily mean temp.", "tmin" = "Daily min temp.", "tmax" = "Daily max temp.")) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "ºC"),
      breaks = round(seq(from = round(min(plot_data$tmin, na.rm = TRUE) - 5), to = 45, by = 5) / 5) * 5,
      limits = c(min(plot_data$tmin, na.rm = TRUE) - 5, 45), 
      sec.axis = sec_axis(trans = ~. +scale, labels = function(x) paste0(x, "mm")
#                         breaks = round(seq(from = round(min(plot_data$pcp, na.rm = TRUE) - 4),
#                                            to = 40, by = 5) / 5) * 5
                          )
#      breaks = round(seq(from = round(min(plot_data$tmin, na.rm = TRUE) - 4), 
#                         to = round(max(plot_data$tmax, na.rm = TRUE) + 3), by = 5) / 5) * 5)
      ) +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd(paste0(selected_year, "-01-15")), 
                              ymd(paste0(selected_year, "-12-31")), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd(paste0(selected_year, "-01-01"))), 
                 as.numeric(ymd(paste0(selected_year, "-12-31"))))) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation and temperature in ", title, " ", selected_year),
      subtitle = paste0("Daily precipitation and daily mean, max and min temperatures"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/")) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25), 
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.1335, 0.8),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.title = element_blank())

  return(list(p, plot_data, "date", "tmean"))
}
  