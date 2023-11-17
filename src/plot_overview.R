OverviewPcpTempPlot <- function(data, selected_year, max_date) {
  # Precipitation data
  plot_data_pcp <- data |>     
    dplyr::filter(date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) & 
                    date <= as.Date(paste0(as.numeric(selected_year), "-12-31"))) |> 
    dplyr::filter(pcp >= 0.1) |> 
    dplyr::arrange(-pcp)
  
  # Temperature data
  plot_data_temp <- data |> 
    dplyr::filter(date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) & 
                    date <= as.Date(paste0(as.numeric(selected_year), "-12-31")))
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data_pcp, aes(x = date)) +
    ggplot2::geom_violin(data = plot_data_temp, aes(y = tmean, group = month), fill = "orange", 
                         alpha = 0.5, na.rm = TRUE) +
    ggplot2::geom_point(aes(y = pcp, size = pcp, color = pcp), alpha = 0.5) +
    ggplot2::scale_size(breaks = c(0.1, 5, 10, 25, 60), name = "Precipitation", range = c(2, 20),
                        labels = c(">0.1mm", ">5mm", ">10mm", ">25mm", ">60mm")) +
    ggrepel::geom_label_repel(data = head(plot_data_pcp, 3), aes(y = pcp, label = paste0(pcp, "mm")),
                              segment.color = 'transparent') +
    ggplot2::scale_color_gradient(high = "#08306b", low = "#6baed6", guide = guide_none()) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "ºC/mm"),
                                breaks = seq(from = 0, to = max(
                                  max(plot_data_pcp$pcp, na.rm = TRUE),
                                  max(plot_data_temp$tmean, na.rm = TRUE)) + 7, by = 5),
                                limits = c(0, max(
                                  max(plot_data_pcp$pcp, na.rm = TRUE),
                                  max(plot_data_temp$tmean, na.rm = TRUE)) + 7)) +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd(paste0(selected_year, "-01-01")), 
                              ymd(paste0(selected_year, "-12-31")), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd(paste0(selected_year, "-01-01"))), 
                 as.numeric(ymd(paste0(selected_year, "-12-31"))))) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation and temperature in Madrid - Retiro ", selected_year),
      subtitle = paste0("Daily precipitation and daily mean temperature"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)")) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25), 
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.095, 0.75),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.title = element_blank()) +
    ggplot2::guides(
      size = guide_legend(override.aes = list(color = c("#6baed6", "#4292c6", "#2171b5", "#08519c", 
                                                        "#08306b")[1:cut(head(plot_data_pcp$pcp, 1), 
                                                                          breaks = c(0.1, 5, 10, 25, 60, Inf), 
                                                                          labels = FALSE, right = FALSE,
                                                                          include.lowest = TRUE)]))
      # The cut() is to adapt number of colors to number of groups depending on max pcp in the year
      )
  
  return(p)
}
