DailyTmeanAnomaliesPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate percentiles of tmean across every day of the year
  reference_daily_pcts_tmean <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    # Smoothing with +- 15 days
    dplyr::mutate(rolling_tmean = round(zoo::rollapply(tmean, width = 2 * 15 + 1, FUN = mean, 
                                                       align = "center", fill = NA), 2)) |>
    dplyr::group_by(day, month) |> 
    dplyr::summarise(
      q05tmean = round(quantile(rolling_tmean, probs = 0.05, na.rm = TRUE), 1),
      q50tmean = round(quantile(rolling_tmean, probs = 0.50, na.rm = TRUE), 1),
      q95tmean = round(quantile(rolling_tmean, probs = 0.95, na.rm = TRUE), 1),
      .groups = "keep" # to avoid warning
    ) |>
    # We choose 2023 as year because it doesn't have 29th Feb
    dplyr::mutate(date = as.Date(paste("2023", month, day, sep = "-"), format = "%Y-%m-%d")) |> 
    dplyr::as_tibble()
  
  # Get daily mean temperatures
  selected_year_daily_tmean <- data |>     
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
                    date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::select(date, tmean) |> 
    dplyr::as_tibble()
  
  # Join data and calculate anomalies (diff with median)
  plot_data <- dplyr::left_join(reference_daily_pcts_tmean, selected_year_daily_tmean, by = "date") |> 
    dplyr::mutate(diffmedian = round(tmean - q50tmean, 1)) |> 
    dplyr::arrange(-diffmedian) |> 
    dplyr::select(date, q05tmean, q50tmean, q95tmean, tmean, diffmedian)
  
  # For geom_segment coloring purposes
  color_data <- plot_data |> 
    dplyr::reframe(x = seq(min(plot_data$date, na.rm = TRUE), max(plot_data$date, na.rm = TRUE), length = 1000),
                   y1 = approx(date, tmean, xout = x)$y,
                   y2 = approx(date, q50tmean, xout = x)$y,
                   diff = y1 - y2)
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = date, y = tmean)) +
    ggplot2::geom_segment(data = color_data, aes(x = x, y = y1, xend = x, yend = y2, color = diff),
                          linewidth = 1, na.rm = TRUE) +
    ggplot2::scale_color_gradient2(high = "#d7191c", mid = "white", low = "#2c7bb6", guide = guide_none()) +
    ggplot2::geom_line(linewidth = 0.65, na.rm = TRUE) +
    ggplot2::geom_line(aes(y = q50tmean, linetype = "q50"), na.rm = TRUE) +
    ggplot2::geom_line(aes(y = q05tmean, linetype = "q05"), na.rm = TRUE) +
    ggplot2::geom_line(aes(y = q95tmean, linetype = "q95"), na.rm = TRUE) +
    ggplot2::scale_linetype_manual(values = c("q50" = "longdash", "q05" = "dotted", "q95" = "dotted"),
                                   labels = c("q50" = paste0("Normal mean temp. (", ref_start_year, " - ", ref_end_year, ")"),
                                              "q05" = expr(paste(italic(P[5]), " (", !!ref_start_year, " - ", !!ref_end_year, ")")),
                                              "q95" = expr(paste(italic(P[95]), " (", !!ref_start_year, " - ", !!ref_end_year, ")"))),
                                   breaks = c("q95", "q50", "q05")) + # To give order
    ggrepel::geom_label_repel(data = rbind(head(plot_data, 3), tail(plot_data |> na.omit(), 3)),
                              aes(y = tmean, label = paste0(ifelse(diffmedian > 0, "+", ""), diffmedian, "ºC")),
                              na.rm = TRUE) +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd("2023-01-01"), ymd("2023-12-31"), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd("2023-01-01")), as.numeric(ymd("2023-12-31")))
    ) +
    ggplot2::scale_y_continuous(limits = c(min(plot_data$tmean, na.rm = TRUE) - 2, 
                                           max(plot_data$tmean, na.rm = TRUE) + 2),
                                breaks = seq(from = round(min(plot_data$tmean, na.rm = TRUE) - 3), 
                                             to = round(max(plot_data$tmean, na.rm = TRUE) + 3), by = 5),
                                labels = function(x) paste0(x, "ºC")) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = "Temperature in Madrid - Retiro",
      subtitle = paste0(
        "Daily mean temperatures anomalies (",
        ref_start_year, " - ", ref_end_year, ")"
      ),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)"
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.15, 0.85),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.title = element_blank()
    )
  
  return(p)
}