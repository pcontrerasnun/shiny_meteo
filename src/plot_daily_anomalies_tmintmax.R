DailyTminTmaxAnomaliesPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date, title) {
  # Calculate percentiles of tmin across every day of the year
  reference_daily_pcts_tmin <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::as_tibble() |>  # so than reframe works
    # Get previous and next 15 days
    dplyr::reframe(ref_date = seq.Date(date - 15, date + 15, "day"), .by = c(date, tmean)) |>
    dplyr::left_join(data |> rename(climate_tmin = tmin), join_by(ref_date == date)) |> 
    dplyr::select(-ref_date) |> 
    dplyr::mutate(day = format(date, "%d"), month = format(date, "%m")) |> 
    dplyr::group_by(day, month) |> 
    dplyr::summarise(
      p50tmin = round(quantile(climate_tmin, probs = 0.50, na.rm = TRUE), 1),
      .groups = "keep" # to avoid warning
    ) |>
    dplyr::ungroup() |> # Don't know why but necessary cause not plot_data fails
    dplyr::mutate(date = as.Date(paste(selected_year, month, day, sep = "-"), format = "%Y-%m-%d")) |> 
    dplyr::filter(!is.na(date))
  
  # Calculate percentiles of tmin across every day of the year
  reference_daily_pcts_tmax <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::as_tibble() |>  # so than reframe works
    # Get previous and next 15 days
    dplyr::reframe(ref_date = seq.Date(date - 15, date + 15, "day"), .by = c(date, tmean)) |>
    dplyr::left_join(data |> rename(climate_tmax = tmax), join_by(ref_date == date)) |> 
    dplyr::select(-ref_date) |> 
    dplyr::mutate(day = format(date, "%d"), month = format(date, "%m")) |> 
    dplyr::group_by(day, month) |> 
    dplyr::summarise(
      p50tmax = round(quantile(climate_tmax, probs = 0.50, na.rm = TRUE), 1),
      .groups = "keep" # to avoid warning
    ) |>
    dplyr::ungroup() |> # Don't know why but necessary cause not plot_data fails
    dplyr::mutate(date = as.Date(paste(selected_year, month, day, sep = "-"), format = "%Y-%m-%d")) |> 
    dplyr::filter(!is.na(date))
  
  # Get daily min and max temperatures
  selected_year_daily_tmintmax <- data |>     
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
                    date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::select(day, month, tmin, tmax) |> 
    dplyr::as_tibble()
  
  # Join data and calculate anomalies (diff with median)
  plot_data <- dplyr::left_join(reference_daily_pcts_tmin, reference_daily_pcts_tmax, 
                                by = c("day", "month", "date")) |> 
    dplyr::left_join(selected_year_daily_tmintmax, by = c("day", "month")) |> 
    dplyr::mutate(diffmediantmin = round(tmin - p50tmin, 1), 
                  diffmediantmax = round(tmax - p50tmax, 1)) |> 
    dplyr::arrange(date) |> 
    dplyr::select(date, tmin, tmax, p50tmin, p50tmax, diffmediantmin, diffmediantmax)
  
  # For geom_segment coloring purposes
  color_data_tmin <- plot_data |> 
    dplyr::reframe(x = seq(min(plot_data$date, na.rm = TRUE), max(plot_data$date, na.rm = TRUE), length = 1000),
                   y1 = approx(date, tmin, xout = x)$y,
                   y2 = approx(date, p50tmin, xout = x)$y,
                   diff = y1 - y2) |> 
    # Remove dates also not present in plot_data to be consistent
    dplyr::filter(!(as.character(x) %in% as.character(plot_data$date[is.na(plot_data$tmin)])))
  
  color_data_tmax <- plot_data |> 
    dplyr::reframe(x = seq(min(plot_data$date, na.rm = TRUE), max(plot_data$date, na.rm = TRUE), length = 1000),
                   y1 = approx(date, tmax, xout = x)$y,
                   y2 = approx(date, p50tmax, xout = x)$y,
                   diff = y1 - y2) |> 
    # Remove dates also not present in plot_data to be consistent
    dplyr::filter(!(as.character(x) %in% as.character(plot_data$date[is.na(plot_data$tmax)])))
  
  p <- ggplot2::ggplot(data = plot_data, aes(x = date)) +
    ggplot2::annotate("rect", ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.2,
                      xmin = seq(ymd(paste0(selected_year, "-02-01")), by = "2 month", length = 6),
                      xmax = seq(ymd(paste0(selected_year, "-03-01")), by = "2 month", length = 6)) +
    ggplot2::geom_segment(data = color_data_tmin, aes(x = x, y = y1, xend = x, yend = y2, color = diff),
                          linewidth = 1, na.rm = TRUE) +
    ggplot2::geom_segment(data = color_data_tmax, aes(x = x, y = y1, xend = x, yend = y2, color = diff),
                          linewidth = 1, na.rm = TRUE) +
    ggplot2::scale_color_gradient2(high = "#ca0020", mid = "white", low = "#0571b0", guide = guide_none()) +
    ggplot2::geom_line(aes(y = tmin, linetype = "tmin"), linewidth = 0.65, lineend = "round", 
                       na.rm = TRUE, show.legend = FALSE, color = "black") +
    ggplot2::geom_line(aes(y = tmax, linetype = "tmax"), linewidth = 0.65, lineend = "round", 
                       na.rm = TRUE, show.legend = FALSE, color = "black") +
    ggplot2::geom_line(aes(y = p50tmin, linetype = "p50tmin"), lineend = "round", na.rm = TRUE) +
    ggplot2::geom_line(aes(y = p50tmax, linetype = "p50tmax"), lineend = "round", na.rm = TRUE) +
    ggplot2::scale_linetype_manual(
      values = c("p50tmin" = "longdash", "p50tmax" = "longdash", "tmin" = "solid", "tmax" = "solid"),
      labels = c("p50tmin" = expr(paste("Normal (", italic(P[50]), ") min temp. (", !!ref_start_year, "-", !!ref_end_year, ")")),
                 "p50tmax" = expr(paste("Normal (", italic(P[50]), ") max temp. (", !!ref_start_year, "-", !!ref_end_year, ")")),
                 "tmin" = paste0("Daily min temp. (", selected_year, ")"),
                 "tmax" = paste0("Daily max temp. (", selected_year, ")")),
      breaks = c("tmax", "p50tmax", "tmin", "p50tmin")) + # To give order
    ggrepel::geom_label_repel(
      data = if ((sum(!is.na(plot_data$tmin)) <= 30)) { # if less than 30 days of data
        rbind(head(plot_data |> arrange(diffmediantmin), 1), tail(plot_data |> arrange(diffmediantmin) 
                                                                  |> na.omit(), 1)) } # only show 1 label
      else { rbind(head(plot_data |> arrange(diffmediantmin), 3), tail(plot_data |> arrange(diffmediantmin) |> # else show 3 labels
                                                                         na.omit(), 3)) },
      aes(y = tmin, label = paste0(ifelse(diffmediantmin > 0, "+", ""), diffmediantmin, "ºC")),
      fill = '#d1e5f0', na.rm = TRUE) + 
    ggrepel::geom_label_repel(
      data = if ((sum(!is.na(plot_data$tmax)) <= 30)) { # if less than 30 days of data
        rbind(head(plot_data |> arrange(diffmediantmax), 1), tail(plot_data |> arrange(diffmediantmax) 
                                                                  |> na.omit(), 1)) } # only show 1 label
      else { rbind(head(plot_data |> arrange(diffmediantmax), 3), tail(plot_data |> arrange(diffmediantmax) |> # else show 3 labels
                                                                         na.omit(), 3)) },
      aes(y = tmax, label = paste0(ifelse(diffmediantmax > 0, "+", ""), diffmediantmax, "ºC")),
      fill = "#fddbc7", na.rm = TRUE) +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd(paste0(selected_year, "-01-01")), 
                              ymd(paste0(selected_year, "-12-31")), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd(paste0(selected_year, "-01-01"))), 
                 as.numeric(ymd(paste0(as.numeric(selected_year) + 1), "-01-01")))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(min(min(plot_data$p50tmin, na.rm = TRUE), min(plot_data$tmin, na.rm = TRUE)) - 2, 
                 max(max(plot_data$p50tmax, na.rm = TRUE), max(plot_data$tmax, na.rm = TRUE)) + 2),
      breaks = 
        round(seq(from = round(min(min(plot_data$p50tmin, na.rm = TRUE), min(plot_data$tmin, na.rm = TRUE)) - 3), 
                  to = round(max(max(plot_data$p50tmax, na.rm = TRUE), max(plot_data$tmax, na.rm = TRUE)) + 3), by = 5) / 5) * 5,
      labels = function(x) paste0(x, "ºC")) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Temperature in ", title, " ", selected_year),
      subtitle = paste0("Daily min and max temperature anomalies vs. historical medians (", 
                        ref_start_year, "-", ref_end_year, ")"),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/")
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.1335, 0.85),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.title = element_blank()
    )
  
  return(list(p, plot_data, "date", "tmin"))
  
}
