DailyTminTmaxPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate percentiles of tmax across every day of the year
  reference_daily_pcts_tmax <- data_temp |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::as_tibble() |> 
    # Get previous and next 15 days
    dplyr::reframe(ref_date = seq.Date(date - 15, date + 15, "day"), .by = c(date, tmax)) |>
    dplyr::left_join(data_temp |> rename(climate_tmax = tmax), join_by(ref_date == date)) |> 
    dplyr::select(-ref_date) |> 
    dplyr::mutate(day = format(date, "%d"), month = format(date, "%m")) |> 
    dplyr::group_by(day, month) |> 
    dplyr::summarise(
      p05tmax = round(quantile(climate_tmax, probs = 0.05, na.rm = TRUE), 1),
      p50tmax = round(quantile(climate_tmax, probs = 0.50, na.rm = TRUE), 1),
      p95tmax = round(quantile(climate_tmax, probs = 0.95, na.rm = TRUE), 1),
      .groups = "keep" # to avoid warning
    ) |>
    dplyr::ungroup() |> 
    dplyr::mutate(date = as.Date(paste(selected_year, month, day, sep = "-"), format = "%Y-%m-%d")) |> 
    dplyr::filter(!is.na(date))
  
  # Calculate percentiles of tmin across every day of the year
  reference_daily_pcts_tmin <- data_temp |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::as_tibble() |> 
    # Get previous and next 15 days
    dplyr::reframe(ref_date = seq.Date(date - 15, date + 15, "day"), .by = c(date, tmin)) |>
    dplyr::left_join(data_temp |> rename(climate_tmin = tmin), join_by(ref_date == date)) |> 
    dplyr::select(-ref_date) |> 
    dplyr::mutate(day = format(date, "%d"), month = format(date, "%m")) |> 
    dplyr::group_by(day, month) |> 
    dplyr::summarise(
      p05tmin = round(quantile(climate_tmin, probs = 0.05, na.rm = TRUE), 1),
      p50tmin = round(quantile(climate_tmin, probs = 0.50, na.rm = TRUE), 1),
      p95tmin = round(quantile(climate_tmin, probs = 0.95, na.rm = TRUE), 1),
      .groups = "keep" # to avoid wartmin
    ) |>
    dplyr::ungroup() |> 
    dplyr::mutate(date = as.Date(paste(selected_year, month, day, sep = "-"), format = "%Y-%m-%d")) |> 
    dplyr::filter(!is.na(date))
  
  # Get daily min and max temperatures
  selected_year_daily_tminmax <- data_temp |>     
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
                    date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::select(day, month, tmin, tmax) |> 
    dplyr::as_tibble()
  
  # Join data
  plot_data <- dplyr::left_join(reference_daily_pcts_tmin, reference_daily_pcts_tmax,
                                by = c("day", "month", "date")) |> 
    dplyr::left_join(selected_year_daily_tminmax, by = c("day", "month")) |> 
    dplyr::select(date, tmin, tmax, everything(), -c(day, month))
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = date)) +
    ggplot2::annotate("rect", ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.2,
             xmin = seq(ymd(paste0(selected_year, "-02-01")), by = "2 month", length = 6),
             xmax = seq(ymd(paste0(selected_year, "-03-01")), by = "2 month", length = 6)) +
    ggplot2::geom_ribbon(aes(ymin = p05tmax, ymax = p95tmax, fill = "tmax"),
                         alpha = 0.3, color = "#ca0020", linetype = "51", 
                         lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = p05tmin, ymax = p95tmin, fill = "tmin"),
                         alpha = 0.3, color = "#0571b0", linetype = "51", 
                         lineend = "round", linejoin = "round") +
    ggplot2::geom_line(aes(y = tmin, color = "tmin"), linewidth = 0.75, lineend = "round", na.rm = TRUE) +
    ggplot2::geom_line(aes(y = p50tmin, color = "p50tmin"), linewidth = 0.75, linetype = "dotted", lineend = "round", na.rm = TRUE) +
    ggplot2::geom_line(aes(y = tmax, color = "tmax"), linewidth = 0.75, lineend = "round", na.rm = TRUE) +
    ggplot2::geom_line(aes(y = p50tmax, color = "p50tmax"), linewidth = 0.75, linetype = "dotted", lineend = "round", na.rm = TRUE) +
    ggplot2::scale_color_manual(
      values = c("tmax" = "#ca0020", "tmin" = "#0571b0", "p50tmax" = "#ca0020", "p50tmin" = "#0571b0"),
      label = c("tmax" = paste0("Daily max temp. (", selected_year, ")"),
                "tmin" = paste0("Daily min temp. (", selected_year, ")"),
                "p50tmax" = expr(paste("Daily ", italic(P[50]), " max temp. (", !!ref_start_year, "-", !!ref_end_year, ")")),
                "p50tmin" = expr(paste("Daily ", italic(P[50]), " min temp. (", !!ref_start_year, "-", !!ref_end_year, ")"))),
      guide = guide_legend(order = 1)) +
    ggplot2::scale_fill_manual(
      values = c("tmax" = "#ca0020", "tmin" = "#0571b0"),
      breaks = c("tmax", "tmin"), # To give order
      labels = c("tmax" = expr(paste("Daily ", italic(P[05]), " - ", italic(P[95])," max temp. (", !!ref_start_year, "-", !!ref_end_year, ")")),
                 "tmin" = expr(paste("Daily ", italic(P[05]), " - ", italic(P[95])," min temp. (", !!ref_start_year, "-", !!ref_end_year, ")"))),
      guide = guide_legend(override.aes = list(colour = NA), order = 2)) +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd(paste0(selected_year, "-01-01")), 
                              ymd(paste0(selected_year, "-12-31")), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd(paste0(selected_year, "-01-01"))),
                 as.numeric(ymd(paste0(as.numeric(selected_year) + 1, "-01-01"))))
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "ºC"),
      breaks = round(seq(from = min(min(plot_data$p05tmin) - 4, min(plot_data$tmin, na.rm = TRUE)) - 2, 
                         to = max(max(plot_data$p95tmax) + 4, max(plot_data$tmax, na.rm = TRUE)) + 2, by = 5) / 5) * 5,
      limits = c(min(min(plot_data$p05tmin) - 4, min(plot_data$tmin, na.rm = TRUE)) - 2, 
                 max(max(plot_data$p95tmax) + 4, max(plot_data$tmax, na.rm = TRUE)) + 2)) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Temperature in Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Daily min and max temperatures vs. historical percentiles (", ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)")
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25), 
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.13, 0.8),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.title = ggplot2::element_blank(),
      legend.text.align = 0
    ) +
    ggplot2::guides(color = guide_legend(override.aes = list(linetype = c("dotted", "dotted", "solid", "solid"))))
  
  return(list(p, plot_data, "date", "tmax"))
  
}