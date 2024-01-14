HighTmaxDaysPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate percentiles in reference period
  reference_cumdays_35tmax <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::mutate(tmaxna = ifelse(is.na(tmax), -999, tmax)) |> # cumsum() doesn't have na.omit = TRUE
    dplyr::group_by(year) |> 
    dplyr::mutate(cumsumtmax35 = cumsum(tmaxna > 35)) |> 
    dplyr::group_by(day, month) |>
    dplyr::summarise(
      cumsump00tmax35 = round(quantile(cumsumtmax35, probs = 0.00, na.rm = TRUE), 1),
      cumsump05tmax35 = round(quantile(cumsumtmax35, probs = 0.05, na.rm = TRUE), 1),
      cumsump50tmax35 = round(quantile(cumsumtmax35, probs = 0.50, na.rm = TRUE), 1),
      cumsump95tmax35 = round(quantile(cumsumtmax35, probs = 0.95, na.rm = TRUE), 1),
      cumsump100tmax35 = round(quantile(cumsumtmax35, probs = 1, na.rm = TRUE), 1),
      .groups = "keep"
    ) |>
    dplyr::as_tibble()
  
  # Calculate number of days with tmax above 35ºC in year of study
  selected_year_cumdays_35tmax <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
                    date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::mutate(tmaxna = ifelse(is.na(tmax), -999, tmax)) |> # cumsum() doesn't have na.omit = TRUE
    dplyr::group_by(year) |>
    dplyr::mutate(cumsumtmax35 = cumsum(tmaxna > 35)) |> 
    dplyr::as_tibble()
  
  # Join data
  plot_data <- left_join(reference_cumdays_35tmax, selected_year_cumdays_35tmax, by = c("day", "month")) |>
    dplyr::mutate(date = as.Date(paste0(day, "-", month, selected_year), format = "%d-%m%Y")) |> 
    dplyr::arrange(date) |> 
    dplyr::select(date, cumsumtmax35, everything(), -c(day, month, year, tmin, tmean, tmax, tmaxna))
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = date, y = cumsumtmax35)) +
    ggplot2::annotate("rect", ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.2,
                      xmin = seq(ymd(paste0(selected_year, "-02-01")), by = "2 month", length = 6),
                      xmax = seq(ymd(paste0(selected_year, "-03-01")), by = "2 month", length = 6)) +
    ggplot2::geom_line(aes(linetype = "cumsumtmax35"), linewidth = 0.65, color = "blue",
                       lineend = "round", na.rm = TRUE, show.legend = FALSE) +
    ggplot2::geom_line(aes(y = cumsump50tmax35, linetype = "p50"), color = "darkgreen", lineend = "round", na.rm = TRUE) +
    ggplot2::geom_line(aes(y = cumsump05tmax35, linetype = "p05"), color = "black", lineend = "round", na.rm = TRUE) +
    ggplot2::geom_line(aes(y = cumsump95tmax35, linetype = "p95"), color = "black", lineend = "round", na.rm = TRUE) +
    ggplot2::geom_line(aes(y = cumsump100tmax35, linetype = "p100"), color = "red", lineend = "round", na.rm = TRUE) +
    ggplot2::scale_linetype_manual(
      values = c("p50" = "longdash", "p05" = "dotted", "p95" = "dotted", "cumsumtmax35" = "solid", "p100" = "dotdash"),
      labels = c("p50" = expr(paste(italic(P[50]), " (", !!ref_start_year, "-", !!ref_end_year, ")")),
                 "p05" = expr(paste(italic(P[5]), " (", !!ref_start_year, "-", !!ref_end_year, ")")),
                 "p95" = expr(paste(italic(P[95]), " (", !!ref_start_year, "-", !!ref_end_year, ")")),
                 "p100" = expr(paste(italic(P[100]), " (", !!ref_start_year, "-", !!ref_end_year, ")")),
                 "cumsumtmax35" = paste0("Days with tmax > 35ºC (", selected_year, ")")),
      breaks = c("p100", "p95", "cumsumtmax35", "p50", "p05")) + # To give order 
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd(paste0(selected_year, "-01-01")), 
                              ymd(paste0(selected_year, "-12-31")), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd(paste0(selected_year, "-01-01"))), 
                 as.numeric(ymd(paste0(as.numeric(selected_year) + 1), "-01-01")))
    ) +
    ggplot2::scale_y_continuous(
      limits = 
        c(0, max(max(plot_data$cumsump100tmax35, na.rm = TRUE), max(plot_data$cumsumtmax35, na.rm = TRUE)) + 2),
      breaks = 
        round(seq(from = 0, to = round(max(max(plot_data$cumsump100tmax35, na.rm = TRUE), 
                                           max(plot_data$cumsumtmax35, na.rm = TRUE)) + 3), by = 5) / 5) * 5) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "Days", title = "Temperature in Madrid - Retiro",
      subtitle = paste0("Annual number of days with max temperature above 35ºC (", 
                        ref_start_year, "-", ref_end_year, ")"),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)")
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
    ) +
    ggplot2::guides(linetype = guide_legend(override.aes = list(color = c("red", "black", "blue", "darkgreen", "black"),
                                                                linewidth = c(0.85, 0.85, 0.85, 0.85, 0.85))))
  
  return(list(p, plot_data, "date", "cumsumtmax35"))
  
}