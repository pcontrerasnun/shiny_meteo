DailyTminPlot <- function(data, data_forecast, selected_year, ref_start_year, ref_end_year, max_date, title) {
  # Calculate percentiles of tmin across every day of the year
  reference_daily_pcts_tmin <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::filter(!(month(date) == 2 & day(date) == 29)) |> # Remove 29 Feb data
    dplyr::as_tibble() |> 
    dplyr::reframe(ref_date = seq.Date(date - 15, date + 15, "day"), .by = c(date, tmin)) |>
    dplyr::left_join(data |> filter(!(month(date) == 2 & day(date) == 29)) |> # Remove 29 Feb data
                       rename(climate_tmin = tmin), join_by(ref_date == date)) |> 
    dplyr::select(-ref_date) |> 
    dplyr::mutate(day = format(date, "%d"), month = format(date, "%m")) |> 
    dplyr::group_by(day, month) |> 
    dplyr::summarise(
      p00tmin = round(quantile(climate_tmin, probs = 0.00, na.rm = TRUE), 1), # not plotting but interesting to see in data
      p01tmin = round(quantile(climate_tmin, probs = 0.01, na.rm = TRUE), 1),
      p05tmin = round(quantile(climate_tmin, probs = 0.05, na.rm = TRUE), 1),
      p20tmin = round(quantile(climate_tmin, probs = 0.20, na.rm = TRUE), 1),
      p40tmin = round(quantile(climate_tmin, probs = 0.40, na.rm = TRUE), 1),
      p60tmin = round(quantile(climate_tmin, probs = 0.60, na.rm = TRUE), 1),
      p80tmin = round(quantile(climate_tmin, probs = 0.80, na.rm = TRUE), 1),
      p95tmin = round(quantile(climate_tmin, probs = 0.95, na.rm = TRUE), 1),
      p99tmin = round(quantile(climate_tmin, probs = 0.99, na.rm = TRUE), 1),
      p100tmin = round(quantile(climate_tmin, probs = 1, na.rm = TRUE), 1), # not plotting but interesting to see in data
      .groups = "keep" # to avoid warning
    ) |>
    dplyr::ungroup() |> 
    dplyr::mutate(date = as.Date(paste(selected_year, month, day, sep = "-"), format = "%Y-%m-%d")) |> 
    dplyr::filter(!is.na(date))
  
  # Get daily min temperatures
  selected_year_daily_tmin <- data |>     
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
                    date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::select(date, day, month, tmin) |> 
    dplyr::as_tibble()
  
  # Join data
  plot_data <- dplyr::full_join(selected_year_daily_tmin, reference_daily_pcts_tmin, by = c("date", "day", "month")) |> 
    dplyr::select(date, tmin, everything(), -c(day, month)) |> 
    dplyr::mutate(
      p00tmin = zoo::na.approx(p00tmin), # This is to fill NA value of 29 Feb with mean of next and previous value (28 Feb and 1st March)
      p01tmin = zoo::na.approx(p01tmin),
      p05tmin = zoo::na.approx(p05tmin),
      p20tmin = zoo::na.approx(p20tmin),
      p40tmin = zoo::na.approx(p40tmin),
      p60tmin = zoo::na.approx(p60tmin),
      p80tmin = zoo::na.approx(p80tmin),
      p95tmin = zoo::na.approx(p95tmin),
      p99tmin = zoo::na.approx(p99tmin),
      p100tmin = zoo::na.approx(p100tmin)
    )
  
  # Ranking max tmin and min tmin
  ranking_tmin <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
                     date <= as.Date(paste0(ref_end_year, "-12-31"))) |
                    (date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
                       date <= as.Date(paste0(as.numeric(selected_year), "-12-31")))) |> # Include year of study
    dplyr::group_by(date) |> 
    dplyr::summarise(mintmin = min(tmin, na.rm = TRUE)) |> 
    dplyr::mutate(year = format(date, "%Y")) |> 
    dplyr::mutate(date = format(date, "%d-%m-%Y")) |> 
    dplyr::arrange(-mintmin) |> 
    dplyr::mutate(ranktmin = rank(-mintmin, ties.method = "first")) |>
    dplyr::as_tibble()
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = date)) +
    ggplot2::annotate("rect", ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.2,
                      xmin = seq(ymd(paste0(selected_year, "-02-01")), by = "2 month", length = 6),
                      xmax = seq(ymd(paste0(selected_year, "-03-01")), by = "2 month", length = 6)) +
    ggplot2::geom_ribbon(aes(ymin = p95tmin, ymax = p99tmin, fill = "P100"), # p95tmin + 4
                         alpha = 0.3, color = "#b2182b", linetype = "51", 
                         lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = p80tmin, ymax = p95tmin, fill = "P95"),
                         alpha = 0.3, color = "#ef8a62", linetype = "51", 
                         lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = p60tmin, ymax = p80tmin, fill = "P80"),
                         alpha = 0.3, color = "#fddbc7", linetype = "51", 
                         lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = p40tmin, ymax = p60tmin, fill = "P60"),
                         alpha = 0.3, color = NA, linetype = "51", 
                         lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = p20tmin, ymax = p40tmin, fill = "P40"),
                         alpha = 0.3, color = "#d1e5f0", linetype = "51", 
                         lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = p05tmin, ymax = p20tmin, fill = "P20"),
                         alpha = 0.3, color = "#67a9cf", linetype = "51", 
                         lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = p01tmin, ymax = p05tmin, fill = "P00"), # p05tmin - 4
                         alpha = 0.3, color = "#2166ac", linetype = "51", 
                         lineend = "round", linejoin = "round") +
    ggplot2::geom_line(aes(y = tmin, color = "tmin"), linewidth = 0.75, lineend = "round", na.rm = TRUE) +
    ggplot2::annotation_custom(
      gridtext::richtext_grob(
        x = unit(.855, "npc"),
        y = unit(.925, "npc"),
        text = paste0("**Ranking** (", ref_start_year, "-", ref_end_year, ")<br>", 
                      "Min tmin. <br><br>", 
                      head(ranking_tmin |> dplyr::arrange(ranktmin), 1)$ranktmin, "º ", 
                      head(ranking_tmin |> dplyr::arrange(ranktmin), 1)$date, ": ", 
                      head(ranking_tmin |> dplyr::arrange(ranktmin), 1)$mintmin, "ºC<br>",
                      head(ranking_tmin |> dplyr::arrange(ranktmin), 2)[2,]$ranktmin, "º ", 
                      head(ranking_tmin |> dplyr::arrange(ranktmin), 2)[2,]$date, ": ", 
                      head(ranking_tmin |> dplyr::arrange(ranktmin), 2)[2,]$mintmin, "ºC<br>",
                      head(ranking_tmin |> dplyr::arrange(ranktmin), 3)[3,]$ranktmin, "º ",
                      head(ranking_tmin |> dplyr::arrange(ranktmin), 3)[3,]$date, ": ", 
                      head(ranking_tmin |> dplyr::arrange(ranktmin), 3)[3,]$mintmin, "ºC<br>",
                      "---------------------------<br>",
                      head(ranking_tmin |> dplyr::filter(year == selected_year) |> dplyr::arrange(ranktmin), 1)$date, ": ",
                      head(ranking_tmin |> dplyr::filter(year == selected_year) |> dplyr::arrange(ranktmin), 1)$mintmin, "ºC"),
        hjust = 0, vjust = 1,
        r = unit(0.15, "lines"),
        box_gp = grid::gpar(col = "black", lwd = 2),
        padding = unit(0.5, "lines")
      )
    ) +
    ggplot2::scale_color_manual(
      values = c("tmin" = "black", "fcsttmin" = "black"),
      label = c("tmin" = paste0("Daily min temp. (", selected_year, ")"),
                "fcsttmin" = paste0("Forecast min temp. +7 days (", selected_year, ")")),
      guide = guide_legend(order = 1)) +
    ggplot2::scale_fill_manual(
      values = c("P100" = "#b2182b", "P95" = "#ef8a62", "P80" = "#fddbc7", "P60" = "#f7f7f7",
                 "P40" = "#d1e5f0", "P20" = "#67a9cf", "P00" = "#2166ac"),
      breaks = c("P100", "P95", "P80", "P60", "P40", "P20", "P00"), # To give order
      labels = c("P100" = expr(paste("Extrem. hot day (", italic(P[95]), "-", italic(P[99]), ") (", 
                                     !!ref_start_year, "-", !!ref_end_year, ")")), 
                 "P95" = expr(paste("Very hot day (", italic(P[80]), "-", italic(P[95]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")), 
                 "P80" = expr(paste("Hot day (", italic(P[60]), "-", italic(P[80]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")), 
                 "P60" = expr(paste("Normal day (", italic(P[40]), "-", italic(P[60]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")),
                 "P40" = expr(paste("Cold day (", italic(P[20]), "-", italic(P[40]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")),
                 "P20" = expr(paste("Very cold day (", italic(P[05]), "-", italic(P[20]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")),
                 "P00" = expr(paste("Extrem. cold day (", italic(P[01]), "-", italic(P[05]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")"))),
      guide = guide_legend(override.aes = list(colour = NA), order = 2)) +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd(paste0(selected_year, "-01-01")), ymd(paste0(selected_year, "-12-31")), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd(paste0(selected_year, "-01-01"))), 
                 as.numeric(ymd(paste0(as.numeric(selected_year) + 1), "-01-01")))
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "ºC"),
      breaks = round(seq(from = min(min(plot_data$p05tmin) - 4, min(plot_data$tmin, na.rm = TRUE)) - 2, 
                         to = max(max(plot_data$p95tmin) + 4, max(plot_data$tmin, na.rm = TRUE)) + 2, by = 5) / 5) * 5,
      limits = c(min(min(plot_data$p05tmin) - 4, min(plot_data$tmin, na.rm = TRUE)) - 2, 
                 max(max(plot_data$p95tmin) + 4, max(plot_data$tmin, na.rm = TRUE)) + 2)
    ) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Temperature in ", title, " ", selected_year),
      subtitle = paste0(
        "Daily min temperature vs. historical percentiles (", ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/")
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25), 
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.1345, 0.8),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.title = ggplot2::element_blank(),
      legend.text.align = 0
    ) +
    ggplot2::guides(fill = guide_legend(override.aes = list(alpha = 0.7 / 7, color = NA)),
                    color = guide_legend(override.aes = list(linetype = c("solid"),
                                                             linewidth = c(0.9))))
  
  # If year of study is current year then plot forecast data
  if (selected_year == year(Sys.Date())) {
    p <- p + ggplot2::geom_line(data = data_forecast, aes(y = tmin, color = "fcsttmin"), linewidth = 0.75, 
                                linetype = "dotted", lineend = "round", na.rm = TRUE) +
      ggplot2::guides(fill = guide_legend(override.aes = list(alpha = 0.7 / 7, color = NA)),
                      color = guide_legend(override.aes = list(linetype = c("dotted", "solid"),
                                                               linewidth = c(0.5, 0.9))))
  }
  
  return(list(p, plot_data, "date", "tmin"))
  
}
