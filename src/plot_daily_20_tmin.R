TropicalNightsPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date, title) {
  # Calculate percentiles in reference period
  reference_cumdays_20tmin <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::mutate(tminna = ifelse(is.na(tmin), -999, tmin)) |> # cumsum() doesn't have na.omit = TRUE
    dplyr::group_by(year) |> 
    dplyr::mutate(cumsumtmin20 = cumsum(tminna > 20)) |> 
    dplyr::group_by(day, month) |>
    dplyr::summarise(
      cumsump00tmin20 = round(quantile(cumsumtmin20, probs = 0.00, na.rm = TRUE), 1),
      cumsump05tmin20 = round(quantile(cumsumtmin20, probs = 0.05, na.rm = TRUE), 1),
      cumsump50tmin20 = round(quantile(cumsumtmin20, probs = 0.50, na.rm = TRUE), 1),
      cumsump95tmin20 = round(quantile(cumsumtmin20, probs = 0.95, na.rm = TRUE), 1),
      cumsump100tmin20 = round(quantile(cumsumtmin20, probs = 1, na.rm = TRUE), 1),
      .groups = "keep"
    ) |>
    dplyr::as_tibble()
  
  # Calculate number of days with tmin above 20ºC in year of study
  selected_year_cumdays_20tmin <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
                    date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::mutate(tminna = ifelse(is.na(tmin), -999, tmin)) |> # cumsum() doesn't have na.omit = TRUE
    dplyr::group_by(year) |>
    dplyr::mutate(cumsumtmin20 = cumsum(tminna > 20)) |> 
    dplyr::as_tibble()
  
  # Join data
  plot_data <- left_join(reference_cumdays_20tmin, selected_year_cumdays_20tmin, by = c("day", "month")) |>
    dplyr::mutate(date = as.Date(paste0(day, "-", month, selected_year), format = "%d-%m%Y")) |> 
    dplyr::arrange(date) |> 
    dplyr::select(date, cumsumtmin20, everything(), -c(day, month, year, tmax, tmean, tmin, tminna))
  
  # Ranking
  reference_yearly_rank_20tmin <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
                     date <= as.Date(paste0(ref_end_year, "-12-31"))) |
                    (date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
                       date <= as.Date(paste0(as.numeric(selected_year), "-12-31")))) |> # Include year of study
    dplyr::mutate(tminna = ifelse(is.na(tmin), -999, tmin)) |> # cumsum() doesn't have na.omit = TRUE
    dplyr::group_by(year) |> 
    dplyr::summarise(sumtmin20 = sum(tminna > 20)) |> 
    dplyr::arrange(-sumtmin20) |> 
    dplyr::mutate(ranking = rank(-sumtmin20, ties.method = "first")) |>
    dplyr::as_tibble()
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = date, y = cumsumtmin20)) +
    ggplot2::annotate("rect", ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.2,
                      xmin = seq(ymd(paste0(selected_year, "-02-01")), by = "2 month", length = 6),
                      xmax = seq(ymd(paste0(selected_year, "-03-01")), by = "2 month", length = 6)) +
    ggplot2::geom_line(aes(linetype = "cumsumtmin20"), linewidth = 0.65, color = "blue",
                       lineend = "round", na.rm = TRUE, show.legend = FALSE) +
    ggplot2::geom_line(aes(y = cumsump50tmin20, linetype = "p50"), color = "darkgreen", lineend = "round", na.rm = TRUE) +
    ggplot2::geom_line(aes(y = cumsump05tmin20, linetype = "p05"), color = "black", lineend = "round", na.rm = TRUE) +
    ggplot2::geom_line(aes(y = cumsump95tmin20, linetype = "p95"), color = "black", lineend = "round", na.rm = TRUE) +
    ggplot2::geom_line(aes(y = cumsump100tmin20, linetype = "p100"), color = "red", lineend = "round", na.rm = TRUE) +
    ggplot2::annotation_custom(
      gridtext::richtext_grob(
        x = unit(.0413, "npc"),
        y = unit(.75, "npc"),
        text = paste0("**Ranking** (", ref_start_year, "-", ref_end_year, ")<br>", 
                      "Most days with tmin > 20ºC <br><br>", 
                      head(reference_yearly_rank_20tmin, 1)$ranking, "º ", 
                      head(reference_yearly_rank_20tmin, 1)$year, ": ", 
                      head(reference_yearly_rank_20tmin, 1)$sumtmin20, " days<br>",
                      head(reference_yearly_rank_20tmin, 2)[2,]$ranking, "º ", 
                      head(reference_yearly_rank_20tmin, 2)[2,]$year, ": ", 
                      head(reference_yearly_rank_20tmin, 2)[2,]$sumtmin20, " days<br>",
                      head(reference_yearly_rank_20tmin, 3)[3,]$ranking, "º ",
                      head(reference_yearly_rank_20tmin, 3)[3,]$year, ": ", 
                      head(reference_yearly_rank_20tmin, 3)[3,]$sumtmin20, " days<br>",
                      "---------------------------<br>",
                      subset(reference_yearly_rank_20tmin, 
                             reference_yearly_rank_20tmin$year == selected_year)$ranking, "º ",
                      selected_year, ": ",
                      subset(reference_yearly_rank_20tmin, 
                             reference_yearly_rank_20tmin$year == selected_year)$sumtmin20,
                      " days"),
        hjust = 0, vjust = 1,
        r = unit(0.15, "lines"),
        box_gp = grid::gpar(col = "black", lwd = 2),
        padding = unit(0.5, "lines")
      )
    ) +
    ggplot2::scale_linetype_manual(
      values = c("p50" = "longdash", "p05" = "dotted", "p95" = "dotted", "cumsumtmin20" = "solid", "p100" = "dotdash"),
      labels = c("p50" = expr(paste(italic(P[50]), " (", !!ref_start_year, "-", !!ref_end_year, ")")),
                 "p05" = expr(paste(italic(P[5]), " (", !!ref_start_year, "-", !!ref_end_year, ")")),
                 "p95" = expr(paste(italic(P[95]), " (", !!ref_start_year, "-", !!ref_end_year, ")")),
                 "p100" = expr(paste(italic(P[100]), " (", !!ref_start_year, "-", !!ref_end_year, ")")),
                 "cumsumtmin20" = paste0("Days with tmin > 20ºC (", selected_year, ")")),
      breaks = c("p100", "p95", "cumsumtmin20", "p50", "p05")) + # To give order 
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd(paste0(selected_year, "-01-01")), 
                              ymd(paste0(selected_year, "-12-31")), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd(paste0(selected_year, "-01-01"))), 
                 as.numeric(ymd(paste0(as.numeric(selected_year) + 1), "-01-01")))
    ) +
    ggplot2::scale_y_continuous(
      limits = 
        c(0, max(max(plot_data$cumsump100tmin20, na.rm = TRUE), max(plot_data$cumsumtmin20, na.rm = TRUE)) + 2),
      breaks = 
        round(seq(from = 0, to = round(max(max(plot_data$cumsump100tmin20, na.rm = TRUE), 
                                           max(plot_data$cumsumtmin20, na.rm = TRUE)) + 3), by = 5) / 5) * 5) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "Days", title = paste0("Temperature in ", title, " ", selected_year),
      subtitle = paste0("Annual number of days with min temperature above 20ºC (", 
                        ref_start_year, "-", ref_end_year, ")"),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/")
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.11, 0.85),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.title = element_blank()
    ) +
    ggplot2::guides(linetype = guide_legend(
      override.aes = list(color = c("red", "black", "blue", "darkgreen", "black"),
                          linewidth = c(0.5, 0.5, 0.5, 0.5, 0.5))))
  
  return(list(p, plot_data, "date", "cumsumtmin20"))
  
}
