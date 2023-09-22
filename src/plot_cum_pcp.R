CumPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate historical mean for reference period
  reference_mean_pcp <- data |>
    dplyr::filter(fecha >= as.Date(paste0(ref_start_year, "-01-01")) &
      fecha <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::group_by(dia, mes) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE), n = n()) |>
    dplyr::ungroup() |>
    dplyr::arrange(mes, dia) |>
    dplyr::mutate(
      meanpcp = sumpcp / n,
      cummeanpcp = round(cumsum(ifelse(is.na(meanpcp), 0, meanpcp)), 1)
    )

  # Calculate cumulative sum precipitation for selected year
  selected_year_pcp <- data |>
    dplyr::filter(fecha >= as.Date(paste0(selected_year, "-01-01")) &
      fecha <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::mutate(cumsumpcp = cumsum(tidyr::replace_na(pcp, 0)))

  # Join previous two datasets and create new columns 'diffmean' and 'fecha'
  plot_data <- left_join(reference_mean_pcp, selected_year_pcp, by = c("dia", "mes")) |>
    dplyr::select(
      dia, mes, cummeanpcp, cumsumpcp
    ) |>
    mutate(diffmean = cumsumpcp - cummeanpcp) |>
    dplyr::mutate(fecha = as.Date(paste0(dia, "-", mes, "2023"), format = "%d-%m%Y")) |> # We choose
    # 2023 since it doesn't have 29th Feb, it doesn't matter what year we choose but it can't be
    # a leap year
    dplyr::as_tibble()

  p <- ggplot2::ggplot(data = plot_data, aes(x = fecha, y = cumsumpcp)) +
    ggplot2::geom_segment(aes(xend = fecha, yend = cummeanpcp, color = diffmean), linewidth = 1.2) +
    ggplot2::scale_color_gradient2(high = "#2c7bb6", mid = "white", low = "#d7191c") +
    ggplot2::geom_line(linewidth = 0.85, lineend = "round") +
    ggplot2::geom_line(aes(y = cummeanpcp)) +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd("2023-01-01"), ymd("2023-12-31"), by = "month")),
      labels = format(seq(ymd("2023-01-01"), ymd("2023-12-31"), by = "month"), "%b"),
      limits = c(as.numeric(ymd("2023-01-01")), as.numeric(ymd("2023-12-31"))),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "mm"),
      breaks = seq(from = 0, to = max(plot_data$cummeanpcp) + 400, by = 100),
      expand = c(0, 20, 0, 100)
    ) +
    ggthemes::theme_hc(base_size = 15) +
    labs(
      x = "", y = "", title = paste0("Precipitación en Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Precipitación acumulada comparada con media histórica (",
        ref_start_year, "-", ref_end_year, ")"
      ),
      caption = paste0(
        "Actualizado: ", max_date, ", Fuente: AEMET, Elab. propia (@Pcontreras95)"
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25), legend.position = "none"
    )

  return(p)
}
