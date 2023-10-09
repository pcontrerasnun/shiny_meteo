#' Plot cumulated precipiation in each month along with min, max and median historical
#' precipitation for each month and ranking of selected year
#'
#' Plots precipitation in each month in selected year along with historical monthly max, min and 
#' mean precipitation (historical period starts at ref_start_year' and ends at 'ref_end_year')
#'
#' @param data An R dataset with AEMET Open data
#' @param selected_year Year of study
#' @param ref_start_year Start year of reference period
#' @param ref_end_year End year of reference period
#' @param max_date Max date of data
#' @returns A ggplot2 plot
#' @examples
#' MonthlyRankingPcpPlot(data, 2023, 1981, 2010, "2023-09-24")
MonthlyRankingPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate each month's rank
  reference_monthly_pcp <- data |>
    dplyr::filter((fecha >= as.Date(paste0(ref_start_year, "-01-01")) &
      fecha <= as.Date(paste0(ref_end_year, "-12-31"))) |
      (fecha >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
        fecha <= as.Date(paste0(as.numeric(selected_year), "-12-31")))) |>
    dplyr::group_by(ano, mes) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::group_by(mes) |>
    dplyr::mutate(rango = rank(-sumpcp, ties.method = "first")) |>
    dplyr::arrange(mes, rango) |>
    dplyr::as_tibble()

  # Calculate min, mean and max historical precipitation in each month
  reference_stats_monthly_pcp <- data |>
    dplyr::filter(fecha >= as.Date(paste0(ref_start_year, "-01-01")) &
      fecha <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::group_by(ano, mes) |>
    dplyr::mutate(sumpcp = sum(pcp)) |>
    dplyr::arrange(ano, mes) |>
    dplyr::group_by(mes) |>
    dplyr::summarise(
      minpcp = round(min(sumpcp, na.rm = TRUE), 1),
      maxpcp = round(max(sumpcp, na.rm = TRUE), 1),
      meanpcp = round(mean(sumpcp, na.rm = TRUE), 1)
    ) |>
    dplyr::arrange(mes) |>
    dplyr::as_tibble()

  # Calculated cumulated precipitacion in each month of selected year
  selected_year_monthly_pcp <- data |>
    dplyr::filter(fecha >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
      fecha <= as.Date(paste0(as.numeric(selected_year), "-12-31"))) |>
    dplyr::group_by(mes, ano) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE)) |>
    dplyr::arrange(ano, mes) |>
    dplyr::ungroup() |>
    dplyr::as_tibble()

  # Draw the plot
  p <- ggplot2::ggplot(data = selected_year_monthly_pcp, aes(x = mes)) +
    ggh4x::geom_box(data = reference_stats_monthly_pcp, aes(
      ymin = minpcp, ymax = maxpcp,
      width = 0.9
    ), fill = "white", color = "black") +
    ggplot2::geom_col(aes(y = sumpcp, fill = "Precip. mensual acumulada")) +
    ggplot2::geom_errorbar(
      data = reference_stats_monthly_pcp,
      aes(
        y = meanpcp, ymin = meanpcp, ymax = meanpcp,
        color = "Precip. media mensual histórica"
      ), linetype = "dashed"
    ) +
    ggplot2::geom_errorbar(
      data = reference_stats_monthly_pcp,
      aes(
        y = maxpcp, ymin = maxpcp, ymax = maxpcp,
        color = "Precip. máxima mensual histórica"
      ), linetype = "solid", linewidth = 1
    ) +
    ggplot2::geom_errorbar(
      data = reference_stats_monthly_pcp,
      aes(
        y = minpcp, ymin = minpcp, ymax = minpcp,
        color = "Precip. mínima mensual histórica"
      ), linetype = "solid", linewidth = 1
    ) +
    ggplot2::scale_x_discrete(
      limits = c(
        "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"
      ),
      labels = c(
        "ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"
      )
    ) +
    ggplot2::scale_color_manual(
      breaks = c("Precip. media mensual histórica", "Precip. máxima mensual histórica", 
                 "Precip. mínima mensual histórica"),
      values = c(
        "Precip. media mensual histórica" = "black",
        "Precip. máxima mensual histórica" = "#4daf4a",
        "Precip. mínima mensual histórica" = "#d7191c"
      )
    ) +
    ggplot2::scale_fill_manual(
      values = c("Precip. mensual acumulada" = "#2c7bb6"),
      labels = c(
        "Precip. mensual acumulada" =
          glue::glue("<span style = 'color: #2c7bb6; '>Precip. mensual acumulada</span>")
      )
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "mm"),
      breaks = seq(
        from = 0, 
        to = max(max(selected_year_monthly_pcp$sumpcp, na.rm = TRUE), 
                 reference_stats_monthly_pcp$maxpcp)
        + 50, by = 25
      ),
      limits = c(0, max(max(selected_year_monthly_pcp$sumpcp, na.rm = TRUE), 
                        reference_stats_monthly_pcp$maxpcp) + 50)
    ) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitación en Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Precipitación mensual acumulada comparada con valores históricos (",
        ref_start_year, "-", ref_end_year, ")"
      ),
      caption = paste0(
        "Actualizado: ", max_date, ", Fuente: AEMET OpenData, Elab. propia (@Pcontreras95)"
      ),
      color = NULL, fill = NULL
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(
        fill = "white", color = "black",
        linewidth = 0.75
      ),
      legend.position = c(0.115, 0.85),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.text = ggtext::element_markdown()
    ) +
    ggplot2::guides(color = guide_legend(
      order = 1,
      override.aes = list(linetype = c("dotted", "solid", "solid"))
    )) 

  # Add position in ranking of selected year
  for (month in unique(selected_year_monthly_pcp$mes)) {
    p <- p + ggplot2::annotate(
      geom = "text", x = subset(selected_year_monthly_pcp, mes == month)$mes,
      y = max(
        subset(selected_year_monthly_pcp, mes == month)$sumpcp,
        subset(reference_stats_monthly_pcp, mes == month)$maxpcp
      ),
      label = paste0(
        subset(reference_monthly_pcp, mes == month & ano == selected_year)$rango, "º ",
        selected_year, " ",
        subset(selected_year_monthly_pcp, mes == month)$sumpcp, "mm"
      ),
      family = "sans", size = 3.5, hjust = 0.5, vjust = 3
    )
  }

  # Add podium of years with most precipitation
  for (month in unique(selected_year_monthly_pcp$mes)) {
    for (rank in 1:3) {
      p <- p + ggplot2::annotate(
        geom = "text", x = subset(reference_monthly_pcp, mes == month & rango == rank)$mes,
        y = max(
          subset(selected_year_monthly_pcp, mes == month)$sumpcp,
          subset(reference_stats_monthly_pcp, mes == month)$maxpcp
        ),
        label = paste0(
          rank, "º ", subset(reference_monthly_pcp, mes == month & rango == rank)$ano,
          " ", subset(reference_monthly_pcp, mes == month & rango == rank)$sumpcp, "mm"
        ),
        family = "sans", size = 3.5, hjust = 0.5, vjust = -0.5 * (-rank) * 3 - 5
      )
    }
  }

  return(p)
}
