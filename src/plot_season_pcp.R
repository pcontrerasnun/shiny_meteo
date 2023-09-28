#' Plot cumulative monthly sum precipitation along with historical and seasonal precipitation 
#' values
#'
#' Plots cumulative monthly sum precipitation in selected year along with historical and
#' seasonal precipitation values based on a reference period defined by 'ref_start_year' 
#' and 'ref_end_year'
#'
#' @param data An R dataset with AEMET Open data
#' @param selected_year Year of study
#' @param ref_start_year Start year of reference period
#' @param ref_end_year End year of reference period
#' @param max_date Max date of data
#' @returns A ggplot2 plot
#' @examples 
#' SeasonPcpPlot(data, 2023, 1981, 2010, '2023-09-24')
SeasonPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate mean historical precipitation in each month
  reference_mean_monthly_pcp <- data |>
    dplyr::filter(fecha >= as.Date(paste0(ref_start_year, "-01-01")) &
      fecha <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::group_by(mes) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::arrange(mes) |>
    dplyr::mutate(meanpcp = round(sumpcp / (ref_end_year - ref_start_year), 1)) |>
    dplyr::select(-sumpcp)

  # Calculate min and max historical precipitation in each month
  reference_maxmin_monthly_pcp <- data |>
    dplyr::filter(fecha >= as.Date(paste0(ref_start_year, "-01-01")) &
      fecha <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::group_by(ano, mes) |>
    dplyr::mutate(sumpcp = sum(pcp)) |>
    dplyr::arrange(ano, mes) |>
    dplyr::group_by(mes) |>
    dplyr::summarise(
      min = round(min(sumpcp, na.rm = TRUE), 1),
      max = round(max(sumpcp, na.rm = TRUE), 1)
    ) |>
    dplyr::arrange(mes)

  # Calculate median historical precipitation for each season
  reference_pcts_season_pcp <- data |>
    dplyr::filter(fecha >= as.Date(paste0(ref_start_year, "-01-01")) &
      fecha <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::mutate(season = dplyr::case_when(
      mes %in% c("12", "01", "02") ~ "4-invierno",
      mes %in% c("03", "04", "05") ~ "1-primavera",
      mes %in% c("06", "07", "08") ~ "2-verano",
      mes %in% c("09", "10", "11") ~ "3-otoño"
    )) |>
    dplyr::as_tibble() |>
    dplyr::mutate(season_aux = dplyr::case_when(
      lubridate::leap_year(fecha) == TRUE & lubridate::yday(fecha) <= 60 ~ 1,
      lubridate::leap_year(fecha) == FALSE & lubridate::yday(fecha) <= 59 ~ 1,
      TRUE ~ 0,
    )) |>
    dtplyr::lazy_dt() |>
    dplyr::mutate(ano_season = as.numeric(ano) - season_aux) |>
    dplyr::group_by(ano_season, season) |>
    dplyr::mutate(sumpcp = sum(pcp)) |>
    dplyr::group_by(season) |>
    dplyr::summarise(seasonmedianpcp = round(quantile(sumpcp, probs = 0.50, na.rm = TRUE), 1)) |>
    dplyr::arrange(season) |>
    dplyr::as_tibble() |>
    tidyr::uncount(weights = c(3, 3, 3, 3), .remove = FALSE) |>
    dtplyr::lazy_dt() |>
    dplyr::mutate(mes = c(
      "03", "04", "05", "06", "07", "08", "09", "10", "11", "12",
      "01", "02"
    )) |>
    dplyr::select(-season)

  # Calculate total precipitation in each month for selected year
  selected_year_monthly_pcp <- data |>
    dplyr::filter(fecha >= as.Date(paste0(as.numeric(selected_year) - 1, "-12-01")) &
      fecha < as.Date(paste0(as.numeric(selected_year) + 1, "-03-01"))) |>
    dplyr::group_by(mes, ano) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE)) |>
    dplyr::arrange(ano, mes) |>
    dplyr::ungroup() |>
    dplyr::select(-mes) |> # para el cbind de despues, que no haya dos columnas llamadas igual
    dplyr::as_tibble()

  # Calculate cumulated precipitation across the seasons of the selected year
  selected_year_season_pcp <- data |>
    dplyr::filter(fecha >= as.Date(paste0(as.numeric(selected_year) - 1, "-12-01")) &
      fecha < as.Date(paste0(as.numeric(selected_year) + 1, "-03-01"))) |>
    dplyr::mutate(season = dplyr::case_when(
      mes %in% c("12", "01", "02") ~ "4-invierno",
      mes %in% c("03", "04", "05") ~ "1-primavera",
      mes %in% c("06", "07", "08") ~ "2-verano",
      mes %in% c("09", "10", "11") ~ "3-otoño"
    )) |>
    dplyr::as_tibble() |>
    dplyr::mutate(season_aux = dplyr::case_when(
      lubridate::leap_year(fecha) == TRUE & lubridate::yday(fecha) <= 60 ~ 1,
      lubridate::leap_year(fecha) == FALSE & lubridate::yday(fecha) <= 59 ~ 1,
      TRUE ~ 0,
    )) |>
    dplyr::mutate(ano_season = as.numeric(ano) - as.numeric(season_aux)) |>
    dplyr::mutate(mes = stringr::str_replace(mes, "12", "00")) |>
    dplyr::group_by(mes, season, ano_season) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE)) |>
    dplyr::arrange(ano_season, season, mes) |>
    dplyr::group_by(season, ano_season) |>
    dplyr::summarise(seasoncumsumpcp = cumsum(sumpcp)) |>
    dplyr::arrange(ano_season, season) |>
    dplyr::ungroup()

  # Join final data
  plot_data <- qpcR:::cbind.na(
    data.frame(mes = c(
      "12", "01", "02", "03", "04", "05", "06",
      "07", "08", "09", "10", "11", "12", "01", "02"
    )),
    selected_year_season_pcp
  )

  plot_data <- qpcR:::cbind.na(plot_data, selected_year_monthly_pcp) |>
    dtplyr::lazy_dt() |>
    dplyr::select(mes, season, seasoncumsumpcp, sumpcp) |>
    dplyr::left_join(reference_mean_monthly_pcp, by = "mes") |>
    dplyr::left_join(reference_maxmin_monthly_pcp, by = "mes") |>
    dplyr::left_join(reference_pcts_season_pcp, by = "mes") |>
    dplyr::mutate(row = as.character(row_number())) |>
    dplyr::mutate(season2 = rep(1:5, each = 3)) |>
    dplyr::as_tibble()

  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = row)) +
    ggh4x::geom_box(aes(ymin = min, ymax = max, width = 0.9),
      fill = "white",
      color = "black"
    ) +
    ggplot2::geom_col(aes(y = sumpcp, fill = "Precip. mensual acumulada")) +
    ggplot2::geom_errorbar(aes(
      y = meanpcp, ymin = meanpcp, ymax = meanpcp,
      color = "Media mensual histórica"
    ), linetype = "dashed") +
    ggplot2::geom_errorbar(aes(
      y = seasonmedianpcp, ymin = seasonmedianpcp, ymax = seasonmedianpcp,
      color = "Mediana estacional histórica"
    ), linetype = "dashed") +
    ggplot2::geom_errorbar(aes(
      y = max, ymin = max, ymax = max,
      color = "Máximo mensual histórico"
    ), linetype = "solid") +
    ggplot2::geom_errorbar(aes(
      y = min, ymin = min, ymax = min,
      color = "Mínimo mensual histórico"
    ), linetype = "solid") +
    ggplot2::geom_line(aes(
      y = seasoncumsumpcp, group = season2,
      color = "Precip. estacional acumulada"
    ), linewidth = 0.85, lineend = "round") +
    ggplot2::scale_color_manual(
      breaks = c(
        "Media mensual histórica",
        "Mediana estacional histórica",
        "Máximo mensual histórico", "Mínimo mensual histórico",
        "Precip. estacional acumulada"
      ),
      values = c(
        "Media mensual histórica" = "black",
        "Mediana estacional histórica" = "#33a02c",
        "Máximo mensual histórico" = "#6a3d9a",
        "Mínimo mensual histórico" = "#d7191c",
        "Precip. estacional acumulada" = "#253494"
      )
    ) +
    ggplot2::scale_fill_manual(
      values = c("Precip. mensual acumulada" = "#2c7bb6"),
      labels = c(
        "Precip. mensual acumulada" =
          glue::glue("<span style = 'color: #2c7bb6; '>Precip. mensual acumulada</span>")
      )
    ) +
    ggplot2::guides(color = guide_legend(
      order = 1,
      override.aes = list(linetype = c(
        "dotted", "dotted",
        "solid", "solid",
        "solid"
      ))
    )) +
    ggplot2::scale_x_discrete(
      limits = c(
        "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11",
        "12", "13", "14", "15"
      ),
      labels = c(
        paste0("dic", (as.numeric(selected_year) - 1) %% 100), "ene", "feb", "mar", "abr",
        "may", "jun", "jul", "ago", "sep", "oct", "nov",
        "dic", paste0("ene", (as.numeric(selected_year) + 1) %% 100),
        paste0("feb", (as.numeric(selected_year) + 1) %% 100)
      )
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "mm"),
      breaks = seq(
        from = 0, to = max(max(plot_data$max), max(plot_data$seasoncumsumpcp, na.rm = TRUE))
        + 100, by = 50
      ),
      limits = c(0, max(plot_data$max) + 50)
    ) + # expand = c(0, 20, 0, 50)
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
      legend.position = c(0.1, 0.85),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.text = ggtext::element_markdown()
    )

  return(p)
}
