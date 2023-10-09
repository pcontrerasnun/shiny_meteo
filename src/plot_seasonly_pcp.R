#' Plot cumulative season sum precipitation along with historical seasonal precipitation
#' medians
#'
#' Plots cumulative monthly sum precipitation in each season (sum returns to 0 when a new 
#' season begins) in selected year along with historical seasonal precipitation medians based on 
#' a reference period defined by 'ref_start_year' and 'ref_end_year'
#'
#' @param data An R dataset with AEMET Open data
#' @param selected_year Year of study
#' @param ref_start_year Start year of reference period
#' @param ref_end_year End year of reference period
#' @param max_date Max date of data
#' @returns A ggplot2 plot
#' @examples
#' SeasonPcpPlot(data, 2023, 1981, 2010, "2023-09-24")
SeasonPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate cumulative historical precipitation percentiles for each season
  reference_cumpcts_season_pcp <- data |>
    dplyr::filter(fecha >= as.Date(paste0(ref_start_year, "-01-01")) &
      fecha <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::mutate(season = dplyr::case_when(
      mes %in% c("12", "01", "02") ~ "4-invierno",
      mes %in% c("03", "04", "05") ~ "1-primavera",
      mes %in% c("06", "07", "08") ~ "2-verano",
      mes %in% c("09", "10", "11") ~ "3-otoño"
    )) |>
    dplyr::as_tibble() |>
    # We calculate number of days to 1st of March (depends if year is leap or not)
    dplyr::mutate(season_aux = dplyr::case_when(
      lubridate::leap_year(fecha) == TRUE & lubridate::yday(fecha) <= 60 ~ 1,
      lubridate::leap_year(fecha) == FALSE & lubridate::yday(fecha) <= 59 ~ 1,
      TRUE ~ 0,
    )) |>
    dtplyr::lazy_dt() |>
    # Put year of Jan and Feb as year - 1 to be part of winter season of previous year
    dplyr::mutate(ano_season = as.numeric(ano) - season_aux) |>
    dplyr::group_by(ano_season, season) |>
    dplyr::mutate(cumsumpcp = cumsum(ifelse(is.na(pcp), 0, pcp))) |>
    # We keep only data for last day of month
    dplyr::filter(lubridate::day(fecha) == lubridate::days_in_month(fecha)) |>
    dplyr::group_by(mes) |>
    dplyr::summarise(
      cumq00pcp = round(quantile(cumsumpcp, probs = 0.00, na.rm = TRUE), 1),
      cumq20pcp = round(quantile(cumsumpcp, probs = 0.20, na.rm = TRUE), 1),
      cumq40pcp = round(quantile(cumsumpcp, probs = 0.40, na.rm = TRUE), 1),
      cumq50pcp = round(quantile(cumsumpcp, probs = 0.50, na.rm = TRUE), 1),
      cumq60pcp = round(quantile(cumsumpcp, probs = 0.60, na.rm = TRUE), 1),
      cumq80pcp = round(quantile(cumsumpcp, probs = 0.80, na.rm = TRUE), 1),
      cumq100pcp = round(quantile(cumsumpcp, probs = 1, na.rm = TRUE), 1)
        ) |>
    dplyr::ungroup() |>
    dplyr::mutate(mes = dplyr::case_when(
      mes %in% "12" ~ "00", # to put December first
      TRUE ~ mes
    )) |>
    as_tibble() |>
    dplyr::arrange(mes) |>
    # Only way to use working dataset as parameter
    do({
      df <- .
      dplyr::bind_rows(df, head(df, n = 3))
    }) |>
    dplyr::mutate(mes = dplyr::case_when(
      mes %in% "00" ~ "12", # rename December correctly
      TRUE ~ mes
    )) |>
    dplyr::mutate(row = as.character(row_number()))

  # Calculate cumulated precipitation across the seasons of the selected year
  selected_year_season_cumpcp <- data |>
    dplyr::filter(fecha >= as.Date(paste0(as.numeric(selected_year) - 1, "-12-01")) &
      fecha < as.Date(paste0(as.numeric(selected_year) + 1, "-03-01"))) |>
    dplyr::mutate(season = dplyr::case_when(
      mes %in% c("12", "01", "02") ~ "4-invierno",
      mes %in% c("03", "04", "05") ~ "1-primavera",
      mes %in% c("06", "07", "08") ~ "2-verano",
      mes %in% c("09", "10", "11") ~ "3-otoño"
    )) |>
    dplyr::as_tibble() |>
    # We calculate number of days to 1st of March (depends if year is leap or not)
    dplyr::mutate(season_aux = dplyr::case_when(
      lubridate::leap_year(fecha) == TRUE & lubridate::yday(fecha) <= 60 ~ 1,
      lubridate::leap_year(fecha) == FALSE & lubridate::yday(fecha) <= 59 ~ 1,
      TRUE ~ 0,
    )) |>
    # Put year of Jan and Feb as year - 1 to be part of winter season of previous year
    dplyr::mutate(ano_season = as.numeric(ano) - as.numeric(season_aux)) |>
    dplyr::mutate(mes = stringr::str_replace(mes, "12", "00")) |> # to put December first
    dplyr::group_by(mes, season, ano_season) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE)) |>
    dplyr::arrange(ano_season, season, mes) |>
    dplyr::group_by(season, ano_season) |>
    dplyr::summarise(seasoncumsumpcp = cumsum(sumpcp)) |>
    dplyr::arrange(ano_season, season) |>
    dplyr::ungroup() |>
    dplyr::select(-season)

  # Join final data
  plot_data <- qpcR:::cbind.na(
    reference_cumpcts_season_pcp,
    selected_year_season_cumpcp
  )

  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = row)) +
    ggh4x::geom_box(aes(ymin = 0, ymax = cumq00pcp, fill = "P00", width = 0.9), alpha = 0.5) +
    ggh4x::geom_box(aes(ymin = cumq00pcp, ymax = cumq20pcp, fill = "P20", width = 0.9), alpha = 0.5) +
    ggh4x::geom_box(aes(ymin = cumq20pcp, ymax = cumq40pcp, fill = "P40", width = 0.9), alpha = 0.5) +
    ggh4x::geom_box(aes(ymin = cumq40pcp, ymax = cumq60pcp, fill = "P60", width = 0.9), alpha = 0.5) +
    ggh4x::geom_box(aes(ymin = cumq60pcp, ymax = cumq80pcp, fill = "P80", width = 0.9), alpha = 0.5) +
    ggh4x::geom_box(aes(ymin = cumq80pcp, ymax = cumq100pcp, fill = "P100", width = 0.9), alpha = 0.5) +
    ggplot2::geom_col(aes(y = seasoncumsumpcp, fill = "Precip. estacional acumulada")) +
    ggplot2::geom_col(aes(y = cumq50pcp, color = "Precip. mediana estacional acumulada histórica"),
      fill = NA, linewidth = 1
    ) +
    ggplot2::scale_color_manual(
      breaks = c("Precip. mediana estacional acumulada histórica"),
      values = c("Precip. mediana estacional acumulada histórica" = "#d7191c")
    ) +
    ggplot2::scale_fill_manual(
      values = c("Precip. estacional acumulada" = "#2c7bb6", "P100" = "#abd9e9",
                 "P80" = "#e0f3f8", "P60" = "white", "P40" = "#fee090", "P20" = "#fdae61",
                 "P00" = "#f46d43"), 
      labels = c(
        "Precip. mensual acumulada" =
          glue::glue("<span style = 'color: #2c7bb6; '>Precip. mensual acumulada</span>"),
        "P00" = "Estación extrem. seca", "P20" = "Estación muy seca", "P40" = "Estación seca",
        "P80" = "Estación húmeda", "P100" = "Estación muy húmeda", "P60" = "Estación normal"
      ),
      breaks = c("P100", "P80", "P60", "P40", "P20", "P00", "Precip. mensual acumulada") # to order
    ) +
    ggplot2::scale_x_discrete(
      limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"),
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
        from = 0, to = max(max(plot_data$cumq100pcp), max(plot_data$seasoncumsumpcp, na.rm = TRUE))
        + 250, by = 25
      ),
      limits = c(0, max(max(plot_data$cumq100pcp), max(plot_data$seasoncumsumpcp, na.rm = TRUE) + 250))
    ) + # expand = c(0, 20, 0, 50)
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitación en Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Precipitación estacional acumulada comparada con valores históricos (",
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
      legend.position = c(0.1475, 0.85),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.text = ggtext::element_markdown()
    )

  return(p)
}
