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
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
      date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
      date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::mutate(season = dplyr::case_when(
      month %in% c("12", "01", "02") ~ "4-winter",
      month %in% c("03", "04", "05") ~ "1-spring",
      month %in% c("06", "07", "08") ~ "2-summer",
      month %in% c("09", "10", "11") ~ "3-autumn"
    )) |>
    dplyr::as_tibble() |>
    # We calculate number of days to 1st of March (depends if year is leap or not)
    dplyr::mutate(season_aux = dplyr::case_when(
      lubridate::leap_year(date) == TRUE & lubridate::yday(date) <= 60 ~ 1,
      lubridate::leap_year(date) == FALSE & lubridate::yday(date) <= 59 ~ 1,
      TRUE ~ 0,
    )) |>
    dtplyr::lazy_dt() |>
    # Put year of Jan and Feb as year - 1 to be part of winter season of previous year
    dplyr::mutate(year_season = as.numeric(year) - season_aux) |>
    dplyr::group_by(year_season, season) |>
    dplyr::mutate(cumsumpcp = cumsum(ifelse(is.na(pcp), 0, pcp))) |>
    # We keep only data for last day of month
    dplyr::filter(lubridate::day(date) == lubridate::days_in_month(date)) |>
    dplyr::group_by(month) |>
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
    dplyr::mutate(month = dplyr::case_when(
      month %in% "12" ~ "00", # to put December first
      TRUE ~ month
    )) |>
    dplyr::arrange(month) |>
    dplyr::as_tibble() |> 
    # Only way to use working dataset as parameter
    do({
      df <- .
      dplyr::bind_rows(df, head(df, n = 3))
    }) |>
    dplyr::mutate(month = dplyr::case_when(
      month %in% "00" ~ "12", # rename December correctly
      TRUE ~ month
    )) |>
    dplyr::mutate(row = as.character(row_number()))

  # Calculate cumulated precipitation across the seasons of the selected year
  selected_year_season_cumpcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(as.numeric(selected_year) - 1, "-12-01")) &
      date < as.Date(paste0(as.numeric(selected_year) + 1, "-03-01"))) |>
    dplyr::mutate(season = dplyr::case_when(
      month %in% c("12", "01", "02") ~ "4-invierno",
      month %in% c("03", "04", "05") ~ "1-spring",
      month %in% c("06", "07", "08") ~ "2-summer",
      month %in% c("09", "10", "11") ~ "3-autumn"
    )) |>
    dplyr::as_tibble() |>
    # We calculate number of days to 1st of March (depends if year is leap or not)
    dplyr::mutate(season_aux = dplyr::case_when(
      lubridate::leap_year(date) == TRUE & lubridate::yday(date) <= 60 ~ 1,
      lubridate::leap_year(date) == FALSE & lubridate::yday(date) <= 59 ~ 1,
      TRUE ~ 0,
    )) |>
    # Put year of Jan and Feb as year - 1 to be part of winter season of previous year
    dplyr::mutate(year_season = as.numeric(year) - as.numeric(season_aux)) |>
    dplyr::mutate(month = stringr::str_replace(month, "12", "00")) |> # to put December first
    dplyr::group_by(month, season, year_season) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE), .groups = "keep") |>
    dplyr::arrange(year_season, season, month) |>
    dplyr::group_by(season, year_season) |>
    dplyr::reframe(seasoncumsumpcp = cumsum(sumpcp)) |> # reframe = summarise
    dplyr::arrange(year_season, season) |>
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
    # https://github.com/tidyverse/ggplot2/issues/3532
    ggplot2::geom_col(aes(y = seasoncumsumpcp, fill = "Cumulative seasonal precip."), na.rm = TRUE) +
    ggplot2::geom_col(aes(y = cumq50pcp, color = "Historical cumulative seasonal median precip."),
      fill = NA, linewidth = 1
    ) +
    ggplot2::scale_color_manual(
      breaks = c("Historical cumulative seasonal median precip."),
      values = c("Historical cumulative seasonal median precip." = "#d7191c")
    ) +
    ggplot2::scale_fill_manual(
      values = c("Cumulative seasonal precip." = "#2c7bb6", "P100" = "#abd9e9",
                 "P80" = "#e0f3f8", "P60" = "white", "P40" = "#fee090", "P20" = "#fdae61",
                 "P00" = "#f46d43"), 
      labels = c(
        "Cumulative seasonal precip." =
          glue::glue("<span style = 'color: #2c7bb6; '>Cumulative seasonal precip.</span>"),
        "P00" = "Extrem. dry season", "P20" = "Very dry season", "P40" = "Dry season",
        "P80" = "Wet season", "P100" = "Very wet season", "P60" = "Normal season"
      ),
      breaks = c("P100", "P80", "P60", "P40", "P20", "P00", "Cumulative seasonal precip.") # To give order
    ) +
    ggplot2::scale_x_discrete(
      limits = plot_data$row,
      labels = c(
        paste0("Dec", (as.numeric(selected_year) - 1) %% 100), "Jan", "Feb", "Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
        paste0("Jan", (as.numeric(selected_year) + 1) %% 100),
        paste0("Feb", (as.numeric(selected_year) + 1) %% 100)
      )
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "mm"),
      breaks = seq(
        from = 0, to = max(max(plot_data$cumq100pcp), max(plot_data$seasoncumsumpcp, na.rm = TRUE))
        + 125, by = 25
      ),
      limits = c(0, max(max(plot_data$cumq100pcp), max(plot_data$seasoncumsumpcp, na.rm = TRUE)) + 125)
    ) + # expand = c(0, 20, 0, 50)
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation in Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Cumulative seasonal precipitation vs. historical values (",
        ref_start_year, "-", ref_end_year, ")"
      ),
      caption = paste0(
        "Updated: ", max_date, ", Source: AEMET OpenData, Graph: @Pcontreras95 (Twitter)"
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
