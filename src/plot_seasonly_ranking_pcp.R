SeasonRankingPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date, title) {
  # Calculate season precipitation rank
  reference_season_rank_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
      date <= as.Date(paste0(ref_end_year, "-12-31"))) |
      (date >= as.Date(paste0(as.numeric(selected_year) - 1, "-01-01")) &
        date <= as.Date(paste0(as.numeric(selected_year) + 1, "-12-31")))) |>
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
    dplyr::group_by(month, year) |>
    # We keep only data for last available day of month
    dplyr::filter(day == max(day)) |>
    dplyr::group_by(month) |>
    dplyr::mutate(ranking = rank(-cumsumpcp, ties.method = "first")) |>
    dplyr::arrange(month, ranking) |>
    dplyr::as_tibble()

  # Calculate max, min and avg precipitation for each month of the season
  reference_season_stats_pcp <- reference_season_rank_pcp |>
    dtplyr::lazy_dt() |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
      date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      cummaxpcp = max(cumsumpcp, na.rm = TRUE),
      cummeanpcp = round(mean(cumsumpcp, na.rm = TRUE), 1),
      cumminpcp = min(cumsumpcp, na.rm = TRUE)
    ) |>
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
    reference_season_stats_pcp,
    selected_year_season_cumpcp
  ) |> 
    dplyr::select(row, month, seasoncumsumpcp, cumminpcp, cummeanpcp, cummaxpcp, year_season)

  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = row)) +
    ggh4x::geom_box(aes(ymin = cumminpcp, ymax = cummaxpcp, width = 0.9), fill = "white", color = "black") +
    ggplot2::geom_col(aes(y = seasoncumsumpcp, fill = "cumsumpcp")) +
    ggplot2::geom_errorbar(aes(y = cummeanpcp, ymin = cummeanpcp, ymax = cummeanpcp, color = "mean"), 
                           linetype = "dashed") +
    ggplot2::geom_errorbar(aes(y = cummaxpcp, ymin = cummaxpcp, ymax = cummaxpcp, color = "max"), 
                           linetype = "solid", linewidth = 1) +
    ggplot2::geom_errorbar(aes(y = cumminpcp, ymin = cumminpcp, ymax = cumminpcp, color = "min"), 
                           linetype = "solid", linewidth = 1) +
    ggplot2::scale_x_discrete(limits = plot_data$row, labels = c(
      paste0("Dec", (as.numeric(selected_year) - 1) %% 100), "Jan", "Feb", "Mar", "Apr", "May",
      "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
      paste0("Jan", (as.numeric(selected_year) + 1) %% 100),
      paste0("Feb", (as.numeric(selected_year) + 1) %% 100)
    )) +
    ggplot2::scale_color_manual(breaks = c("max", "mean", "min"),
      values = c("mean" = "black", "max" = "#4d004b", "min" = "#bfd3e6"),
      labels = c("mean" = paste0("Cumulative mean seasonal precip. (", ref_start_year, "-", ref_end_year, ")"),
                 "max" = paste0("Cumulative max seasonal precip. (", ref_start_year, "-", ref_end_year, ")"),
                 "min" = paste0("Cumulative min seasonal precip. (", ref_start_year, "-", ref_end_year, ")"))) +
    ggplot2::scale_fill_manual(
      values = c("cumsumpcp" = "#2c7bb6"),
      labels = c("cumsumpcp" = paste0("Cumulative seasonal precip. (", selected_year, ")"))) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "mm"),
      breaks = seq(from = 0, to = max(max(plot_data$seasoncumsumpcp, na.rm = TRUE), 
                                      plot_data$cummaxpcp) + 125, by = 50),
      limits = c(0, max(max(plot_data$seasoncumsumpcp, na.rm = TRUE), plot_data$cummaxpcp) + 100)) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation in ", title, " ", selected_year),
      subtitle = paste0("Ranking seasonal precipitation vs. historical values (", 
                        ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/"),
      color = NULL, fill = NULL) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.135, 0.85),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5)
    ) +
    ggplot2::guides(color = guide_legend(override.aes = list(linetype = c("solid", "dotted", "solid"))))

  # Add position in ranking of selected year
  for (row_loop in plot_data[!is.na(plot_data$seasoncumsumpcp), ]$row) { # Loop over months with data in selected year
    if (row_loop %in% c("1", "2", "3")) {
      current_year <- as.numeric(selected_year) - 1
    } else {
      current_year <- selected_year
    }

    p <- p + ggplot2::annotate(
      geom = "text", x = row_loop,
      y = max(
        subset(plot_data, row == row_loop)$seasoncumsumpcp,
        subset(plot_data, row == row_loop)$cummaxpcp,
        na.rm = TRUE
      ),
      label = paste0(
        subset(reference_season_rank_pcp, month == plot_data[plot_data$row == row_loop, ]$month & year_season == current_year)$ranking, "º ",
        current_year, " ",
        subset(reference_season_rank_pcp, month == plot_data[plot_data$row == row_loop, ]$month & year_season == current_year)$cumsumpcp, "mm"
      ),
      family = "sans", size = 3, hjust = 0.5, vjust = 3
    )
  }

  # Add podium of years with most precipitation for each season
  for (row_loop in plot_data$row) { # Loop over 15 months
    for (rank in 1:3) { # Top 3 months
      p <- p + ggplot2::annotate(
        geom = "text", x = row_loop,
        y = max(
          subset(plot_data, row == row_loop)$seasoncumsumpcp,
          subset(plot_data, row == row_loop)$cummaxpcp,
          na.rm = TRUE
        ),
        label = paste0(
          rank, "º ", subset(reference_season_rank_pcp, month == plot_data[plot_data$row == row_loop, ]$month & ranking == rank)$year,
          " ", subset(reference_season_rank_pcp, month == plot_data[plot_data$row == row_loop, ]$month & ranking == rank)$cumsumpcp, "mm"
        ),
        family = "sans", size = 3.15, hjust = 0.5, vjust = -0.5 * (-rank) * 3 - 5
      )
    }
  }

  return(list(p, plot_data |> dplyr::select(-year_season), "row", "seasoncumsumpcp"))
  
}
