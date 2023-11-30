
SeasonRankingTmeanPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate cumulative historical tmean percentiles for each season
  reference_season_cumtmean <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::mutate(season = dplyr::case_when(
      month %in% c("12", "01", "02") ~ "Winter",
      month %in% c("03", "04", "05") ~ "Spring",
      month %in% c("06", "07", "08") ~ "Summer",
      month %in% c("09", "10", "11") ~ "Autumn"
    )) |>
    dplyr::as_tibble() |> 
    # We calculate number of days to 1st of March (depends if year is leap or not)
    dplyr::mutate(season_aux = dplyr::case_when(
      lubridate::leap_year(date) == TRUE & lubridate::yday(date) <= 60 ~ 1,
      lubridate::leap_year(date) == FALSE & lubridate::yday(date) <= 59 ~ 1,
      TRUE ~ 0,
    )) |>
    # Put year of Jan and Feb as year - 1 to be part of winter season of previous year
    dplyr::mutate(year_season = as.numeric(year) - season_aux) |> 
    dplyr::group_by(year_season, season) |>
    # Cummean ignoring NAs and replacing NAs with last valid value
    dplyr::mutate(cumtmean = round(replace(tmean, complete.cases(tmean), cummean(na.omit(tmean))), 1)) |>
    tidyr::fill(cumtmean) |> 
    # We keep only data for last day of month
    dplyr::filter(lubridate::day(date) == lubridate::days_in_month(date)) |> 
    dplyr::group_by(month) |>
    dplyr::summarise(
      cump00tmean = round(quantile(cumtmean, probs = 0.00, na.rm = TRUE), 1),
      cump05tmean = round(quantile(cumtmean, probs = 0.05, na.rm = TRUE), 1),
      cump50tmean = round(quantile(cumtmean, probs = 0.50, na.rm = TRUE), 1),
      cump95tmean = round(quantile(cumtmean, probs = 0.95, na.rm = TRUE), 1),
      cump100tmean = round(quantile(cumtmean, probs = 1, na.rm = TRUE), 1)
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
      # Add Dec past year and Jan, Feb next year
      dplyr::bind_rows(df, head(df, n = 3))
    }) |>
    dplyr::mutate(month = dplyr::case_when(
      month %in% "00" ~ "12", # rename December correctly
      TRUE ~ month
    )) |>
    dplyr::mutate(row = as.character(row_number()))
  
  # Calculate cumulated precipitation across the seasons of the selected year
  selected_year_season_cumtmean <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(as.numeric(selected_year) - 1, "-12-01")) &
                    date < as.Date(paste0(as.numeric(selected_year) + 1, "-03-01"))) |>
    dplyr::mutate(season = dplyr::case_when(
      month %in% c("12", "01", "02") ~ "4-Winter",
      month %in% c("03", "04", "05") ~ "1-Spring",
      month %in% c("06", "07", "08") ~ "2-Summer",
      month %in% c("09", "10", "11") ~ "3-Autumn"
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
    dplyr::group_by(season, year_season) |>
    dplyr::mutate(cumtmean = round(replace(tmean, complete.cases(tmean), cummean(na.omit(tmean))), 1)) |>
    tidyr::fill(cumtmean) |> 
    # We keep only last available data for each month
    dplyr::group_by(month, year) |> 
    dplyr::slice_tail(n = 1) |> 
    dplyr::ungroup() |> 
    dplyr::arrange(year_season, season, month) |> 
    dplyr::select(-month)
  
  # Calculate rankings
  reference_season_ranking_cumtmean <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(
      (date >= as.Date(paste0(ref_start_year, "-01-01")) &
         date <= as.Date(paste0(ref_end_year, "-12-31"))) |
        (date >= as.Date(paste0(as.numeric(selected_year) - 1, "-01-01")) & # Include year of study
           date <= as.Date(paste0(as.numeric(selected_year) + 1, "-12-31")))) |>
    dplyr::mutate(season = dplyr::case_when(
      month %in% c("12", "01", "02") ~ "Winter",
      month %in% c("03", "04", "05") ~ "Spring",
      month %in% c("06", "07", "08") ~ "Summer",
      month %in% c("09", "10", "11") ~ "Autumn"
    )) |>
    dplyr::as_tibble() |> 
    # We calculate number of days to 1st of March (depends if year is leap or not)
    dplyr::mutate(season_aux = dplyr::case_when(
      lubridate::leap_year(date) == TRUE & lubridate::yday(date) <= 60 ~ 1,
      lubridate::leap_year(date) == FALSE & lubridate::yday(date) <= 59 ~ 1,
      TRUE ~ 0,
    )) |>
    # Put year of Jan and Feb as year - 1 to be part of winter season of previous year
    dplyr::mutate(year_season = as.numeric(year) - season_aux) |> 
    dplyr::group_by(year_season, season) |>
    # Cummean ignoring NAs and replacing NAs with last valid value
    dplyr::mutate(cumtmean = round(replace(tmean, complete.cases(tmean), cummean(na.omit(tmean))), 1)) |>
    tidyr::fill(cumtmean) |> 
    # We keep only last available data for each month
    dplyr::group_by(month, year) |> 
    dplyr::slice_tail(n = 1) |> 
    dplyr::ungroup() |> 
    dplyr::group_by(month) |> 
    dplyr::mutate(ranking = rank(-cumtmean, ties.method = "first")) |>
    dplyr::arrange(-cumtmean, season, month) |> 
    dplyr::select(-c(tmin, tmax, tmean))
  
  # Join final data
  plot_data <- qpcR:::cbind.na(
    reference_season_cumtmean,
    selected_year_season_cumtmean
  )
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = row)) +
    ggplot2::geom_point(aes(y = cumtmean), color = "red", na.rm = TRUE) +
    ggplot2::geom_line(aes(y = cumtmean, group = 1, linetype = "cumtmean"), color = "red", na.rm = TRUE) +
    ggplot2::geom_line(aes(y = cump50tmean, group = 1, linetype = "cump50")) +
    ggplot2::geom_line(aes(y = cump05tmean, group = 1, linetype = "cump05")) +
    ggplot2::geom_line(aes(y = cump95tmean, group = 1, linetype = "cump95")) +
    ggplot2::scale_linetype_manual(
      values = c("cump50" = "longdash", "cump05" = "dotdash", "cump95" = "dotdash", "cumtmean" = "solid"),
      labels = c("cump50" = paste0("Cumulative seasonal normal mean temp. (", ref_start_year, "-", ref_end_year, ")"),
                 "cump05" = expr(paste(italic(P[5]), " (", !!ref_start_year, "-", !!ref_end_year, ")")),
                 "cump95" = expr(paste(italic(P[95]), " (", !!ref_start_year, "-", !!ref_end_year, ")")),
                 "cumtmean" = paste0("Cumulative seasonal mean temp. (", selected_year, ")")),
      breaks = c("cump95", "cump50", "cumtmean", "cump05")) + # To give order
    ggplot2::scale_x_discrete(
      limits = plot_data$row,
      labels = c(
        paste0("Dec", (as.numeric(selected_year) - 1) %% 100), "Jan", "Feb", "Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
        paste0("Jan", (as.numeric(selected_year) + 1) %% 100),
        paste0("Feb", (as.numeric(selected_year) + 1) %% 100)
      )) +
    ggplot2::scale_y_continuous(
      limits = c(min(min(plot_data$cumtmean, na.rm = TRUE), min(plot_data$cump05tmean, na.rm = TRUE)) - 3, 
                 max(max(plot_data$cumtmean, na.rm = TRUE), max(plot_data$cump95tmean, na.rm = TRUE)) + 2),
      breaks = seq(from = round(min(min(plot_data$cumtmean, na.rm = TRUE), min(plot_data$cump05tmean, na.rm = TRUE)) - 4), 
                   to = round(max(max(plot_data$cumtmean, na.rm = TRUE), max(plot_data$cump95tmean, na.rm = TRUE)) + 3), by = 5),
      labels = function(x) paste0(x, "ºC")) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = "Temperature in Madrid - Retiro",
      subtitle = paste0(
        "Cumulative seasonal mean temperature vs. historical percentiles (",
        ref_start_year, "-", ref_end_year, ")"),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)"
      )) +
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
    ggplot2::guides(linetype = guide_legend(override.aes = list(color = c("black", "black", "red", "black"))))
  
  # Add position in ranking of selected year
  for (row_loop in plot_data[!is.na(plot_data$cumtmean), ]$row) { # Loop over months with data in selected year
    if (row_loop %in% c("1", "2", "3")) {
      current_year <- as.numeric(selected_year) - 1
    } else {
      current_year <- selected_year
    }
    
    p <- p + ggplot2::annotate(
      geom = "text", x = row_loop,
      y = min(plot_data$cumtmean, plot_data$cump00tmean, na.rm = TRUE),
      label = paste0(
        subset(reference_season_ranking_cumtmean, month == plot_data[plot_data$row == row_loop, ]$month & year_season == current_year)$ranking, "º ",
        current_year, " ",
        subset(reference_season_ranking_cumtmean, month == plot_data[plot_data$row == row_loop, ]$month & year_season == current_year)$cumtmean, "ºC"
      ),
      family = "sans", size = 3.15, hjust = 0.5, vjust = 6
    )
  }
  
  # Add podium of years with most precipitation for each season
  for (row_loop in plot_data$row) { # Loop over 15 months
    for (rank in 1:3) { # Top 3 months
      p <- p + ggplot2::annotate(
        geom = "text", x = row_loop,
        y = min(plot_data$cumtmean, plot_data$cump00tmean, na.rm = TRUE),
        label = paste0(
          rank, "º ", subset(reference_season_ranking_cumtmean, month == plot_data[plot_data$row == row_loop, ]$month & ranking == rank)$year,
          " ", subset(reference_season_ranking_cumtmean, month == plot_data[plot_data$row == row_loop, ]$month & ranking == rank)$cumtmean, "ºC"
        ),
        family = "sans", size = 3.15, hjust = 0.5, vjust = -0.5 * (-rank) * 3 - 1
      )
    }
  }

  return(p)
}







