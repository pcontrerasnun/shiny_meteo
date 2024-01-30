SeasonRankingTmeanPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date, title) {
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
      cump20tmean = round(quantile(cumtmean, probs = 0.20, na.rm = TRUE), 1),
      cump40tmean = round(quantile(cumtmean, probs = 0.40, na.rm = TRUE), 1),
      cump50tmean = round(quantile(cumtmean, probs = 0.50, na.rm = TRUE), 1),
      cump60tmean = round(quantile(cumtmean, probs = 0.60, na.rm = TRUE), 1),
      cump80tmean = round(quantile(cumtmean, probs = 0.80, na.rm = TRUE), 1),
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
    ) |> 
    dplyr::select(row, month, cumtmean, everything(), -c(date, day, year, season, year_season, tmin,
                                                         tmax, tmean, season_aux)) |> 
    dplyr::mutate(diffmedian = round(cumtmean - cump50tmean, 1))
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = row, group = 1)) +
    ggplot2::geom_ribbon(
      aes(ymin = cump100tmean, ymax = cump100tmean + 4, fill = ">P100", group = 1), alpha = 0.3, 
      color = "#b2182b", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(
      aes(ymin = cump80tmean, ymax = cump100tmean, fill = "P100"), alpha = 0.3, 
      color = "#ef8a62", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(
      aes(ymin = cump60tmean, ymax = cump80tmean, fill = "P80"), alpha = 0.3, 
      color = "#fddbc7", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(
      aes(ymin = cump40tmean, ymax = cump60tmean, fill = "P60"), alpha = 0.3, 
      color = NA, linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(
      aes(ymin = cump20tmean, ymax = cump40tmean, fill = "P40"), alpha = 0.3, 
      color = "#d1e5f0", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(
      aes(ymin = cump00tmean, ymax = cump20tmean, fill = "P20"), alpha = 0.3, 
      color = "#67a9cf", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(
      aes(ymin = cump00tmean - 4, ymax = cump00tmean, fill = "P00"), alpha = 0.3, 
      color = "#2166ac", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_point(aes(y = cumtmean), color = "black", na.rm = TRUE) +
    ggplot2::geom_line(aes(y = cumtmean, group = 1, color = "cumtmean"), linewidth = 0.75, na.rm = TRUE) +
    ggrepel::geom_label_repel(
      aes(y = cumtmean, label = paste0(ifelse(diffmedian > 0, "+", "") , diffmedian, "*ºC")), # ~vs.~italic(P[50])
      parse = TRUE, na.rm = TRUE) +
    ggplot2::scale_fill_manual(
      values = c(">P100" = "#b2182b", "P100" = "#ef8a62", "P80" = "#fddbc7", "P60" = "#f7f7f7",
                 "P40" = "#d1e5f0", "P20" = "#67a9cf", "P00" = "#2166ac"),
      breaks = c(">P100", "P100", "P80", "P60", "P40", "P20", "P00"), # To give order
      labels = c(">P100" = expr(paste("Extrem. hot season (>", italic(P[100]), ") (", 
                                     !!ref_start_year, "-", !!ref_end_year, ")")), 
                 "P100" = expr(paste("Very hot season (", italic(P[80]), "-", italic(P[100]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")), 
                 "P80" = expr(paste("Hot season (", italic(P[60]), "-", italic(P[80]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")), 
                 "P60" = expr(paste("Normal season (", italic(P[40]), "-", italic(P[60]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")),
                 "P40" = expr(paste("Cold season (", italic(P[20]), "-", italic(P[40]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")),
                 "P20" = expr(paste("Very cold season (", italic(P[00]), "-", italic(P[20]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")),
                 "P00" = expr(paste("Extrem. cold season (<", italic(P[00]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")))) +
    ggplot2::scale_color_manual(
      values = c("cumtmean" = "black"), 
      label = paste0("Cumulative seasonal mean temp. (", selected_year, ")"), guide = guide_legend(order = 1)) +
    ggplot2::scale_x_discrete(
      limits = plot_data$row,
      labels = c(
        paste0("Dec", (as.numeric(selected_year) - 1) %% 100), "Jan", "Feb", "Mar", "Apr", "May", 
        "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
        paste0("Jan", (as.numeric(selected_year) + 1) %% 100),
        paste0("Feb", (as.numeric(selected_year) + 1) %% 100)
      )) +
    ggplot2::scale_y_continuous(
      limits = c(min(min(plot_data$cumtmean, na.rm = TRUE), min(plot_data$cump00tmean, na.rm = TRUE) - 4) - 4, 
                 max(max(plot_data$cumtmean, na.rm = TRUE), max(plot_data$cump100tmean, na.rm = TRUE) + 4) + 2),
      breaks = round(seq(from = round(min(min(plot_data$cumtmean, na.rm = TRUE), 
                                          min(plot_data$cump00tmean, na.rm = TRUE) - 4) - 4), 
                   to = round(max(max(plot_data$cumtmean, na.rm = TRUE), 
                                  max(plot_data$cump100tmean, na.rm = TRUE) + 4) + 3), by = 5) / 5) * 5,
      labels = function(x) paste0(x, "ºC")) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Temperature in ", title, " ", selected_year),
      subtitle = paste0(
        "Cumulative seasonal mean temperature vs. historical percentiles (",
        ref_start_year, "-", ref_end_year, ")"),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/"
      )) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.1335, 0.85),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.title = element_blank()
    ) +
    ggplot2::guides(fill = guide_legend(override.aes = list(alpha = 0.7 / 7, color = NA)))
  
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
      family = "sans", size = 3.75, hjust = 0.5, vjust = 19
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
        family = "sans", size = 3.75, hjust = 0.5, vjust = -0.5 * (-rank) * 3 - (-12)
      )
    }
  }

  return(list(p, plot_data, "row", "cumtmean"))
}
