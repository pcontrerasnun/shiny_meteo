HighPcpDaysPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate number of days per season with more than 25mm
  season_25mm_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
                     date <= as.Date(paste0(ref_end_year, "-12-31")))) |> 
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
    dtplyr::lazy_dt() |>
    # Put year of Jan and Feb as year - 1 to be part of winter season of previous year
    dplyr::mutate(year_season = as.numeric(year) - season_aux) |> 
    dplyr::group_by(year_season, season) |> 
    dplyr::summarise(pcp25 = sum(pcp >= 25), .groups = "keep") |> # .groups to silent warning
    dplyr::as_tibble()
  
  # Calculate number of days per year with more than 25mm
  annual_25mm_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
                     date <= as.Date(paste0(ref_end_year, "-12-31")))) |> 
    dplyr::group_by(year) |> 
    dplyr::summarise(pcp25 = sum(pcp >= 25), .groups = "keep") |> # .groups to silent warning
    dplyr::ungroup() |> 
    dplyr::mutate(year_season = as.numeric(year), season = "Total") |> 
    dplyr::select(-year) |> 
    dplyr::select(year_season, season, pcp25) |> 
    dplyr::as_tibble()
  
  # Join data
  plot_data <- rbind(season_25mm_pcp, annual_25mm_pcp)
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = year_season, y = pcp25)) +
    ggplot2::geom_line(aes(color = "pcp25")) +
    ggplot2::geom_point(size = 0.75) +
    ggplot2::geom_smooth(aes(color = "trend", group = season), method = lm, se = FALSE, na.rm = TRUE, show.legend = FALSE) + 
    ggplot2::facet_wrap(~factor(season, levels = c("Winter", "Spring", "Summer", "Autumn", "Total")), ncol = 2) +
    ggplot2::scale_color_manual(
      breaks = c("pcp25", "trend"), values = c("trend" = "blue", "pcp25" = "black"), 
      labels = c("trend" = "Trend", "pcp25" = "Days with >25mm precip.")) +
    ggplot2::scale_x_continuous(breaks = seq(from = min(plot_data$year_season), 
                                             to = max(plot_data$year_season), by = 10)) +
    #ggplot2::scale_y_continuous(labels = function(x) paste0(x, "mm")) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "Days", title = paste0("Precipitation in Madrid - Retiro"),
      subtitle = paste0(
        "Number of days with more than 25mm of precipitation (", ref_start_year, "-", ref_end_year, ")"),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)"),
      color = NULL) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.085, 0.925),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      axis.text.x = element_text(angle = 45, size = 10, hjust = 1)
    ) +
    ggplot2::guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid"),
                                                             linewidth = c(0.5, 0.85))))
  
  return(p)
}