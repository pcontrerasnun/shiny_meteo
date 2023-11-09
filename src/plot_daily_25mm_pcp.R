HighPcpDaysPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate number of days per season with more than 25mm
  reference_yearly_25mm_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
                     date <= as.Date(paste0(ref_end_year, "-12-31"))) |
                    (date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
                       date <= as.Date(paste0(as.numeric(selected_year), "-12-31")))) |> # Also include year of study
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
    dplyr::summarise(pcp_25 = sum(pcp >= 25), .groups = "keep") |> # .groups to silent warning
    dplyr::as_tibble()
  
  # Draw the plot
  p <- ggplot2::ggplot(data = reference_yearly_25mm_pcp, aes(x = year_season, y = pcp_25)) +
    ggplot2::geom_col(aes(fill =  season), position = "stack") +
    ggplot2::geom_smooth(aes(group = season, color = season), method = lm, se = FALSE, na.rm = TRUE) +
    ggplot2::scale_color_manual(
      values = c(c("#ff7f00", "#b2df8a", "#e31a1c", "#a6cee3"))) +
    ggplot2::scale_fill_manual(
      values = c(c("#ff7f00", "#b2df8a", "#e31a1c", "#a6cee3"))) +
    ggplot2::scale_x_continuous(
      breaks = seq(from = min(reference_yearly_25mm_pcp$year_season), 
                   to = max(reference_yearly_25mm_pcp$year_season), by = 1),
      expand = c(0, 2, 0, 2)) +
    ggplot2::scale_y_continuous(
      breaks = seq(
        from = 0, to = max(reference_yearly_25mm_pcp |> dplyr::group_by(year_season) |> 
                             dplyr::summarise(sum = sum(pcp_25)) |> dplyr::select(sum), na.rm = TRUE), 
        by = 1
      )) + 
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation in Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Number of days with more than 25mm of precipitation (",
        ref_start_year, "-", ref_end_year, ")"
      ),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)"
      ),
      color = NULL, fill = NULL) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.05, 0.85),
      legend.justification = c(0.05, 0.85),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.text = ggtext::element_markdown(),
      axis.text.x = element_text(angle = 45, size = 10, hjust = 1)
    )
  
  return(p)
}