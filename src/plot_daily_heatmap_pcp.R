DailyHeatmapPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate daily smoothed empirical distribution for each day
  reference_daily_pcts_pcp <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::as_tibble() |> 
    # Get previous and next 15 days
    dplyr::reframe(ref_date = seq.Date(date - 15, date + 15, "day"), .by = c(date, pcp)) |>
    dplyr::left_join(data |> rename(climate_pcp = pcp), join_by(ref_date == date)) |> 
    dplyr::select(-ref_date) |> 
    dplyr::mutate(day = format(date, "%d"), month = format(date, "%m")) |> 
    dplyr::group_by(day, month) |> 
    dplyr::filter(climate_pcp != 0) |> # Filter pcp different to 0
    dplyr::filter(!is.na(day)) |> # Filter NA days so that ecdf() doesn't return Error
    # Calculate empirical distribution
    dplyr::summarise(ecdf_func = list(ecdf(climate_pcp)), .groups = "keep")
  
  # Get daily precip. for year of study
  selected_year_daily_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
                    date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::select(day, month, pcp) |> 
    dplyr::filter(pcp != 0) |>
    dplyr::as_tibble()
  
  # Join data and see where mean daily precip. of year of study falls in emprirical distribution
  plot_data <- dplyr::left_join(reference_daily_pcts_pcp, selected_year_daily_pcp, by = c("day", "month")) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(percentile = round(ecdf_func(pcp), 2) * 100) |>
    dplyr::arrange(month, day) 
  
  # Draw the plot
  p <- ggplot2::ggplot(plot_data, aes(x = day, y = month, fill = percentile)) +
    ggplot2::geom_tile(color = "white", size = 0.1) +
    ggplot2::geom_text(aes(label = pcp), color = "white", vjust = 1, fontface = "bold", na.rm = TRUE) +
    ggplot2::scale_y_discrete(
      limits = rev, # to put labels in reverse order (Jan -> Dec instead of Dec -> Jan)
      labels = rev(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    ) +
    ggplot2::scale_fill_stepsn(
      guide = guide_colorsteps(
        show.limits = FALSE, even.steps = FALSE,
        label.position = "bottom", label.hjust = 0,
        title = "Percentile", title.position = "top",
        barwidth = unit(15, "cm")
      ),
      breaks = c(0, 25, 50, 75, 85, 90, 95, 100),
      labels = c(0, 25, 50, 75, 90, 95, 99, 100),
      colors = c("#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#4d004b"),
      limits = c(0, 100),
      na.value = "transparent") +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation in Madrid - Retiro ", selected_year),
      subtitle = paste0("Daily precipitation vs. empirical distribution (", ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/"),
      color = NULL, fill = NULL
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.spacing.x = ggplot2::unit(0, "cm"),
      legend.key.size = unit(3, "line"),
      legend.title = ggplot2::element_text(family = "sans")
    ) 
  #guides(fill = guide_legend(nrow = 1, label.position = "bottom", label.hjust = 0,
  #                           title = "Percentile", title.position = "top"))
  
  return(list(p, plot_data |> dplyr::select(day, month, pcp, percentile), "day", "month"))
}
