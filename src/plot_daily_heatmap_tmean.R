DailyHeatmapTmeanPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate daily smoothed mean and empirical distribution for each day
  reference_daily_pcts_tmean <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    # Smoothing with +- 15 days
    dplyr::mutate(rolling_tmean = round(zoo::rollapply(tmean, width = 2 * 15 + 1, FUN = mean, 
                                                       align = "center", fill = NA), 1)) |>
    dplyr::group_by(day, month) |> 
    dplyr::summarise(ecdf_func = list(ecdf(rolling_tmean)), .groups = "keep") |> 
    dplyr::as_tibble()
  
  # Get daily mean temperatures for year of study
  selected_year_daily_tmean <- data |>     
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
                    date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::select(day, month, tmean) |> 
    dplyr::as_tibble()
  
  # Join data and see where mean daily temp of year of study falls in emprirical distribution
  plot_data <- dplyr::left_join(reference_daily_pcts_tmean, selected_year_daily_tmean, by = c("day", "month")) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(percentile = round(ecdf_func(tmean), 2) * 100) |> 
    dplyr::mutate(pct_cut = cut(percentile, breaks = c(-0.1, 5, 20, 40, 60, 80, 95, 100),
                                labels = c("#2166ac", "#67a9cf", "#d1e5f0", "#f7f7f7", "#fddbc7", "#ef8a62", "#b2182b")))
  
  # Draw the plot
  p <- ggplot2::ggplot(plot_data, aes(x = day, y = month, fill = pct_cut)) +
    ggplot2::geom_tile(color = "white", size = 0.1) +
    ggplot2::geom_text(aes(label = percentile), vjust = 1, fontface = "bold", na.rm = TRUE) +
    ggplot2::scale_y_discrete(
      limits = rev,
      labels = rev(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    ) +
    ggplot2::scale_fill_identity(guide = "legend", labels = c(0, 5, 20, 40, 60, 80, 95, 100)) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Temperature in Madrid - Retiro ", selected_year),
      subtitle = paste0("Daily mean temperature vs. empirical distribution (", ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)"),
      color = NULL, fill = NULL
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.spacing.x = ggplot2::unit(0, "cm"),
      legend.key.size = unit(3,"line"),
      legend.title = ggplot2::element_text(family = "sans")
    ) +
    guides(fill = guide_legend(nrow = 1, label.position = "bottom", label.hjust = 0,
                               title = "Percentile", title.position = "top"))
    
  return(p)
}


