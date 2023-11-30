OverviewPcpTempPlot2 <- function(data_temp, data_pcp, ref_start_year, ref_end_year, selected_year, max_date) {
  # Calculate median tmean in reference period
  reference_median_tmean <- data_temp |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::group_by(year) |> 
    dplyr::summarise(tmean = round(mean(tmean, na.rm = TRUE), 1)) |> 
    dplyr::summarise(p50tmean = round(quantile(tmean, probs = 0.50, na.rm = TRUE), 1)) |> 
    dplyr::as_tibble()
  
  # Calculate mean temperature for each year
  annual_tmeans <- data_temp |>
    dtplyr::lazy_dt() |>
    dplyr::group_by(year) |> 
    dplyr::summarise(tmean = round(mean(tmean, na.rm = TRUE), 1)) |> 
    dplyr::arrange(year) |> 
    dplyr::as_tibble()
  
  # Calculate median precipitation in reference period
  reference_median_pcp <- data_pcp |>
    dtplyr::lazy_dt() |>
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
                     date <= as.Date(paste0(ref_end_year, "-12-31")))) |> 
    dplyr::group_by(year) |> 
    dplyr::summarise(pcp = sum(pcp, na.rm = TRUE)) |>
    dplyr::summarise(p50pcp = round(quantile(pcp, probs = 0.50, na.rm = TRUE), 1)) |> 
    dplyr::as_tibble()
  
  # Calculate total precipitation for each year
  annual_pcp <- data_pcp |> 
    dtplyr::lazy_dt() |>
    dplyr::group_by(year) |> 
    dplyr::summarise(pcp = sum(pcp, na.rm = TRUE)) |> 
    dplyr::arrange(year) |> 
    dplyr::as_tibble()
  
  # Join data
  plot_data <- dplyr::left_join(cbind(annual_tmeans, reference_median_tmean), 
                                cbind(annual_pcp, reference_median_pcp), by = "year") |> 
    dplyr::mutate(diffp50tmean = round(tmean - p50tmean, 1)) |> 
    dplyr::mutate(diffp50pcp = round((pcp / p50pcp) * 100, 1)) |> 
    dplyr::mutate(year = as.numeric(year))
  
  
  p <- ggplot2::ggplot(data = plot_data, aes(x = diffp50pcp, y = diffp50tmean, color = year)) +
    ggplot2::geom_vline(xintercept = 100) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_point(size = 5, na.rm = TRUE) +
    ggplot2::scale_color_gradientn(colors = wes_palette("Zissou1", 100, type = "continuous")) + 
    ggrepel::geom_label_repel(data = subset(plot_data, year == selected_year), aes(label = year), fontface = "bold") +
    ggplot2::scale_x_continuous(
      limits = c(0, 200), breaks = seq(0, 200, by = 50),
      labels = function(x) paste0(x, "%")) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "ºC"),
      limits = c(-3.5, 3.5), breaks = seq(-4, 4, by = 1)
    ) +
    ggplot2::annotate(geom = 'label', label = 'Cálido y seco', x = -Inf, y = Inf, hjust = "inward", vjust = 2, size = 4) +
    ggplot2::annotate(geom = 'label', label = 'Cálido y húmedo', x = Inf, y = Inf, hjust = "inward", vjust = 2, size = 4) +
    ggplot2::annotate(geom = 'label', label = 'Frio y seco', x = -Inf, y = -Inf, hjust = "inward", vjust = -2, size = 4) +
    ggplot2::annotate(geom = 'label', label = 'Frio y húmedo', x = Inf, y = -Inf, hjust = "inward", vjust = -2, size = 4) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "Precipitation anomaly", y = "Temperature anomaly", 
      title = paste0("Precipitation and temperature in Madrid - Retiro ", selected_year),
      subtitle = paste0("Annual precipitation and annual mean temperature vs. historical medians (", ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)"),
      color = "Year") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25), 
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.095, 0.75)
    ) +
    ggplot2::guides(color = guide_colorbar(ticks.colour = NA))

  return(p)
}

