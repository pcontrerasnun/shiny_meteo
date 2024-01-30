AnnualPcpAnomaliesPlot <- function(data, ref_start_year, ref_end_year, max_date, title) {
  # Calculate median precipitation in reference period
  reference_annual_pcts_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
                     date <= as.Date(paste0(ref_end_year, "-12-31")))) |> 
    dplyr::group_by(year) |> 
    dplyr::summarise(pcp = sum(pcp, na.rm = TRUE)) |>
    dplyr::summarise(p50pcp = round(quantile(pcp, probs = 0.50, na.rm = TRUE), 1)) |> 
    dplyr::as_tibble()
  
  # Calculate total precipitation for each year
  annual_pcp <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::group_by(year) |> 
    dplyr::summarise(pcp = sum(pcp, na.rm = TRUE)) |> 
    dplyr::as_tibble()
  
  # Join data
  plot_data <- cbind(annual_pcp, reference_annual_pcts_pcp) |> 
    dplyr::mutate(diffmedian = round(pcp - p50pcp, 1)) |> 
    dplyr::mutate(year = as.numeric(year)) |> 
    dplyr::arrange(-diffmedian)
  
  # For geom_segment coloring purposes
  color_data <- plot_data |> 
    dplyr::reframe(x = seq(min(plot_data$year), max(plot_data$year), length = 1000),
                   y1 = approx(year, pcp, xout = x)$y,
                   y2 = approx(year, p50pcp, xout = x)$y,
                   diff = y1 - y2)
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = year, y = pcp)) +
    ggplot2::geom_segment(data = color_data, aes(x = x, y = y1, xend = x, yend = y2, color = diff),
                          linewidth = 1, na.rm = TRUE) +
    ggplot2::scale_color_gradient2(high = "#2c7bb6", mid = "white", low = "#d7191c", guide = guide_none()) +
    ggplot2::geom_line(aes(linetype = "pcp"), linewidth = 0.75, lineend = "round", linejoin = "round") +
    ggplot2::geom_line(aes(y = p50pcp, linetype = "p50")) +
    ggplot2::geom_smooth(aes(linetype = "trend"), color = "blue", linewidth = 0.85,
                         method = lm, se = FALSE, na.rm = TRUE, show.legend = FALSE) + 
    ggplot2::scale_linetype_manual(values = c("p50" = "longdash", "trend" = "dotted", "pcp" = "solid"),
                                   labels = c("p50" = paste0("Annual normal precip. (", ref_start_year, "-", ref_end_year, ")"),
                                              "trend" = paste0("Trend (", ref_start_year, "-", ref_end_year, ")"), 
                                              "pcp" = "Annual total precip.")) +
    ggrepel::geom_label_repel(data = rbind(head(plot_data, 3), tail(plot_data, 3)),
                              aes(y = pcp, label = paste0(ifelse(diffmedian > 0, "+", ""), diffmedian, "mm"))) +
    ggplot2::scale_x_continuous(breaks = seq(from = min(plot_data$year), to = max(plot_data$year), by = 5)) +
    ggplot2::scale_y_continuous(limits = c(min(plot_data$pcp) - 50, max(plot_data$pcp) + 50),
                                breaks = seq(from = round(min(plot_data$pcp) - 50, digits = -1), 
                                             to = round(max(plot_data$pcp) + 50, digits = -1), by = 100),
                                labels = function(x) paste0(x, "mm")) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation in ", title),
      subtitle = paste0(
        "Annual total precipitation anomalies vs. historical median (",
        ref_start_year, "-", ref_end_year, ")"
      ),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/"
      )
    ) +
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
    ggplot2::guides(linetype = guide_legend(override.aes = list(
      color = c("black", "black", "blue"),
      linewidth = c(0.5, 0.5, 0.85))))
  
  return(list(p, plot_data, "year", "pcp"))
  
}
