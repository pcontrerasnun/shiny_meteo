MonthlyAnomaliesPcpPlot <- function(data, ref_start_year, ref_end_year, max_date) {
  # Calculate percentiles of total precip. per month in reference period
  reference_monthly_pcts_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::group_by(year, month) |> 
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE), .groups = "keep") |> # .groups to avoid warnings
    dplyr::group_by(month) |> 
    dplyr::summarise(
      q00pcp = round(quantile(sumpcp, probs = 0.00, na.rm = TRUE), 1),
      q05pcp = round(quantile(sumpcp, probs = 0.05, na.rm = TRUE), 1),
      q20pcp = round(quantile(sumpcp, probs = 0.20, na.rm = TRUE), 1),
      q40pcp = round(quantile(sumpcp, probs = 0.40, na.rm = TRUE), 1),
      q50pcp = round(quantile(sumpcp, probs = 0.50, na.rm = TRUE), 1),
      q60pcp = round(quantile(sumpcp, probs = 0.60, na.rm = TRUE), 1),
      q80pcp = round(quantile(sumpcp, probs = 0.80, na.rm = TRUE), 1),
      q95pcp = round(quantile(sumpcp, probs = 0.95, na.rm = TRUE), 1),
      q100pcp = round(quantile(sumpcp, probs = 1, na.rm = TRUE), 1),
      .groups = "keep" # .groups to avoid warnings
    ) |> 
    dplyr::as_tibble()
  
  # Calculate total precip. per month in all history
  all_monthly_pcp <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::group_by(year, month) |> 
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE), .groups = "keep") |> # .groups to avoid warnings
    dplyr::as_tibble()
  
  # Join data and calculate anomalies
  plot_data <- dplyr::left_join(all_monthly_pcp, reference_monthly_pcts_pcp, by = "month") |> 
    mutate(diffmedian = round(sumpcp - q50pcp, 1)) |> 
    dplyr::mutate(year = as.numeric(year))
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = year, y = sumpcp)) +
    ggplot2::geom_line(aes(color = "sumpcp"), show.legend = FALSE) +
    ggplot2::geom_line(aes(y = q50pcp, color = "q50"), linetype = "dashed") +
    ggplot2::geom_smooth(aes(color = "trend", group = month), method = lm, se = FALSE, na.rm = TRUE, show.legend = FALSE) + 
    ggplot2::facet_wrap(vars(month), labeller = labeller(month = c("01" = "Jan", "02" = "Feb", "03" = "Mar", 
                                                                   "04" = "Apr", "05" = "May", "06" = "Jun", 
                                                                   "07" = "Jul", "08" = "Aug", "09" = "Sep", 
                                                                   "10" = "Oct", "11" = "Nov", "12" = "Dec"))) +
    ggplot2::scale_color_manual(
      breaks = c("sumpcp", "q50", "trend"),
      values = c("trend" = "blue", "q50" = "black", "sumpcp" = "black"), 
      labels = c("trend" = "Trend", "sumpcp" = "Monthly precip.",
                 "q50" = paste0("Monthly median precip. (", ref_start_year, "-", ref_end_year, ")"))) +
    ggplot2::scale_x_continuous(breaks = seq(from = min(plot_data$year), to = max(plot_data$year), by = 10)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "mm")) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = "Precipitation in Madrid - Retiro",
      subtitle = paste0("Historical monthly precipitation vs. historical median (",
                        ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)"),
      color = NULL, fill = NULL) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(
        fill = "white", color = "black",
        linewidth = 0.75
      ),
      legend.position = c(0.58, 0.59),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.text = ggtext::element_markdown(),
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, size = 10)
    ) +
    ggplot2::guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed", "solid"),
                                                             linewidth = c(0.5, 0.5, 0.85))))

  return(p)  
}
