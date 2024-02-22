AnnualPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date, title) {
  # Calculate historical annual percentiles
  reference_pcts_annual_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::group_by(year) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE)) |>
    dplyr::summarise(
      p00pcp = round(quantile(sumpcp, probs = 0.00, na.rm = TRUE), 1),
      p20pcp = round(quantile(sumpcp, probs = 0.20, na.rm = TRUE), 1),
      p40pcp = round(quantile(sumpcp, probs = 0.40, na.rm = TRUE), 1),
      p50pcp = round(quantile(sumpcp, probs = 0.50, na.rm = TRUE), 1),
      p60pcp = round(quantile(sumpcp, probs = 0.60, na.rm = TRUE), 1),
      p80pcp = round(quantile(sumpcp, probs = 0.80, na.rm = TRUE), 1),
      p100pcp = round(quantile(sumpcp, probs = 1, na.rm = TRUE), 1)
    ) |>
    dplyr::as_tibble()
  
  # Calculate total precipitation for each year
  annual_pcp <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::group_by(year) |> 
    dplyr::summarise(pcp = sum(pcp, na.rm = TRUE)) |> 
    dplyr::arrange(-pcp) |> 
    dplyr::as_tibble()
  
  # Join data
  plot_data <- cbind(annual_pcp, reference_pcts_annual_pcp) |> 
    dplyr::mutate(year = as.numeric(year)) 
  
  # Ranking data
  ranking_pcp <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::group_by(year) |> 
    dplyr::summarise(pcp = sum(pcp, na.rm = TRUE)) |> 
    dplyr::arrange(-pcp) |> 
    dplyr::mutate(ranking = row_number()) |> 
    dplyr::filter(ranking <= 3 | ranking > (n() - 3) | year == selected_year) |> 
    dplyr::as_tibble()
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = year)) +
    ggplot2::geom_ribbon(aes(ymin = 0, ymax = p00pcp), alpha = 0.3, color = "#b2182b", 
                         fill = "#b2182b", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = p00pcp, ymax = p20pcp), alpha = 0.3, color = "#ef8a62", 
                         fill = "#ef8a62", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = p20pcp, ymax = p40pcp), alpha = 0.3, color = "#fddbc7", 
                         fill = "#fddbc7", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = p40pcp, ymax = p60pcp), alpha = 0.3, color = "#f7f7f7", 
                         fill = "#f7f7f7", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = p60pcp, ymax = p80pcp), alpha = 0.3, color = "#d1e5f0", 
                         fill = "#d1e5f0", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = p80pcp, ymax = p100pcp), alpha = 0.3, color = "#67a9cf",
                         fill = "#67a9cf", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = p100pcp, ymax = p100pcp + 100), alpha = 0.3, color = "#2166ac",
                         fill = "#2166ac", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_segment(aes(x = year, xend = year, y = 0, yend = pcp, color = "sumpcp"), 
                          linewidth = 0.75, na.rm = TRUE) +
    ggplot2::geom_smooth(aes(y = pcp, color = "trend"), method = NULL, se = FALSE, na.rm = TRUE) + 
    ggplot2::geom_point(aes(y = pcp, color = "sumpcp"), na.rm = TRUE) +
    ggrepel::geom_label_repel(data = ranking_pcp, aes(y = pcp, label = paste(ranking, "º"))) +
    ggplot2::scale_color_manual(values = c("sumpcp" = "black", "trend" = "blue"),
                                label = c("sumpcp" = paste0("Annual total precip."),
                                          "trend" = "Trend"), 
                                guide = guide_legend(order = 1)) +
    ggplot2::scale_x_continuous(
      breaks = seq(from = min(plot_data$year), to = max(plot_data$year), by = 10),
      limits = c(min(plot_data$year), max(plot_data$year) + 4)
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "mm")) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$year, na.rm = TRUE), y = max(plot_data$p100pcp),
      label = paste("Extrem.~wet~(italic(max))"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$year, na.rm = TRUE), y = max(plot_data$p80pcp),
      label = paste("Very~wet~(italic(P)[80])"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$year, na.rm = TRUE), y = max(plot_data$p60pcp),
      label = paste("Wet~(italic(P)[60])"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$year, na.rm = TRUE), y = max(plot_data$p50pcp),
      label = paste("Normal~(italic(P)[50])"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$year, na.rm = TRUE), y = max(plot_data$p40pcp),
      label = paste("Dry~(italic(P)[40])"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$year, na.rm = TRUE), y = max(plot_data$p20pcp),
      label = paste("Very~dry~(italic(P)[20])"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$year, na.rm = TRUE), y = max(plot_data$p00pcp),
      label = paste("Extrem.~dry~(italic(min))"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation in ", title, " ", selected_year),
      subtitle = paste0("Annual precipitation vs. historical percentiles (", ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/"),
      color = NULL, fill = NULL
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.125, 0.85),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.title = ggplot2::element_blank()
    )
  
  return(list(p, plot_data, "year", "pcp"))
  
}

