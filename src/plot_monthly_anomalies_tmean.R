MonthlyTmeanAnomaliesPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate mean temperature for each month
  reference_monthly_tmean <- data |>     
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::group_by(month) |> 
    dplyr::summarise(tmeanmean = round(mean(tmean, na.rm = TRUE), 1)) |> 
    dplyr::as_tibble()
  
  # Calculate mean temperature for each month in selected year
  selected_year_tmean <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
                    date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::group_by(month) |> 
    dplyr::summarise(tmean = round(mean(tmean, na.rm = TRUE), 1)) |> 
    dplyr::as_tibble()
  
  # Join data and calculate temperatures anomalies
  plot_data <- dplyr::left_join(reference_monthly_tmean, selected_year_tmean, by = "month") |> 
    dplyr::mutate(difftmean = round(tmean - tmeanmean, 1)) |> 
    dplyr::mutate(difftmean_label = ifelse(difftmean > 0, paste0("+", difftmean), difftmean)) |> 
    dplyr::mutate(month = as.numeric(month))
  
  # Create a bunch of point to color plot with geom_segment()
  color_data <- plot_data |> 
    dplyr::reframe(x = seq(1, 12, length = 1000),
            y1 = approx(month, tmean, xout = x)$y,
            y2 = approx(month, tmeanmean, xout = x)$y,
            diff = y1 - y2)
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = month, y = tmean, group = 1)) +
    ggplot2::geom_segment(data = color_data, aes(x = x, y = y1, xend = x, yend = y2, color = diff),
                 linewidth = 1, na.rm = TRUE) +
    ggplot2::scale_color_gradient2(high = "#d7191c", mid = "white", low = "#2c7bb6", guide = guide_none()) +
    ggplot2::geom_line(aes(linetype = "tmean"), linewidth = 0.85, lineend = "round", na.rm = TRUE) +
    ggplot2::geom_line(aes(y = tmeanmean, linetype = "tmeanmean"), na.rm = TRUE, show.legend = FALSE) +
    ggplot2::scale_linetype_manual(values = c("tmean" = "solid", "tmeanmean" = "longdash"),
                                   labels = c("tmean" = paste0("Monthly mean temp. (", selected_year, ")"),
                                              "tmeanmean" = paste0("Monthly mean temp. (", ref_start_year, 
                                                                   "-", ref_end_year, ")"))) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggrepel::geom_label_repel(aes(label = paste0(difftmean_label, "ºC")), segment.color = NA, na.rm = TRUE) +
    ggplot2::scale_x_continuous(
      breaks = seq(1, 12), 
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                 "Sep", "Oct", "Nov", "Dec")) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "ºC"), 
      limits = c(floor(min(min(plot_data$tmean, na.rm = TRUE),
                           min(plot_data$tmeanmean, na.rm = TRUE)) - 2), 
                 ceiling(max(max(plot_data$tmean, na.rm = TRUE),
                             max(plot_data$tmeanmean, na.rm = TRUE)) + 2)),
      breaks = seq(from = floor(min(min(plot_data$tmean, na.rm = TRUE),
                                    min(plot_data$tmeanmean, na.rm = TRUE)) - 2), 
                   to = ceiling(max(max(plot_data$tmean, na.rm = TRUE),
                                    max(plot_data$tmeanmean, na.rm = TRUE))) + 2, by = 5)) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Temperature in Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Monthly mean temperature anomalies vs. historical mean (",
        ref_start_year, "-", ref_end_year, ")"
      ),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)"
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.125, 0.85),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.title = element_blank()
    ) +
    ggplot2::guides(linetype = guide_legend(override.aes = list(
      linewidth = c(0.75, 0.5),
      lineend = c("square", "round")
    )))
  
  return(list(p, plot_data |> dplyr::select(month, tmean, tmeanmean, difftmean), "month", "tmean"))
}


