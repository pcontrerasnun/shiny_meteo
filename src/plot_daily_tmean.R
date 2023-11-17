
DailyTmeanPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate percentiles of tmean across every day of the year
  reference_daily_pcts_tmean <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    # Smoothing with +- 15 days
    dplyr::mutate(rolling_tmean = round(zoo::rollapply(tmean, width = 2 * 15 + 1, FUN = mean, 
                                                 align = "center", fill = NA), 2)) |>
    dplyr::group_by(day, month) |> 
    dplyr::summarise(
      q00tmean = round(quantile(rolling_tmean, probs = 0.00, na.rm = TRUE), 1),
      q20tmean = round(quantile(rolling_tmean, probs = 0.20, na.rm = TRUE), 1),
      q40tmean = round(quantile(rolling_tmean, probs = 0.40, na.rm = TRUE), 1),
      q60tmean = round(quantile(rolling_tmean, probs = 0.60, na.rm = TRUE), 1),
      q80tmean = round(quantile(rolling_tmean, probs = 0.80, na.rm = TRUE), 1),
      q100tmean = round(quantile(rolling_tmean, probs = 1, na.rm = TRUE), 1),
      .groups = "keep" # to avoid warning
    ) |>
    # We choose 2023 as year because it doesn't have 29th Feb
    dplyr::mutate(date = as.Date(paste("2023", month, day, sep = "-"), format = "%Y-%m-%d")) |> 
    dplyr::as_tibble()
  
  # Get daily mean temperatures
  selected_year_daily_tmean <- data |>     
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
                    date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::select(day, month, tmean) |> 
    dplyr::as_tibble()
  
  # Join data
  plot_data <- dplyr::left_join(reference_daily_pcts_tmean, selected_year_daily_tmean, by = c("day", "month"))
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = date)) +
    ggplot2::geom_ribbon(aes(ymin = q80tmean, ymax = q100tmean, fill = "P100"),
                         alpha = 0.3, color = "#d7191c", linetype = "51", 
                         lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = q60tmean, ymax = q80tmean, fill = "P80"),
                         alpha = 0.1, color = "#fdae61", linetype = "51", 
                         lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = q40tmean, ymax = q60tmean, fill = "P60"),
                         alpha = 0.1, color = NA, linetype = "51", 
                         lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = q20tmean, ymax = q40tmean, fill = "P40"),
                         alpha = 0.1, color = "#abd9e9", linetype = "51", 
                         lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = q00tmean, ymax = q20tmean, fill = "P20"),
                         alpha = 0.3, color = "#2c7bb6", linetype = "51", 
                         lineend = "round", linejoin = "round") +
    ggplot2::geom_line(aes(y = tmean, color = "tmean"), linewidth = 0.75, lineend = "round", na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c("tmean" = "black"),
                                label = paste0("Daily mean temp. (", selected_year, ")"), guide = guide_legend(order = 1)) +
    ggplot2::scale_fill_manual(
      values = c("P100" = "#d7191c", "P80" = "#fdae61", "P60" = "white",
                 "P40" = "#abd9e9", "P20" = "#2c7bb6"),
      breaks = c("P100", "P80", "P60", "P40", "P20"), # To give order
      labels = c("P100" = expr(paste("Very hot day (", italic(P[80]), "-", italic(P[100]), ") (", 
                                     !!ref_start_year, "-", !!ref_end_year, ")")), 
                 "P80" = expr(paste("Hot day (", italic(P[60]), "-", italic(P[80]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")), 
                 "P60" = expr(paste("Normal day (", italic(P[40]), "-", italic(P[60]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")),
                 "P40" = expr(paste("Cold day (", italic(P[20]), "-", italic(P[40]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")),
                 "P20" = expr(paste("Very cold day (", italic(P[00]), "-", italic(P[20]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")"))),
      guide = guide_legend(override.aes = list(colour = NA), order = 2)) +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd("2023-01-01"), ymd("2023-12-31"), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd("2023-01-01")), as.numeric(ymd("2023-12-31")))
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "ºC"),
      breaks = round(seq(from = min(min(plot_data$q00tmean), min(plot_data$tmean, na.rm = TRUE)) - 2, 
                   to = max(max(plot_data$q100tmean), max(plot_data$tmean, na.rm = TRUE)) + 2, by = 5), 0),
      limits = c(min(min(plot_data$q00tmean), min(plot_data$tmean, na.rm = TRUE)) - 2, 
                 max(max(plot_data$q100tmean), max(plot_data$tmean, na.rm = TRUE)) + 2)
    ) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Temperature in Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Daily mean temperature vs. historical percentiles (", ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)")
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25), 
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.13, 0.8),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.title = ggplot2::element_blank(),
      legend.text.align = 0
    )
 
  return(p) 
}
