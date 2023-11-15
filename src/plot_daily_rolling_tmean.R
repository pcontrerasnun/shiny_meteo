DailyRollingTmeanPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate mean rolling temperature percentiles
  reference_daily_rolling_tmean <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
      date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
      date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::group_by(year) |>
    dplyr::mutate(tmean_acumulated = round(cumsum(tmean) / row_number(), 1)) |> 
    dplyr::group_by(day, month) |>
    dplyr::summarise(
      cumq00tmean = round(quantile(tmean_acumulated, probs = 0.00, na.rm = TRUE), 1),
      cumq05tmean = round(quantile(tmean_acumulated, probs = 0.05, na.rm = TRUE), 1),
      cumq20tmean = round(quantile(tmean_acumulated, probs = 0.20, na.rm = TRUE), 1),
      cumq40tmean = round(quantile(tmean_acumulated, probs = 0.40, na.rm = TRUE), 1),
      cumq50tmean = round(quantile(tmean_acumulated, probs = 0.50, na.rm = TRUE), 1),
      cumq60tmean = round(quantile(tmean_acumulated, probs = 0.60, na.rm = TRUE), 1),
      cumq80tmean = round(quantile(tmean_acumulated, probs = 0.80, na.rm = TRUE), 1),
      cumq95tmean = round(quantile(tmean_acumulated, probs = 0.95, na.rm = TRUE), 1),
      cumq100tmean = round(quantile(tmean_acumulated, probs = 1, na.rm = TRUE), 1),
      .groups = "keep"
    ) |>
    dplyr::as_tibble()

  # Calculate rolling mean temperature for selected year
  selected_year_rolling_tmean <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
      date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::group_by(year) |>
    dplyr::mutate(tmean_acumulated = round(cumsum(tmean) / row_number(), 1)) |>
    dplyr::as_tibble()

  # Join data
  plot_data <- left_join(reference_daily_rolling_tmean, selected_year_rolling_tmean, by = c("day", "month")) |>
    dplyr::select(
      day, month, tmean_acumulated, cumq00tmean, cumq05tmean, cumq20tmean, cumq40tmean,
      cumq50tmean, cumq60tmean, cumq80tmean, cumq95tmean, cumq100tmean
    ) |>
    dplyr::mutate(date = as.Date(paste0(day, "-", month, "2023"), format = "%d-%m%Y")) # We choose
  # 2023 since it doesn't have 29th Feb, it doesn't matter what year we choose but it can't be
  # a leap year

  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = date, y = tmean_acumulated)) +
    ggplot2::geom_ribbon(aes(ymin = cumq80tmean, ymax = cumq100tmean, fill = "Very hot"),
      alpha = 0.3, color = "#d7191c", linetype = "51", lineend = "round", linejoin = "round"
    ) +
    ggplot2::geom_ribbon(aes(ymin = cumq60tmean, ymax = cumq80tmean, fill = "Hot"),
      alpha = 0.1, color = "#fdae61", linetype = "51", lineend = "round", linejoin = "round"
    ) +
    ggplot2::geom_ribbon(aes(ymin = cumq40tmean, ymax = cumq60tmean, fill = "Normal"),
      alpha = 0.1, linetype = "51", lineend = "round", linejoin = "round"
    ) +
    ggplot2::geom_ribbon(aes(ymin = cumq20tmean, ymax = cumq40tmean, fill = "Cold"),
      alpha = 0.1, color = "#abd9e9", linetype = "51", lineend = "round", linejoin = "round"
    ) +
    ggplot2::geom_ribbon(aes(ymin = cumq00tmean, ymax = cumq20tmean, fill = "Very cold"),
      alpha = 0.3, color = "#2c7bb6", linetype = "51", lineend = "round", linejoin = "round"
    ) +
    ggplot2::geom_line(aes(color = "selected_year"), linewidth = 0.85, lineend = "round", na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c("selected_year" = "black"),
                                label = paste0("Daily mean temp. ", selected_year)) +
    ggplot2::scale_fill_manual(
      values = c("Very hot" = "#d7191c", "Hot" = "#fdae61", "Normal" = "white", "Cold" = "#abd9e9", "Very cold" = "#2c7bb6"), 
      breaks = c("Very hot", "Hot", "Normal", "Cold", "Very cold"), # To give order,
      labels = c("Very hot" = expr(paste("Very hot (", italic(P[80]), " - ", italic(P[100]), ") (", 
                                               !!ref_start_year, " - ", !!ref_end_year, ")")), 
                 "Hot" = expr(paste("Hot (", italic(P[60]), " - ", italic(P[80]), ") (", 
                                          !!ref_start_year, " - ", !!ref_end_year, ")")), 
                 "Normal" = expr(paste("Normal (", italic(P[40]), " - ", italic(P[60]), ") (", 
                                             !!ref_start_year, " - ", !!ref_end_year, ")")),
                 "Cold" = expr(paste("Cold (", italic(P[20]), " - ", italic(P[40]), ") (", 
                                           !!ref_start_year, " - ", !!ref_end_year, ")")),
                 "Very cold" = expr(paste("Very cold (", italic(P[00]), " - ", italic(P[20]), ") (", 
                                                !!ref_start_year, " - ", !!ref_end_year, ")"))),
      guide = guide_legend(override.aes = list(colour = NA))
    ) +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd("2023-01-01"), ymd("2023-12-31"), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd("2023-01-01")), as.numeric(ymd("2024-01-09"))),
      expand = expansion(mult = c(0.02, 0))
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "ºC"),
      limits = c(
        floor(min(
          min(plot_data$tmean_acumulated, na.rm = TRUE),
          min(plot_data$cumq00tmean, na.rm = TRUE)
        ) - 2),
        ceiling(max(
          max(plot_data$tmean_acumulated, na.rm = TRUE),
          max(plot_data$cumq100tmean, na.rm = TRUE)
        ) + 2)
      ),
      breaks = seq(
        from = floor(min(
          min(plot_data$tmean_acumulated, na.rm = TRUE),
          min(plot_data$cumq00tmean, na.rm = TRUE)
        ) - 2),
        to = ceiling(max(
          max(plot_data$tmean_acumulated, na.rm = TRUE),
          max(plot_data$cumq100tmean, na.rm = TRUE)
        )) + 2, by = 2
      )
    ) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Temperature in Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Rolling daily mean temperature vs. historical percentiles (",
        ref_start_year, " - ", ref_end_year, ")"
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
      legend.position = c(0.075, 0.85),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.title = element_blank(),
      legend.text.align = 0
    )

  return(p)
}
