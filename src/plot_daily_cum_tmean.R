DailyCumulativeTmeanPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date, title) {
  # Calculate mean cumulative temperature percentiles
  reference_daily_cum_tmean <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
      date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
      date > as.Date(paste0(selected_year, "-12-31")))) |> 
    dplyr::group_by(year) |>
    dplyr::mutate(cumtmean = round(replace(tmean, complete.cases(tmean), cummean(na.omit(tmean))), 1)) |>
    tidyr::fill(cumtmean) |> 
    dplyr::group_by(day, month) |>
    dplyr::summarise(
      cump00tmean = round(quantile(cumtmean, probs = 0.00, na.rm = TRUE), 1),
      cump20tmean = round(quantile(cumtmean, probs = 0.20, na.rm = TRUE), 1),
      cump40tmean = round(quantile(cumtmean, probs = 0.40, na.rm = TRUE), 1),
      cump50tmean = round(quantile(cumtmean, probs = 0.50, na.rm = TRUE), 1),
      cump60tmean = round(quantile(cumtmean, probs = 0.60, na.rm = TRUE), 1),
      cump80tmean = round(quantile(cumtmean, probs = 0.80, na.rm = TRUE), 1),
      cump100tmean = round(quantile(cumtmean, probs = 1, na.rm = TRUE), 1),
      .groups = "keep"
    ) |>
    dplyr::as_tibble()

  # Calculate cumulative mean temperature for selected year
  selected_year_cum_tmean <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
      date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::filter(complete.cases(tmean)) |> 
    dplyr::group_by(year) |>
    dplyr::mutate(cumtmean = round(replace(tmean, complete.cases(tmean), cummean(na.omit(tmean))), 1)) |>
    tidyr::fill(cumtmean) |> 
    dplyr::as_tibble()

  # Join data
  plot_data <- left_join(reference_daily_cum_tmean, selected_year_cum_tmean, by = c("day", "month")) |>
    dplyr::select(
      day, month, cumtmean, cump00tmean, cump20tmean, cump40tmean,
      cump50tmean, cump60tmean, cump80tmean, cump100tmean
    ) |>
    dplyr::mutate(diffmedian = round(cumtmean - cump50tmean, 1)) |> 
    dplyr::mutate(date = as.Date(paste0(day, "-", month, selected_year), format = "%d-%m%Y")) |> 
    dplyr::arrange(date) |> 
    dplyr::select(date, everything(), -c(day, month))
  
  # For labelling anomaly purposes
  if (tail(na.omit(plot_data), 1)$diffmedian < 0){
    sign <- ""
  } else {
    sign <- "+"
  }

  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = date, y = cumtmean)) +
    ggplot2::annotate("rect", ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.2,
                      xmin = seq(ymd(paste0(selected_year, "-02-01")), by = "2 month", length = 6),
                      xmax = seq(ymd(paste0(selected_year, "-03-01")), by = "2 month", length = 6)) +
    ggplot2::geom_ribbon(aes(ymin = cump100tmean, ymax = cump100tmean + 1.5, fill = "Extrem. hot"),
      alpha = 0.3, color = "#b2182b", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = cump80tmean, ymax = cump100tmean, fill = "Very hot"),
      alpha = 0.3, color = "#ef8a62", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = cump60tmean, ymax = cump80tmean, fill = "Hot"),
      alpha = 0.3, color = "#fddbc7", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = cump40tmean, ymax = cump60tmean, fill = "Normal"),
      alpha = 0.3, linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = cump20tmean, ymax = cump40tmean, fill = "Cold"),
      alpha = 0.3, color = "#d1e5f0", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = cump00tmean, ymax = cump20tmean, fill = "Very cold"),
      alpha = 0.3, color = "#67a9cf", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = cump00tmean, ymax = cump00tmean - 1.5, fill = "Extrem. cold"),
      alpha = 0.3, color = "#2166ac", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_line(aes(color = "selected_year"), linewidth = 0.85, lineend = "round", na.rm = TRUE) +
    ggrepel::geom_label_repel(
      data = tail(na.omit(plot_data), 1), 
      aes(label = paste0("atop(Cumulative~mean~temp.~", cumtmean, "*ºC,", sign, diffmedian, "*ºC~vs.~italic(P[50]))")),
      parse = TRUE) +
    ggplot2::scale_color_manual(values = c("selected_year" = "black"),
                                label = paste0("Daily cumulative mean temp. (", selected_year, ")"), 
                                guide = guide_legend(order = 1)) +
    ggplot2::scale_fill_manual(
      values = c("Extrem. hot" = "#b2182b", "Very hot" = "#ef8a62", "Hot" = "#fddbc7", 
                 "Normal" = "#f7f7f7", "Cold" = "#d1e5f0", "Very cold" = "#67a9cf", "Extrem. cold" = "#2166ac"), 
      breaks = c("Extrem. hot", "Very hot", "Hot", "Normal", "Cold", "Very cold", "Extrem. cold"), # To give order,
      labels = c(
        "Extrem. hot" = expr(paste("Extrem. hot (>", italic(P[100]), ") (", 
                                !!ref_start_year, "-", !!ref_end_year, ")")), 
        "Very hot" = expr(paste("Very hot (", italic(P[80]), "-", italic(P[100]), ") (", 
                                !!ref_start_year, "-", !!ref_end_year, ")")), 
        "Hot" = expr(paste("Hot (", italic(P[60]), "-", italic(P[80]), ") (", 
                           !!ref_start_year, "-", !!ref_end_year, ")")), 
        "Normal" = expr(paste("Normal (", italic(P[40]), "-", italic(P[60]), ") (", 
                              !!ref_start_year, "-", !!ref_end_year, ")")),
        "Cold" = expr(paste("Cold (", italic(P[20]), "-", italic(P[40]), ") (", 
                            !!ref_start_year, "-", !!ref_end_year, ")")),
        "Very cold" = expr(paste("Very cold (", italic(P[00]), "-", italic(P[20]), ") (", 
                                 !!ref_start_year, "-", !!ref_end_year, ")")),
        "Extrem. cold" = expr(paste("Extrem. cold (<", italic(P[00]), ") (", 
                                 !!ref_start_year, "-", !!ref_end_year, ")"))),
      guide = guide_legend(override.aes = list(colour = NA), guide = guide_legend(order = 2))
    ) +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd(paste0(selected_year, "-01-01")), 
                              ymd(paste0(selected_year, "-12-31")), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd(paste0(selected_year, "-01-01"))), 
                 as.numeric(ymd(paste0(as.numeric(selected_year) + 1), "-01-01"))),
      # expand = expansion(mult = c(0.02, 0))
      ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "ºC"),
      limits = c(floor(min(min(plot_data$cumtmean, na.rm = TRUE),
                           min(plot_data$cump00tmean, na.rm = TRUE)) - 2),
                 ceiling(max(max(plot_data$cumtmean, na.rm = TRUE),
                             max(plot_data$cump100tmean, na.rm = TRUE)) + 2)),
      breaks = round(seq(from = floor(min(min(plot_data$cumtmean, na.rm = TRUE),
                                    min(plot_data$cump00tmean, na.rm = TRUE)) - 2),
                   to = ceiling(max(max(plot_data$cumtmean, na.rm = TRUE),
                                    max(plot_data$cump100tmean, na.rm = TRUE))) + 2, by = 2) / 5) * 5) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Temperature in ", title, " ", selected_year),
      subtitle = paste0("Cumulative daily mean temperature vs. historical percentiles (",
                        ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/")) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.85, 0.2),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.title = element_blank(),
      legend.text.align = 0) +
    ggplot2::guides(fill = guide_legend(override.aes = list(alpha = 0.7 / 7, color = NA)))

  return(list(p, plot_data, "date", "cumtmean"))
}
