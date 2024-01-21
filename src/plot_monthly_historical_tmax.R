MonthlyHistoricalTmaxPlot <- function(data, ref_start_year, ref_end_year, max_date) {
  # Calculate max temp for each month and year
  plot_data <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
                     date <= as.Date(paste0(ref_end_year, "-12-31")))) |> # Only ref period data
    dplyr::group_by(year, month) |> 
    dplyr::summarise(maxtmax = max(tmax, na.rm = TRUE), .groups = "keep") |> 
    dplyr::filter(maxtmax != -Inf) |> 
    dplyr::as_tibble()
  
  ranking_maxtmaxs <- plot_data |> 
    dtplyr::lazy_dt() |>
    dplyr::arrange(-maxtmax) |>
    dplyr::group_by(month) |> 
    dplyr::mutate(ranking = rank(-maxtmax, ties.method = "first")) |>
    dplyr::slice_head(n = 3) |>
    dplyr::as_tibble()
  
  ranking_mintmaxs <- plot_data |> 
    dtplyr::lazy_dt() |>
    dplyr::arrange(-maxtmax) |> 
    dplyr::group_by(month) |> 
    dplyr::mutate(ranking = rank(maxtmax, ties.method = "last")) |>
    dplyr::slice_tail(n = 3) |>
    dplyr::as_tibble()
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = year, y = maxtmax)) +
    ggplot2::geom_line(aes(color = "maxtmax")) +
    ggplot2::geom_smooth(aes(color = "trend", group = month), linetype = "solid", linewidth = 0.5,
                         method = lm, se = FALSE, na.rm = TRUE) + 
    ggplot2::geom_point(data = ranking_maxtmaxs, color = "#b2182b") +
    ggplot2::geom_point(data = ranking_mintmaxs, color = "#2166ac") +
    ggrepel::geom_label_repel(data = ranking_maxtmaxs, aes(label = paste0(ranking, 'º (', year, ')')), size = 2.5) +
    ggrepel::geom_label_repel(data = ranking_mintmaxs, aes(label = paste0(ranking, 'º (', year, ')')), size = 2.5) +
    ggplot2::facet_wrap(
      vars(month), 
      labeller = labeller(month = c("01" = "Jan", "02" = "Feb", "03" = "Mar", "04" = "Apr", 
                                    "05" = "May", "06" = "Jun", "07" = "Jul", "08" = "Aug", 
                                    "09" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec"))) +
    ggplot2::scale_color_manual(
      values = c("trend" = "blue", "maxtmax" = "black"), 
      labels = c("trend" = paste0("Trend (", ref_start_year, "-", ref_end_year, ")"), 
                 "maxtmax" = "Monthly max temp."),
      breaks = c("maxtmax", "trend")) +
    ggplot2::scale_x_continuous(breaks = seq(from = min(plot_data$year), to = max(plot_data$year), by = 10)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "ºC")) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = "Temperature in Madrid - Retiro",
      subtitle = paste0("Historical monthly max temperature (", ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/"),
      color = NULL, fill = NULL) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(
        fill = "white", color = "black",
        linewidth = 0.75
      ),
      legend.position = c(0.075, 0.925),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.text = ggtext::element_markdown(),
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, size = 10)
    )
  
  return(list(p, plot_data, "year", "maxtmax"))

}
