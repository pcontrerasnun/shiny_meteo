MonthlyRankingTmeanPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate historical monthly percentiles
  reference_pcts_monthly_tmean <- data_temp |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::group_by(year, month) |>
    dplyr::summarise(tmean = mean(tmean, na.rm = TRUE), .groups = "keep") |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      p00tmean = round(quantile(tmean, probs = 0.00, na.rm = TRUE), 1),
      p20tmean = round(quantile(tmean, probs = 0.20, na.rm = TRUE), 1),
      p40tmean = round(quantile(tmean, probs = 0.40, na.rm = TRUE), 1),
      p50tmean = round(quantile(tmean, probs = 0.50, na.rm = TRUE), 1),
      p60tmean = round(quantile(tmean, probs = 0.60, na.rm = TRUE), 1),
      p80tmean = round(quantile(tmean, probs = 0.80, na.rm = TRUE), 1),
      p100tmean = round(quantile(tmean, probs = 1, na.rm = TRUE), 1)
    ) |>
    dplyr::as_tibble()
  
  # Calculate each month's rank
  reference_monthly_rank_tmean <- data_temp |>
    dtplyr::lazy_dt() |>
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
                     date <= as.Date(paste0(ref_end_year, "-12-31"))) |
                    (date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
                       date <= as.Date(paste0(as.numeric(selected_year), "-12-31")))) |> # Include year of study
    dplyr::group_by(year, month) |>
    dplyr::summarise(tmean = round(mean(tmean, na.rm = TRUE), 1), .groups = "keep") |>
    dplyr::ungroup() |>
    dplyr::group_by(month) |>
    dplyr::mutate(ranking = rank(-tmean, ties.method = "first")) |>
    dplyr::arrange(month, ranking) |>
    dplyr::as_tibble()
  
  # Calculate mean temperature in each month for selected year
  selected_year_monthly_tmean <- data_temp |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
                    date <= as.Date(paste0(as.numeric(selected_year), "-12-31"))) |>
    dplyr::group_by(month) |>
    dplyr::summarise(tmean = round(mean(tmean, na.rm = TRUE), 1)) |>
    dplyr::arrange(month) |>
    dplyr::ungroup() |>
    dplyr::as_tibble()
  
  # Join data
  plot_data <- dplyr::left_join(reference_pcts_monthly_tmean, selected_year_monthly_tmean, by = "month")
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = month)) +
    #ggh4x::geom_box(aes(ymin = 0, ymax = p00tmean, fill = "P00", width = 0.9), alpha = 0.5) +
    ggh4x::geom_box(aes(ymin = p00tmean, ymax = p20tmean, fill = "P20", width = 0.9), alpha = 0.5) +
    ggh4x::geom_box(aes(ymin = p20tmean, ymax = p40tmean, fill = "P40", width = 0.9), alpha = 0.5) +
    ggh4x::geom_box(aes(ymin = p40tmean, ymax = p60tmean, fill = "P60", width = 0.9), alpha = 0.5) +
    ggh4x::geom_box(aes(ymin = p60tmean, ymax = p80tmean, fill = "P80", width = 0.9), alpha = 0.5) +
    ggh4x::geom_box(aes(ymin = p80tmean, ymax = p100tmean, fill = "P100", width = 0.9), alpha = 0.5) +
    ggplot2::geom_segment(aes(x = month, xend = month, y = 0, yend = tmean, color = "tmean"), na.rm = TRUE) +
    ggplot2::geom_point(aes(y = tmean, color = "tmean"), na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c("tmean" = "red"),
                                label = paste0("Monthly mean temp. (", selected_year, ")"), 
                                guide = guide_legend(order = 1)) +
    ggplot2::scale_fill_manual(
      values = c("P40" = "#abd9e9", "P20" = "#e0f3f8", "P60" = "white", "P80" = "#fee090", 
                 "P100" = "#fdae61"), 
      labels = c("P100" = expr(paste("Very hot month (", italic(P[80]), "-", italic(P[100]), ") (", 
                                     !!ref_start_year, "-", !!ref_end_year, ")")), 
                 "P80" = expr(paste("Hot month (", italic(P[60]), "-", italic(P[80]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")), 
                 "P60" = expr(paste("Normal month (", italic(P[40]), "-", italic(P[60]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")),
                 "P40" = expr(paste("Cold month (", italic(P[20]), "-", italic(P[40]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")),
                 "P20" = expr(paste("Very cold month (", italic(P[00]), "-", italic(P[20]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")"))),
      breaks = c("P100", "P80", "P60", "P40", "P20", "P00")) + # to give order
    ggplot2::scale_x_discrete(
      limits = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "ºC"),
      limits = c(floor(min(min(plot_data$tmean, na.rm = TRUE),
                           min(plot_data$p00tmean, na.rm = TRUE)) - 2), 
                 ceiling(max(max(plot_data$tmean, na.rm = TRUE),
                             max(plot_data$p100tmean, na.rm = TRUE)) + 2)),
      breaks = seq(from = floor(min(min(plot_data$tmean, na.rm = TRUE),
                                    min(plot_data$p00tmean, na.rm = TRUE)) - 2), 
                   to = ceiling(max(max(plot_data$tmean, na.rm = TRUE),
                                    max(plot_data$p100tmean, na.rm = TRUE))) + 2, by = 5)) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Temperature in Madrid - Retiro ", selected_year),
      subtitle = paste0("Monthly mean temperature vs. historical percentiles (", ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)"),
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
  
  # Add position in ranking of selected year
  for (mes in unique(selected_year_monthly_tmean$month)) { # Loop over 12 months
    p <- p + ggplot2::annotate(
      geom = "text", x = subset(selected_year_monthly_tmean, month == mes)$month,
      y = subset(selected_year_monthly_tmean, month == mes)$tmean,
      label = paste0(
        subset(reference_monthly_rank_tmean, month == mes & year == selected_year)$ranking, "º ",
        selected_year, " ",
        subset(selected_year_monthly_tmean, month == mes)$tmean, "ºC"
      ),
      family = "sans", size = 3.25, hjust = 0.5, vjust = 3
    )
  }
  
  # Add podium of years with most precipitation for each month
  for (mes in unique(selected_year_monthly_tmean$month)) { # Loop over 12 months
    for (rank in 1:3) { # Top 3 months
      p <- p + ggplot2::annotate(
        geom = "text", x = subset(reference_monthly_rank_tmean, month == mes & ranking == rank)$month,
        y = subset(selected_year_monthly_tmean, month == mes)$tmean,
        label = paste0(
          rank, "º ", subset(reference_monthly_rank_tmean, month == mes & ranking == rank)$year,
          " ", subset(reference_monthly_rank_tmean, month == mes & ranking == rank)$tmean, "ºC"
        ),
        family = "sans", size = 3.5, hjust = 0.5, vjust = -0.5 * (-rank) * 3 - 6
      )
    }
  }
    
  return(p) 
}
