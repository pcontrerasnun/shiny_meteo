#' Plot cumulated precipiation in each month along with min, max and median historical
#' precipitation for each month and ranking of selected year
#'
#' Plots precipitation in each month in selected year along with historical monthly max, min and
#' mean precipitation (historical period starts at ref_start_year' and ends at 'ref_end_year')
#'
#' @param data An R dataset with AEMET Open data
#' @param selected_year Year of study
#' @param ref_start_year Start year of reference period
#' @param ref_end_year End year of reference period
#' @param max_date Max date of data
#' @returns A ggplot2 plot
#' @examples
#' MonthlyRankingPcpPlot(data, 2023, 1981, 2010, "2023-09-24")
MonthlyRankingPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate each month's rank
  reference_monthly_rank_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
      date <= as.Date(paste0(ref_end_year, "-12-31"))) |
      (date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
        date <= as.Date(paste0(as.numeric(selected_year), "-12-31")))) |> # Include year of study
    dplyr::group_by(year, month) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE), .groups = "keep") |>
    dplyr::ungroup() |>
    dplyr::group_by(month) |>
    dplyr::mutate(ranking = rank(-sumpcp, ties.method = "first")) |>
    dplyr::arrange(month, ranking) |>
    dplyr::as_tibble()

  # Calculate min, mean and max historical precipitation in each month
  reference_stats_monthly_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
      date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
      date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::group_by(year, month) |>
    dplyr::mutate(sumpcp = sum(pcp)) |>
    dplyr::arrange(year, month) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      minpcp = round(min(sumpcp, na.rm = TRUE), 1),
      maxpcp = round(max(sumpcp, na.rm = TRUE), 1),
      meanpcp = round(mean(sumpcp, na.rm = TRUE), 1),
      .groups = "keep"
    ) |>
    dplyr::arrange(month) |>
    dplyr::as_tibble()

  # Calculated total precipitacion in each month of selected year
  selected_year_monthly_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
      date <= as.Date(paste0(as.numeric(selected_year), "-12-31"))) |>
    dplyr::group_by(month, year) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE), .groups = "keep") |>
    dplyr::arrange(year, month) |>
    dplyr::ungroup() |>
    dplyr::as_tibble()
  
  # Join data
  plot_data <- dplyr::left_join(reference_stats_monthly_pcp, selected_year_monthly_pcp, by = "month") |> 
    dplyr::select(-year)

  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = month)) +
    ggh4x::geom_box(aes(ymin = minpcp, ymax = maxpcp, width = 0.9), 
                    fill = "white", color = "black") +
    ggplot2::geom_col(aes(y = sumpcp, fill = "sumpcp")) +
    ggplot2::geom_errorbar(aes(y = meanpcp, ymin = meanpcp, ymax = meanpcp, color = "mean"), 
                           linetype = "dashed") +
    ggplot2::geom_errorbar(aes(y = maxpcp, ymin = maxpcp, ymax = maxpcp,color = "max"), 
                           linetype = "solid", linewidth = 1) +
    ggplot2::geom_errorbar(aes(y = minpcp, ymin = minpcp, ymax = minpcp, color = "min"), 
                           linetype = "solid", linewidth = 1) +
    ggplot2::scale_x_discrete(
      limits = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    ggplot2::scale_color_manual(breaks = c("max", "mean", "min"),
      values = c("mean" = "black", "max" = "#4daf4a", "min" = "#d7191c"),      
      labels = c("mean" = paste0("Monthly mean precip. (", ref_start_year, "-", ref_end_year, ")"),
                 "max" = paste0("Monthly max precip. (", ref_start_year, "-", ref_end_year, ")"),
                 "min" = paste0("Monthly min precip. (", ref_start_year, "-", ref_end_year, ")"))) +
    ggplot2::scale_fill_manual(
      values = c("sumpcp" = "#2c7bb6"),
      labels = c("sumpcp" = paste0("Monthly total precip. (", selected_year, ")"))) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "mm"), 
      breaks = seq(from = 0, to = max(max(plot_data$sumpcp, na.rm = TRUE), 
                                      plot_data$maxpcp) + 50, by = 25),
      limits = c(0, max(max(plot_data$sumpcp, na.rm = TRUE), 
                        plot_data$maxpcp) + 50)) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation in Madrid - Retiro ", selected_year),
      subtitle = paste0("Ranking monthly precipitation vs. historical values (",
                        ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/"),
      color = NULL, fill = NULL) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black",linewidth = 0.75),
      legend.position = c(0.135, 0.85),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5)
    ) +
    ggplot2::guides(color = guide_legend(override.aes = list(linetype = c("solid", "dotted", "solid"))))

  # Add position in ranking of selected year
  for (mes in unique(selected_year_monthly_pcp$month)) { # Loop over 12 months
    p <- p + ggplot2::annotate(
      geom = "text", x = subset(selected_year_monthly_pcp, month == mes)$month,
      y = max(
        subset(selected_year_monthly_pcp, month == mes)$sumpcp,
        subset(reference_stats_monthly_pcp, month == mes)$maxpcp
      ),
      label = paste0(
        subset(reference_monthly_rank_pcp, month == mes & year == selected_year)$ranking, "º ",
        selected_year, " ",
        subset(selected_year_monthly_pcp, month == mes)$sumpcp, "mm"
      ),
      family = "sans", size = 3.25, hjust = 0.5, vjust = 3
    )
  }

  # Add podium of years with most precipitation for each month
  for (mes in unique(selected_year_monthly_pcp$month)) { # Loop over 12 months
    for (rank in 1:3) { # Top 3 months
      p <- p + ggplot2::annotate(
        geom = "text", x = subset(reference_monthly_rank_pcp, month == mes & ranking == rank)$month,
        y = max(
          subset(selected_year_monthly_pcp, month == mes)$sumpcp,
          subset(reference_stats_monthly_pcp, month == mes)$maxpcp
        ),
        label = paste0(
          rank, "º ", subset(reference_monthly_rank_pcp, month == mes & ranking == rank)$year,
          " ", subset(reference_monthly_rank_pcp, month == mes & ranking == rank)$sumpcp, "mm"
        ),
        family = "sans", size = 3.5, hjust = 0.5, vjust = -0.5 * (-rank) * 3 - 5
      )
    }
  }

  return(list(p, plot_data, "month", "sumpcp"))
  
}
