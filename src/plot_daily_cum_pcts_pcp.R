#' Plot cumulative daily sum precipitation along with historical precipitation percentiles
#'
#' Plots cumulative daily sum precipitation in selected year along with historical precipitation
#' percentiles based on a reference period defined by 'ref_start_year' and 'ref_end_year'
#'
#' @param data An R dataset with AEMET Open data
#' @param selected_year Year of study
#' @param ref_start_year Start year of reference period
#' @param ref_end_year End year of reference period
#' @param max_date Max date of data
#' @returns A ggplot2 plot
#' @examples
#' DailyCumPcpPctsPlot(data, 2023, 1981, 2010, "2023-09-24")
DailyCumPcpPctsPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate historical percentiles for reference period
  reference_pcts_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
      date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::group_by(year) |>
    dplyr::mutate(cumsumpcp = cumsum(pcp)) |>
    dplyr::arrange(year, month, day) |>
    dplyr::group_by(day, month) |>
    dplyr::summarise(
      cumq00pcp = round(quantile(cumsumpcp, probs = 0.00, na.rm = TRUE), 1),
      cumq05pcp = round(quantile(cumsumpcp, probs = 0.05, na.rm = TRUE), 1),
      cumq20pcp = round(quantile(cumsumpcp, probs = 0.20, na.rm = TRUE), 1),
      cumq40pcp = round(quantile(cumsumpcp, probs = 0.40, na.rm = TRUE), 1),
      cumq50pcp = round(quantile(cumsumpcp, probs = 0.50, na.rm = TRUE), 1),
      cumq60pcp = round(quantile(cumsumpcp, probs = 0.60, na.rm = TRUE), 1),
      cumq80pcp = round(quantile(cumsumpcp, probs = 0.80, na.rm = TRUE), 1),
      cumq95pcp = round(quantile(cumsumpcp, probs = 0.95, na.rm = TRUE), 1),
      cumq100pcp = round(quantile(cumsumpcp, probs = 1, na.rm = TRUE), 1),
      .groups = "keep"
    ) |>
    dplyr::arrange(month, day)

  # Calculate cumulative sum precipitation for selected year
  selected_year_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
      date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::mutate(cumsumpcp = cumsum(tidyr::replace_na(pcp, 0)))

  # Join previous two datasets and create new columns 'diffmedian' and 'date'
  plot_data <- left_join(reference_pcts_pcp, selected_year_pcp, by = c("day", "month")) |>
    dplyr::select(
      day, month, cumq00pcp, cumq05pcp, cumq20pcp, cumq40pcp, cumq50pcp,
      cumq60pcp, cumq80pcp, cumq95pcp, cumq100pcp, cumsumpcp
    ) |>
    dplyr::mutate(diffmedian = cumsumpcp - cumq50pcp) |> 
    dplyr::mutate(date = as.Date(paste0(day, "-", month, "2023"), format = "%d-%m%Y")) |> # We choose
    # 2023 since it doesn't have 29th Feb, it doesn't matter what year we choose but it can't be
    # a leap year
    dplyr::as_tibble()

  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = date, y = cumsumpcp)) +
    ggplot2::geom_ribbon(aes(ymin = cumq00pcp, ymax = cumq20pcp),
      alpha = 0.3,
      color = "#d7191c", fill = "#d7191c", linetype = "51", lineend = "round",
      linejoin = "round"
    ) +
    ggplot2::geom_ribbon(aes(ymin = cumq20pcp, ymax = cumq40pcp),
      alpha = 0.1,
      color = "#fdae61", fill = "#fdae61", linetype = "51", lineend = "round",
      linejoin = "round"
    ) +
    ggplot2::geom_ribbon(aes(ymin = cumq60pcp, ymax = cumq80pcp),
      alpha = 0.1,
      color = "#abd9e9", fill = "#abd9e9", linetype = "51", lineend = "round",
      linejoin = "round"
    ) +
    #  ggplot2::geom_ribbon_pattern(aes(ymin = cumq80pcp, ymax = cumq100pcp), pattern = 'gradient',
    #                      na.rm = TRUE, pattern_fill  = '#abd9e9', pattern_fill2 = '#2c7bb6',
    #                      pattern_alpha = 0.01, pattern_linetype = '51', lineend = 'round',
    #                      linejoin = 'round', pattern_orientation = 'vertical') +
    ggplot2::geom_ribbon(aes(ymin = cumq80pcp, ymax = cumq100pcp),
      alpha = 0.3, color = "#2c7bb6",
      fill = "#2c7bb6", linetype = "51", lineend = "round", linejoin = "round"
    ) +
    #  geom_line(aes(y = cumq50pcp)) +
    ggplot2::geom_line(linewidth = 0.85, lineend = "round", na.rm = TRUE) +
    #  ggplot2::geom_ribbon_pattern(aes(x = date, ymin = cumq50pcp, ymax = cumsumpcp),
    #                               pattern = 'gradient', na.rm = TRUE, pattern_fill  = '#377eb8',
    #                               pattern_fill2 = '#e41a1c') +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd("2023-01-01"), ymd("2023-12-31"),
        by = "month"
      )),
      labels = format(seq(ymd("2023-01-01"), ymd("2023-12-31"), by = "month"), "%b"),
      limits = c(as.numeric(ymd("2023-01-01")), as.numeric(ymd("2024-02-10"))),
      expand = expansion(mult = c(0.02, 0))
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "mm"),
      breaks = seq(from = 0, to = max(plot_data$cumq100pcp) + 100, by = 100),
      limits = c(0, max(plot_data$cumq100pcp) + 100),
      expand = c(0, 20, 0, 0)
    ) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation in Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Cumulative daily precipitation vs. historical percentiles (",
        ref_start_year, "-", ref_end_year, ")"
      ),
      caption = paste0(
        "Updated: ", max_date, ", Source: AEMET OpenData, Graph: @Pcontreras95 (Twitter)"
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25)
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$date, na.rm = TRUE), y = max(plot_data$cumq100pcp),
      label = paste("Extrem.~wet~(italic(max))"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$date, na.rm = TRUE), y = max(plot_data$cumq80pcp),
      label = paste("Very~wet~(italic(P)[80])"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$date, na.rm = TRUE), y = max(plot_data$cumq60pcp),
      label = paste("Wet~(italic(P)[60])"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$date, na.rm = TRUE), y = max(plot_data$cumq50pcp),
      label = paste("Normal~(italic(P)[50])"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$date, na.rm = TRUE), y = max(plot_data$cumq40pcp),
      label = paste("Dry~(italic(P)[40])"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$date, na.rm = TRUE), y = max(plot_data$cumq20pcp),
      label = paste("Very~dry~(italic(P)[20])"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$date, na.rm = TRUE), y = max(plot_data$cumq00pcp),
      label = paste("Extrem.~dry~(italic(min))"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) 
#    ggplot2::geom_point(data = plot_data |> filter(!is.na(cumsumpcp)) |> slice_tail(n = 1)) +
#    https://github.com/slowkow/ggrepel/issues/153
#    ggrepel::geom_text_repel(data = plot_data |> filter(!is.na(cumsumpcp)) |> slice_tail(n = 1),
#                             aes(label = paste0("Precip. ", max(plot_data$cumsumpcp, na.rm = TRUE),
#                                                "mm"))) 
#    ggplot2::annotate(geom = "text", x = plot_data[mlr3misc::which_max(plot_data$cumsumpcp, 
#                                                                       ties_method = "last",
#                                                                       na_rm = TRUE), ]$date,
#                      y = max(plot_data$cumsumpcp, na.rm = TRUE),
#                      label = paste0("Precip. ", max(plot_data$cumsumpcp, na.rm = TRUE), "mm"),
#                      hjust = -0.1)

  # If there has been superavit of rain during the year
  # In plot_daily_cum_pcp.R a different approach can be found for plotting these points with ggrepel
  if (max(plot_data$diffmedian, na.rm = TRUE) > 0) {
    p <- p +
      ggplot2::annotate(
        geom = "point", x = plot_data[which.max(plot_data$diffmedian), ]$date,
        y = plot_data[which.max(plot_data$diffmedian), ]$cumsumpcp,
        shape = 21, fill = "#2c7bb6", size = 2, stroke = 1
      ) +
      ggplot2::annotate(
        geom = "text", x = plot_data[which.max(plot_data$diffmedian), ]$date,
        y = plot_data[which.max(plot_data$diffmedian), ]$cumsumpcp,
        label = paste(
          "+", plot_data[which.max(plot_data$diffmedian), ]$diffmedian,
          "*mm~vs.~italic(P)[50]"
        ), parse = TRUE, vjust = -1
      )
#    https://github.com/slowkow/ggrepel/issues/153
#    ggplot2::geom_point(data = plot_data[which.max(plot_data$diffmedian), ], shape = 21, 
#                        fill = "#2c7bb6", size = 2, stroke = 1) +
#    ggrepel::geom_text_repel(data = plot_data[which.max(plot_data$diffmedian), ],
#                               aes(label = paste("+", plot_data[which.max(plot_data$diffmedian), ]$diffmedian, 
#    "*mm~vs.~italic(P)[50]"), parse = TRUE), vjust = -1) 
  }

  # If there has been deficit of rain during the year
  # In plot_daily_cum_pcp.R a different approach can be found for plotting these points with ggrepel
  if (min(plot_data$diffmedian, na.rm = TRUE) < 0) {
    p <- p +
      ggplot2::annotate(
        geom = "point", x = plot_data[which.min(plot_data$diffmedian), ]$date,
        y = plot_data[which.min(plot_data$diffmedian), ]$cumsumpcp,
        shape = 21, fill = "#d7191c", size = 2, stroke = 1
      ) +
      ggplot2::annotate(
        geom = "text", x = plot_data[which.min(plot_data$diffmedian), ]$date,
        y = plot_data[which.min(plot_data$diffmedian), ]$cumsumpcp,
        label = paste(
          plot_data[which.min(plot_data$diffmedian), ]$diffmedian,
          "*mm~vs.~italic(P)[50]"
        ), parse = TRUE, vjust = 2
      )
  }

  return(p)
}
