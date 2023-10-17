#' Plot cumulative daily sum precipitation along with historical precipitation mean
#'
#' Plots cumulative daily sum precipitation in selected year along with historical precipitation
#' mean based on a reference period defined by 'ref_start_year' and 'ref_end_year'
#'
#' @param data An R dataset with AEMET Open data
#' @param selected_year Year of study
#' @param ref_start_year Start year of reference period
#' @param ref_end_year End year of reference period
#' @param max_date Max date of data
#' @returns A ggplot2 plot
#' @examples
#' DailyCumPcpPlot(data, 2023, 1981, 2010, "2023-09-24")
DailyCumPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate historical mean for reference period
  reference_mean_pcp <- data_clean |>
    dplyr::filter(fecha >= as.Date(paste0(ref_start_year, "-01-01")) &
      fecha <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::group_by(dia, mes) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE), n = n()) |>
    dplyr::ungroup() |>
    dplyr::arrange(mes, dia) |>
    dplyr::mutate(
      meanpcp = sumpcp / n,
      cummeanpcp = round(cumsum(ifelse(is.na(meanpcp), 0, meanpcp)), 1)
    )

  # Calculate cumulative sum precipitation for selected year
  selected_year_pcp <- data_clean |>
    dplyr::filter(fecha >= as.Date(paste0(selected_year, "-01-01")) &
      fecha <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::mutate(cumsumpcp = cumsum(tidyr::replace_na(pcp, 0)))

  # Join previous two datasets and create new columns 'diffmean' and 'fecha'
  plot_data <- left_join(reference_mean_pcp, selected_year_pcp, by = c("dia", "mes")) |>
    dplyr::select(
      dia, mes, cummeanpcp, cumsumpcp
    ) |>
    mutate(diffmean = cumsumpcp - cummeanpcp) |>
    dplyr::mutate(fecha = as.Date(paste0(dia, "-", mes, "2023"), format = "%d-%m%Y")) |> # We choose
    # 2023 since it doesn't have 29th Feb, it doesn't matter what year we choose but it can't be
    # a leap year
    dplyr::as_tibble()
  
  # For annotating points
  annotate_data <- rbind(plot_data |> filter(!is.na(cumsumpcp)) |> slice_tail(n = 1), # Current pcp
                         subset(plot_data, diffmean == min(diffmean[diffmean < 0], na.rm = TRUE)), # Max deficit
                         subset(plot_data, diffmean == max(diffmean[diffmean > 0], na.rm = TRUE))) |> # Max superavit
    dplyr::mutate(percentage = round(diffmean / cummeanpcp * 100))
  
  if (annotate_data[mlr3misc::which_max(annotate_data$cumsumpcp, ties_method = "last"), ]$diffmean > 0) {
    sign <- "+"
  } else {
    sign <- ""
  }
  
  annotate_labels <- data.frame(
    label = c(
      paste0("'Precip. ", max(annotate_data$cumsumpcp), "mm\n(", sign, 
            annotate_data[mlr3misc::which_max(annotate_data$cumsumpcp, ties_method = "last"), ]$diffmean, 
            "mm, ", sign, annotate_data[which.max(annotate_data$fecha), ]$percentage, "%)'"),
      if (sum(annotate_data$diffmean < 0) > 0) { # Only create label if there has been deficit
      paste(min(annotate_data$diffmean), "*mm~vs.~italic(mean)")},
      if (sum(annotate_data$diffmean > 0) > 0) { # Only create label if there has been superavit
      paste("+", max(annotate_data$diffmean), "*mm~vs.~italic(mean)")}
    )
  )
  
  # Data points colors
  if (any(annotate_data$diffmean < 0) && any(annotate_data$diffmean > 0)) {
    colors <- c("black", "#d7191c", "#2c7bb6")
  } else if (any(annotate_data$diffmean < 0)) {
    colors <- c("black", "#d7191c")
  } else if (any(annotate_data$diffmean > 0)) {
    colors <- c("black", "#2c7bb6")
  }

  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = fecha, y = cumsumpcp)) +
    ggplot2::geom_segment(aes(xend = fecha, yend = cummeanpcp, color = diffmean), linewidth = 1.2) +
    ggplot2::scale_color_gradient2(high = "#2c7bb6", mid = "white", low = "#d7191c") +
    ggplot2::geom_line(linewidth = 0.85, lineend = "round") +
    ggplot2::geom_line(aes(y = cummeanpcp)) +
    ggplot2::geom_point(data = annotate_data, fill = colors, 
                        size = 2, stroke = 1, shape = 21) +
    ggrepel::geom_text_repel(data = annotate_data, aes(label = annotate_labels$label), parse = TRUE) +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd("2023-01-01"), ymd("2023-12-31"), by = "month")),
      labels = format(seq(ymd("2023-01-01"), ymd("2023-12-31"), by = "month"), "%b"),
      limits = c(as.numeric(ymd("2023-01-01")), as.numeric(ymd("2023-12-31"))),
      expand = ggplot2::expansion(mult = c(0.04, 0.05))
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "mm"),
      breaks = seq(from = 0, to = max(
        max(plot_data$cumsumpcp, na.rm = TRUE),
        max(plot_data$cummeanpcp, na.rm = TRUE)
      ) + 100, by = 100),
      limits = c(0, max(
        max(plot_data$cumsumpcp, na.rm = TRUE),
        max(plot_data$cummeanpcp, na.rm = TRUE)
      ) + 100),
      expand = c(0, 20, 0, 0)
    ) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitación en Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Precipitación diaria acumulada comparada con media histórica (",
        ref_start_year, "-", ref_end_year, ")"
      ),
      caption = paste0(
        "Actualizado: ", max_date, ", Fuente: AEMET OpenData, Elab. propia (@Pcontreras95)"
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25), legend.position = "none"
    )

  return(p)
}
