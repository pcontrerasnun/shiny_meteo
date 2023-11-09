MonthlyPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate historical monthly percentiles
  reference_pcts_monthly_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
      date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
      date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::group_by(year, month) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE), .groups = "keep") |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      q00pcp = round(quantile(sumpcp, probs = 0.00, na.rm = TRUE), 1),
      q20pcp = round(quantile(sumpcp, probs = 0.20, na.rm = TRUE), 1),
      q40pcp = round(quantile(sumpcp, probs = 0.40, na.rm = TRUE), 1),
      q50pcp = round(quantile(sumpcp, probs = 0.50, na.rm = TRUE), 1),
      q60pcp = round(quantile(sumpcp, probs = 0.60, na.rm = TRUE), 1),
      q80pcp = round(quantile(sumpcp, probs = 0.80, na.rm = TRUE), 1),
      q100pcp = round(quantile(sumpcp, probs = 1, na.rm = TRUE), 1)
    ) |>
    dplyr::as_tibble()

  # Calculate total precipitation in each month for selected year
  selected_year_monthly_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
      date <= as.Date(paste0(as.numeric(selected_year), "-12-31"))) |>
    dplyr::group_by(month) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE)) |>
    dplyr::arrange(month) |>
    dplyr::ungroup() |>
    dplyr::as_tibble()

  # Join data and create new columns with diff vs P50
  plot_data <- left_join(reference_pcts_monthly_pcp, selected_year_monthly_pcp, by = "month") |> 
    dplyr::mutate(diffq50pcp = round(sumpcp - q50pcp, 1)) |>
    dplyr::mutate(diffq50pcp_x = ifelse(diffq50pcp > 0, paste0("x", round(sumpcp / q50pcp, 1)), 
                                 paste0("/", round(q50pcp / sumpcp, 1)))) |> 
    dplyr::mutate(diffq50pcp_x = ifelse(diffq50pcp_x == "/Inf", "-", diffq50pcp_x)) |> # Replace Inf with -
    dplyr::mutate(diffq50pcp_x = ifelse(diffq50pcp_x %in% c("/1", "x1"), "=", diffq50pcp_x)) |>  # Replace /1 and x1 with =
    dplyr::mutate(diffq50pcp = ifelse(diffq50pcp > 0, paste0("+", diffq50pcp), diffq50pcp)) 

  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = month)) +
    ggh4x::geom_box(aes(ymin = 0, ymax = q00pcp, fill = "P00", width = 0.9), alpha = 0.5) +
    ggh4x::geom_box(aes(ymin = q00pcp, ymax = q20pcp, fill = "P20", width = 0.9), alpha = 0.5) +
    ggh4x::geom_box(aes(ymin = q20pcp, ymax = q40pcp, fill = "P40", width = 0.9), alpha = 0.5) +
    ggh4x::geom_box(aes(ymin = q40pcp, ymax = q60pcp, fill = "P60", width = 0.9), alpha = 0.5) +
    ggh4x::geom_box(aes(ymin = q60pcp, ymax = q80pcp, fill = "P80", width = 0.9), alpha = 0.5) +
    ggh4x::geom_box(aes(ymin = q80pcp, ymax = q100pcp, fill = "P100", width = 0.9), alpha = 0.5) +
    ggplot2::geom_segment(aes(x = month, xend = month, y = 0, yend = sumpcp), na.rm = TRUE) +
    ggplot2::geom_point(aes(y = sumpcp), na.rm = TRUE) +
    ggplot2::geom_text(aes(y = sumpcp, label = paste(diffq50pcp, "*mm~vs.~italic(P)[50]")), 
                       parse = TRUE, vjust = -2.5, na.rm = TRUE) +
    ggplot2::geom_text(aes(y = sumpcp, label = diffq50pcp_x), vjust = -1.5, na.rm = TRUE) +
#    ggplot2::annotate(
#      geom = "text",
#      x = tail(subset(plot_data, !is.na(sumpcp)), 1)$month, # Month with last no NULL sumpcp
#      y = tail(subset(plot_data, !is.na(sumpcp)), 1)$sumpcp, # Last no NULL sumpcp
#      label = selected_year, family = "sans", fontface = "bold", hjust = -0.25, vjust = -0.7
#    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "P100" = "#abd9e9", "P80" = "#e0f3f8", "P60" = "white",
        "P40" = "#fee090", "P20" = "#fdae61", "P00" = "#f46d43"
      ),
      labels = c(
        "P00" = "Extrem. dry month", "P20" = "Very dry month",
        "P40" = "Dry month", "P80" = "Wet month",
        "P100" = "Very wet month", "P60" = "Normal month"
      ),
      breaks = c("P100", "P80", "P60", "P40", "P20", "P00")
    ) + # To give order
    ggplot2::scale_x_discrete(
      limits = c(
        "01", "02", "03", "04", "05", "06", "07", "08",
        "09", "10", "11", "12"
      ),
      labels = c(
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
        "Sep", "Oct", "Nov", "Dec"
      )
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "mm"),
      breaks = seq(
        from = 0, to = max(max(plot_data$q100pcp), max(plot_data$sumpcp, na.rm = TRUE))
        + 50, by = 25
      ),
      limits = c(0, max(max(plot_data$q100pcp), max(plot_data$sumpcp, na.rm = TRUE)) + 50)
    ) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation in Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Monthly precipitation vs. historical values (",
        ref_start_year, "-", ref_end_year, ")"
      ),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)"
      ),
      color = NULL, fill = NULL
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(
        fill = "white", color = "black",
        linewidth = 0.75
      ),
      legend.position = c(0.075, 0.85),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.text = ggtext::element_markdown()
    )
  
  return(p)
}
