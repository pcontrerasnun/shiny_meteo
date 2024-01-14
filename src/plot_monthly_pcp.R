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
      p00pcp = round(quantile(sumpcp, probs = 0.00, na.rm = TRUE), 1),
      p20pcp = round(quantile(sumpcp, probs = 0.20, na.rm = TRUE), 1),
      p40pcp = round(quantile(sumpcp, probs = 0.40, na.rm = TRUE), 1),
      p50pcp = round(quantile(sumpcp, probs = 0.50, na.rm = TRUE), 1),
      p60pcp = round(quantile(sumpcp, probs = 0.60, na.rm = TRUE), 1),
      p80pcp = round(quantile(sumpcp, probs = 0.80, na.rm = TRUE), 1),
      p100pcp = round(quantile(sumpcp, probs = 1, na.rm = TRUE), 1)
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
    dplyr::mutate(diffmedian = round(sumpcp - p50pcp, 1)) |>
    dplyr::mutate(diffmedian_x = ifelse(diffmedian > 0, paste0("x", round(sumpcp / p50pcp, 1)), 
                                 paste0("/", round(p50pcp / sumpcp, 1)))) |> 
    dplyr::mutate(diffmedian_x = ifelse(diffmedian_x == "/Inf", "-", diffmedian_x)) |> # Replace Inf with -
    dplyr::mutate(diffmedian_x = ifelse(diffmedian_x %in% c("/1", "x1"), "=", diffmedian_x)) |>  # Replace /1 and x1 with =
    dplyr::mutate(diffmedian = ifelse(diffmedian > 0, paste0("+", diffmedian), diffmedian)) |> 
    dplyr::mutate(diffmedian_y = ifelse(!is.na(sumpcp), paste0(round(sumpcp / p50pcp * 100), '%'), NA)) # Calculate %

  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = month)) +
    ggh4x::geom_box(aes(ymin = 0, ymax = p00pcp, fill = "P00", width = 0.9), alpha = 0.3) +
    ggh4x::geom_box(aes(ymin = p00pcp, ymax = p20pcp, fill = "P20", width = 0.9), alpha = 0.3) +
    ggh4x::geom_box(aes(ymin = p20pcp, ymax = p40pcp, fill = "P40", width = 0.9), alpha = 0.3) +
    ggh4x::geom_box(aes(ymin = p40pcp, ymax = p60pcp, fill = "P60", width = 0.9), alpha = 0.3) +
    ggh4x::geom_box(aes(ymin = p60pcp, ymax = p80pcp, fill = "P80", width = 0.9), alpha = 0.3) +
    ggh4x::geom_box(aes(ymin = p80pcp, ymax = p100pcp, fill = "P100", width = 0.9), alpha = 0.3) +
    ggh4x::geom_box(aes(ymin = p100pcp, ymax = p100pcp + 50, fill = ">P100", width = 0.9), alpha = 0.3) +
    ggplot2::geom_segment(aes(x = month, xend = month, y = 0, yend = sumpcp, color = "sumpcp"), 
                          linewidth = 0.75, na.rm = TRUE) +
    ggplot2::geom_point(aes(y = sumpcp, color = "sumpcp"), na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c("sumpcp" = "black"),
                                label = paste0("Monthly total precip. (", selected_year, ")"), 
                                guide = guide_legend(order = 1)) +
    ggplot2::geom_text(aes(y = sumpcp, label = paste(diffmedian, "*mm~vs.~italic(P)[50]")), 
                       parse = TRUE, vjust = -2.5, na.rm = TRUE) +
    ggplot2::geom_text(aes(y = sumpcp, label = paste0(diffmedian_x, ', ', diffmedian_y)), vjust = -1.5, na.rm = TRUE) +
#    ggplot2::annotate(
#      geom = "text",
#      x = tail(subset(plot_data, !is.na(sumpcp)), 1)$month, # Month with last no NULL sumpcp
#      y = tail(subset(plot_data, !is.na(sumpcp)), 1)$sumpcp, # Last no NULL sumpcp
#      label = selected_year, family = "sans", fontface = "bold", hjust = -0.25, vjust = -0.7
#    ) +
    #ggplot2::scale_alpha_manual(values = rep(0.3, 1)) +
    ggplot2::scale_fill_manual(
      values = c(">P100" = "#2166ac", "P100" = "#67a9cf", "P80" = "#d1e5f0", "P60" = "#f7f7f7", "P40" = "#fddbc7", 
                 "P20" = "#ef8a62", "P00" = "#b2182b"),
      labels = c(">P100" = expr(paste("Extrem. wet month (>", italic(P[100]), ") (", 
                                     !!ref_start_year, "-", !!ref_end_year, ")")), 
                 "P100" = expr(paste("Very wet month (", italic(P[80]), "-", italic(P[100]), ") (", 
                                     !!ref_start_year, "-", !!ref_end_year, ")")), 
                 "P80" = expr(paste("Wet month (", italic(P[60]), "-", italic(P[80]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")), 
                 "P60" = expr(paste("Normal month (", italic(P[40]), "-", italic(P[60]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")),
                 "P40" = expr(paste("Dry month (", italic(P[20]), "-", italic(P[40]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")),
                 "P20" = expr(paste("Very dry month (", italic(P[00]), "-", italic(P[20]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")")),
                 "P00" = expr(paste("Extrem. dry month (<", italic(P[00]), ") (", 
                                    !!ref_start_year, "-", !!ref_end_year, ")"))),
      breaks = c(">P100", "P100", "P80", "P60", "P40", "P20", "P00")) + # to give order
    ggplot2::scale_x_discrete(
      limits = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "mm"),
      breaks = seq(from = 0, to = max(max(plot_data$p100pcp), max(plot_data$sumpcp, na.rm = TRUE)) + 125, by = 50),
      limits = c(0, max(max(plot_data$p100pcp), max(plot_data$sumpcp, na.rm = TRUE)) + 100)) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation in Madrid - Retiro ", selected_year),
      subtitle = paste0("Monthly precipitation vs. historical percentiles (", ref_start_year, "-", ref_end_year, ")"),
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
    ) +
    ggplot2::guides(fill = guide_legend(override.aes = list(alpha = 0.7 / 7)))
  
  
  return(list(p, plot_data |> dplyr::select(month, sumpcp, everything(), -diffmedian_x), "month", "sumpcp"))
  
}
