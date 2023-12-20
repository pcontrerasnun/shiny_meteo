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
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
      date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::group_by(year) |>
    dplyr::mutate(cumsumpcp = cumsum(pcp)) |>
    dplyr::arrange(year, month, day) |>
    dplyr::group_by(day, month) |>
    dplyr::summarise(
      cump00pcp = round(quantile(cumsumpcp, probs = 0.00, na.rm = TRUE), 1),
      cump05pcp = round(quantile(cumsumpcp, probs = 0.05, na.rm = TRUE), 1),
      cump20pcp = round(quantile(cumsumpcp, probs = 0.20, na.rm = TRUE), 1),
      cump40pcp = round(quantile(cumsumpcp, probs = 0.40, na.rm = TRUE), 1),
      cump50pcp = round(quantile(cumsumpcp, probs = 0.50, na.rm = TRUE), 1),
      cump60pcp = round(quantile(cumsumpcp, probs = 0.60, na.rm = TRUE), 1),
      cump80pcp = round(quantile(cumsumpcp, probs = 0.80, na.rm = TRUE), 1),
      cump95pcp = round(quantile(cumsumpcp, probs = 0.95, na.rm = TRUE), 1),
      cump100pcp = round(quantile(cumsumpcp, probs = 1, na.rm = TRUE), 1),
      .groups = "keep" # .groups to avoid warnings
    ) |>
    dplyr::arrange(month, day) |>
    dplyr::as_tibble()

  # Calculate cumulative sum precipitation for selected year
  selected_year_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
      date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::mutate(cumsumpcp = cumsum(tidyr::replace_na(pcp, 0))) |>
    dplyr::as_tibble()

  # Join previous two datasets and create new columns 'diffmedian' and 'date'
  plot_data <- left_join(reference_pcts_pcp, selected_year_pcp, by = c("day", "month")) |>
    dplyr::select(
      day, month, cump00pcp, cump05pcp, cump20pcp, cump40pcp, cump50pcp,
      cump60pcp, cump80pcp, cump95pcp, cump100pcp, cumsumpcp
    ) |>
    dplyr::mutate(diffmedian = cumsumpcp - cump50pcp) |>
    dplyr::mutate(date = as.Date(paste0(day, "-", month, selected_year), format = "%d-%m%Y"))
  
  # For ranking of max consecutive days of precip.
  ranking_max_consec_days_pcp <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::group_by(year) |> 
    dplyr::mutate(haspcp = ifelse(coalesce(pcp, 0) > 0, 1, 0)) |> # coalesce to turn NA to 0
    dplyr::group_by(year) |> 
    dplyr::summarise(rle = list(rle(haspcp))) |> 
    dplyr::mutate(consecdayspcp = sapply(rle, function(x) max(x$lengths[x$values == 1]))) |> 
    dplyr::select(year, consecdayspcp) |> 
    dplyr::arrange(-consecdayspcp) |> 
    dplyr::mutate(ranking = rank(-consecdayspcp, ties.method = "first")) |>
    dplyr::as_tibble()
  
  # For ranking of max consecutive days of NO precip.
  ranking_max_consec_days_no_pcp <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::group_by(year) |> 
    dplyr::mutate(haspcp = ifelse(coalesce(pcp, 0) == 0, 1, 0)) |> # coalesce to turn NA to 0
    dplyr::group_by(year) |> 
    dplyr::summarise(rle = list(rle(haspcp))) |> 
    dplyr::mutate(consecdaysnopcp = sapply(rle, function(x) max(x$lengths[x$values == 1]))) |> 
    dplyr::select(year, consecdaysnopcp) |> 
    dplyr::arrange(-consecdaysnopcp) |> 
    dplyr::mutate(ranking = rank(-consecdaysnopcp, ties.method = "first")) |>
    dplyr::as_tibble()

  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = date, y = cumsumpcp)) +
    ggplot2::geom_ribbon(aes(ymin = 0, ymax = cump00pcp), alpha = 0.3, color = "#b2182b", 
                         fill = "#b2182b", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = cump00pcp, ymax = cump20pcp), alpha = 0.3, color = "#ef8a62", 
                         fill = "#ef8a62", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = cump20pcp, ymax = cump40pcp), alpha = 0.3, color = "#fddbc7", 
                         fill = "#fddbc7", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = cump40pcp, ymax = cump60pcp), alpha = 0.3, color = "#f7f7f7", 
                         fill = "#f7f7f7", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = cump60pcp, ymax = cump80pcp), alpha = 0.3, color = "#d1e5f0", 
                         fill = "#d1e5f0", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = cump80pcp, ymax = cump100pcp), alpha = 0.3, color = "#67a9cf",
                         fill = "#67a9cf", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = cump100pcp, ymax = cump100pcp + 100), alpha = 0.3, color = "#2166ac",
                         fill = "#2166ac", linetype = "51", lineend = "round", linejoin = "round") +
    ggplot2::geom_line(aes(color = "cumsumpcp"), linewidth = 0.85, lineend = "round", na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c("cumsumpcp" = "black"), 
                               label = paste0("Cumulative daily precip. (", selected_year, ")")) +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd(paste0(selected_year, "-01-01")), ymd(paste0(selected_year, "-12-31")), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd(paste0(selected_year, "-01-01"))), 
                 as.numeric(ymd(paste0(as.numeric(selected_year) + 1, "-02-04")))),
      expand = expansion(mult = c(0.02, 0))
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "mm"),
      breaks = seq(from = 0, to = max(plot_data$cump100pcp) + 200, by = 100),
      limits = c(0, max(plot_data$cump100pcp) + 200),
      expand = c(0, 20, 0, 0)
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$date, na.rm = TRUE), y = max(plot_data$cump100pcp),
      label = paste("Extrem.~wet~(italic(max))"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$date, na.rm = TRUE), y = max(plot_data$cump80pcp),
      label = paste("Very~wet~(italic(P)[80])"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$date, na.rm = TRUE), y = max(plot_data$cump60pcp),
      label = paste("Wet~(italic(P)[60])"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$date, na.rm = TRUE), y = max(plot_data$cump50pcp),
      label = paste("Normal~(italic(P)[50])"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$date, na.rm = TRUE), y = max(plot_data$cump40pcp),
      label = paste("Dry~(italic(P)[40])"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$date, na.rm = TRUE), y = max(plot_data$cump20pcp),
      label = paste("Very~dry~(italic(P)[20])"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = max(plot_data$date, na.rm = TRUE), y = max(plot_data$cump00pcp),
      label = paste("Extrem.~dry~(italic(min))"),
      parse = TRUE, family = "sans", hjust = -0.05, vjust = 0.5
    ) +
    ggplot2::annotation_custom(
      gridtext::richtext_grob(
        x = unit(.05, "npc"),
        y = unit(.8, "npc"),
        text = paste0("**Ranking** (", ref_start_year, "-", ref_end_year, ")<br>", 
                      "Max consecutive days with precip. <br><br>", 
                      head(ranking_max_consec_days_pcp, 1)$ranking, "º ", 
                      head(ranking_max_consec_days_pcp, 1)$year, ": ", 
                      head(ranking_max_consec_days_pcp, 1)$consecdayspcp, " days<br>",
                      head(ranking_max_consec_days_pcp, 2)[2,]$ranking, "º ", 
                      head(ranking_max_consec_days_pcp, 2)[2,]$year, ": ", 
                      head(ranking_max_consec_days_pcp, 2)[2,]$consecdayspcp, " days<br>",
                      head(ranking_max_consec_days_pcp, 3)[3,]$ranking, "º ",
                      head(ranking_max_consec_days_pcp, 3)[3,]$year, ": ", 
                      head(ranking_max_consec_days_pcp, 3)[3,]$consecdayspcp, " days<br>",
                      "---------------------------<br>",
                      subset(ranking_max_consec_days_pcp, 
                             ranking_max_consec_days_pcp$year == selected_year)$ranking, "º ",
                      selected_year, ": ",
                      subset(ranking_max_consec_days_pcp, 
                             ranking_max_consec_days_pcp$year == selected_year)$consecdayspcp,
                      " days"),
        hjust = 0, vjust = 1,
        r = unit(0.15, "lines"),
        box_gp = grid::gpar(col = "black", lwd = 2),
        padding = unit(0.5, "lines")
      )
    ) +
    ggplot2::annotation_custom(
      gridtext::richtext_grob(
        x = unit(.2, "npc"),
        y = unit(.8725, "npc"),
        text = paste0("**Ranking** (", ref_start_year, "-", ref_end_year, ")<br>", 
                      "Max consecutive days with no precip. <br><br>", 
                      head(ranking_max_consec_days_no_pcp, 1)$ranking, "º ", 
                      head(ranking_max_consec_days_no_pcp, 1)$year, ": ", 
                      head(ranking_max_consec_days_no_pcp, 1)$consecdaysnopcp, " days<br>",
                      head(ranking_max_consec_days_no_pcp, 2)[2,]$ranking, "º ", 
                      head(ranking_max_consec_days_no_pcp, 2)[2,]$year, ": ", 
                      head(ranking_max_consec_days_no_pcp, 2)[2,]$consecdaysnopcp, " days<br>",
                      head(ranking_max_consec_days_no_pcp, 3)[3,]$ranking, "º ",
                      head(ranking_max_consec_days_no_pcp, 3)[3,]$year, ": ", 
                      head(ranking_max_consec_days_no_pcp, 3)[3,]$consecdaysnopcp, " days<br>",
                      "---------------------------<br>",
                      subset(ranking_max_consec_days_no_pcp, 
                             ranking_max_consec_days_no_pcp$year == selected_year)$ranking, "º ",
                      selected_year, ": ",
                      subset(ranking_max_consec_days_no_pcp, 
                             ranking_max_consec_days_no_pcp$year == selected_year)$consecdaysnopcp,
                      " days"),
        hjust = 0, vjust = 1,
        r = unit(0.15, "lines"),
        box_gp = grid::gpar(col = "black", lwd = 2),
        padding = unit(0.5, "lines")
      )
    ) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation in Madrid - Retiro ", selected_year),
      subtitle = paste0("Cumulative daily precipitation vs. historical percentiles (",
                        ref_start_year, "-", ref_end_year, ")"),
      caption = paste0("Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)")) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.125, 0.85),
      #legend.justification = c(0, 1),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.title = ggplot2::element_blank())

  # If there has been superavit of rain during the year
  # In plot_daily_cum_pcp.R a different approach can be found for plotting these points with ggrepel
  if (max(plot_data$diffmedian, na.rm = TRUE) > 0) {
    p <- p +
      ggplot2::annotate(
        geom = "point", x = plot_data[which.max(plot_data$diffmedian), ]$date,
        y = plot_data[which.max(plot_data$diffmedian), ]$cumsumpcp,
        shape = 21, fill = "#2c7bb6", size = 2, stroke = 1
      ) +
      ggrepel::geom_label_repel(
        data = data.frame(x = plot_data[which.max(plot_data$diffmedian), ]$date,
            y = plot_data[which.max(plot_data$diffmedian), ]$cumsumpcp,
            label = paste("+", plot_data[which.max(plot_data$diffmedian), ]$diffmedian, "*mm~vs.~italic(P)[50]")
        ), aes(x = x, y = y, label = label), parse = TRUE)
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
      ggrepel::geom_label_repel(
        data = data.frame(x = plot_data[which.min(plot_data$diffmedian), ]$date,
                          y = plot_data[which.min(plot_data$diffmedian), ]$cumsumpcp,
                          label = paste(plot_data[which.min(plot_data$diffmedian), ]$diffmedian, "*mm~vs.~italic(P)[50]")
        ), aes(x = x, y = y, label = label), parse = TRUE)
  }

  return(list(p, plot_data |> dplyr::select(date, cumsumpcp, everything(), -c(day, month, cump05pcp, cump95pcp)), 
              "date", "cumsumpcp"))
  
}
