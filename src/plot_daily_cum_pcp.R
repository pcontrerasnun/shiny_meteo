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
DailyCumPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date, title) {
  # Calculate historical mean for reference period
  reference_mean_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
      date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::group_by(day, month) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE), n = n(), .groups = "keep") |>
    dplyr::ungroup() |>
    dplyr::arrange(month, day) |>
    dplyr::mutate(
      meanpcp = sumpcp / n,
      cummeanpcp = round(cumsum(ifelse(is.na(meanpcp), 0, meanpcp)), 1)
    ) |> 
    dplyr::as_tibble()

  # Calculate cumulative sum precipitation for selected year
  selected_year_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(selected_year, "-01-01")) &
      date <= as.Date(paste0(selected_year, "-12-31"))) |>
    dplyr::mutate(cumsumpcp = cumsum(tidyr::replace_na(pcp, 0))) |> 
    dplyr::as_tibble()

  # Join previous two datasets and create new columns 'diffmean' and 'date'
  plot_data <- left_join(reference_mean_pcp, selected_year_pcp, by = c("day", "month")) |>
    dplyr::select(
      day, month, cummeanpcp, cumsumpcp
    ) |>
    mutate(diffmean = cumsumpcp - cummeanpcp) |>
    dplyr::mutate(date = as.Date(paste0(day, "-", month, selected_year), format = "%d-%m%Y")) |> 
    dplyr::select(date, cummeanpcp, cumsumpcp, diffmean)
  
  # For geom_segment coloring purposes
  color_data <- plot_data |> 
    dplyr::reframe(x = seq(min(plot_data$date, na.rm = TRUE), max(plot_data$date, na.rm = TRUE), length = 1000),
                   y1 = approx(date, cumsumpcp, xout = x)$y,
                   y2 = approx(date, cummeanpcp, xout = x)$y,
                   diff = y1 - y2)
  
  # For annotating points
  annotate_data <- rbind(plot_data |> filter(!is.na(cumsumpcp)) |> slice_tail(n = 1), # Current pcp
                         # Max deficit, slice_tail in case there are two days with same deficit
                         slice_tail(subset(plot_data, diffmean == min(diffmean[diffmean < 0], na.rm = TRUE)), n = 1),
                         slice_tail(subset(plot_data, diffmean == max(diffmean[diffmean > 0], na.rm = TRUE)), n = 1)) |> # Max superavit
    dplyr::mutate(percentage = ifelse(diffmean > 0, paste0("x", round(cumsumpcp / cummeanpcp, 1)), 
                                      paste0("phantom()/", round(cummeanpcp / cumsumpcp, 1)))) |> 
    dplyr::mutate(percentage = ifelse(percentage == "/Inf", "-", percentage)) |> # Replace Inf with -
    dplyr::mutate(percentage = ifelse(percentage %in% c("phantom()/1", "x1"), "phantom()==phantom()", percentage)) # Replace /1 and x1 with =
  
  # If there is superavit
  if (annotate_data[mlr3misc::which_max(annotate_data$cumsumpcp, ties_method = "last"), ]$diffmean > 0) {
    sign <- "+"
  } else {
    sign <- ""
  }
  
  annotate_labels <- data.frame(
    label = c(
      paste0("atop(Precip.~", max(annotate_data$cumsumpcp), "*mm,list(", sign, # Current precip.
            annotate_data[mlr3misc::which_max(annotate_data$cumsumpcp, ties_method = "last"), ]$diffmean, # absolute diff
            "*mm,", annotate_data[which.max(annotate_data$date), ]$percentage, "))"), # % diff
      if (sum(annotate_data$diffmean < 0) > 0) { # Only create label if there has been deficit
      paste(min(annotate_data$diffmean), "*mm~vs.~italic(mean)")},
      if (sum(annotate_data$diffmean > 0) > 0) { # Only create label if there has been superavit
      paste0("+", max(annotate_data$diffmean), "*mm~vs.~italic(mean)")}
    )
  )
  
  # Data points colors
  if (any(annotate_data$diffmean < 0) && any(annotate_data$diffmean > 0)) {
    colors <- c("black", "#d7191c", "#2c7bb6") # If superavit and deficit exist in same year
  } else if (any(annotate_data$diffmean < 0)) {
    colors <- c("black", "#d7191c") # If only there has been deficit
  } else if (any(annotate_data$diffmean > 0)) {
    colors <- c("black", "#2c7bb6") # If only there has been superavit
  }
  
  # For ranking of days with most rain
  ranking_days_most_pcp <- data |> 
    dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
                     date <= as.Date(paste0(ref_end_year, "-12-31"))) |
                    (date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
                       date <= as.Date(paste0(as.numeric(selected_year), "-12-31")))) |>
    dplyr::arrange(-pcp) |> 
    dplyr::mutate(ranking = rank(-pcp, ties.method = "first")) |> 
    dplyr::mutate(date = format(date, "%d-%m-%Y")) |> 
    dplyr::as_tibble()

  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = date, y = cumsumpcp)) +
#    ggplot2::geom_segment(aes(xend = date, yend = cummeanpcp, color = diffmean), linewidth = 1.2,
#                          na.rm = TRUE) +
    ggplot2::annotate("rect", ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.2,
                      xmin = seq(ymd(paste0(selected_year, "-02-01")), by = "2 month", length = 6),
                      xmax = seq(ymd(paste0(selected_year, "-03-01")), by = "2 month", length = 6)) +
    ggplot2::geom_segment(data = color_data, aes(x = x, y = y1, xend = x, yend = y2, color = diff),
                          linewidth = 1, na.rm = TRUE) +
    ggplot2::scale_color_gradient2(high = "#2c7bb6", mid = "white", low = "#d7191c", guide = guide_none()) +
    ggplot2::geom_line(aes(linetype = "Cumsumpcp"), linewidth = 0.85, lineend = "round", na.rm = TRUE) +
    ggplot2::geom_line(aes(y = cummeanpcp, linetype = "Cummeanpcp"), na.rm = TRUE, show.legend = FALSE) +
    ggplot2::scale_linetype_manual(values = c("Cummeanpcp" = "longdash", "Cumsumpcp" = "solid"),
                                   labels = c(paste0("Cumulative daily mean precip. (", 
                                                     ref_start_year, "-", ref_end_year, ")"), 
                                              paste0("Cumulative daily precip. (", selected_year, ")"))) +
    ggplot2::geom_point(data = annotate_data, fill = colors, 
                        size = 2, stroke = 1, shape = 21) +
    ggrepel::geom_label_repel(data = annotate_data, aes(label = annotate_labels$label), parse = TRUE) +
    ggplot2::annotation_custom(
      gridtext::richtext_grob(x = unit(.0435, "npc"), y = unit(.8, "npc"), 
      text = paste0("**Ranking** (", ref_start_year, "-", ref_end_year, ")<br>",
                     "Max precip. in a single day <br><br>", 
                     head(ranking_days_most_pcp, 1)$ranking, "º ", head(ranking_days_most_pcp, 1)$date, ": ", 
                     head(ranking_days_most_pcp, 1)$pcp, "mm<br>",
                     head(ranking_days_most_pcp, 2)[2,]$ranking, "º ", head(ranking_days_most_pcp, 2)[2,]$date, ": ", 
                     head(ranking_days_most_pcp, 2)[2,]$pcp, "mm<br>",
                     head(ranking_days_most_pcp, 3)[3,]$ranking, "º ", head(ranking_days_most_pcp, 3)[3,]$date, ": ", 
                     head(ranking_days_most_pcp, 3)[3,]$pcp, "mm<br>",
                     "---------------------------<br>",
                     head(ranking_days_most_pcp |> dplyr::filter(year == selected_year), 1)$date, ": ",
                     head(ranking_days_most_pcp |> dplyr::filter(year == selected_year), 1)$pcp, "mm"
                    ),
      #hjust = 0, vjust = 0.5, label.size = 0.75, label.padding = unit(0.5, "lines"), r = unit(0.15, "lines")
      hjust = 0, vjust = 1,
      r = unit(0.15, "lines"),
      box_gp = grid::gpar(col = "black", lwd = 2),
      padding = unit(0.5, "lines")
    )) + 
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(ymd(paste0(selected_year, "-01-01")), ymd(paste0(selected_year, "-12-31")), by = "month")),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      limits = c(as.numeric(ymd(paste0(selected_year, "-01-01"))), 
                 as.numeric(ymd(paste0(as.numeric(selected_year) + 1), "-01-01"))),
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
      x = "", y = "", title = paste0("Precipitation in ", title, " ", selected_year),
      subtitle = paste0(
        "Cumulative daily precipitation vs. historical mean (",
        ref_start_year, "-", ref_end_year, ")"
      ),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter), https://pablocontreras.shinyapps.io/shiny_meteo/"
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25), 
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.75),
      legend.position = c(0.145, 0.85),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(r = 5, l = 5, b = 5),
      legend.title = element_blank()
    ) +
    ggplot2::guides(linetype = guide_legend(override.aes = list(
      linewidth = c(0.5, 0.85),
      lineend = c("square", "round")
    )))

  return(list(p, plot_data, "date", "cumsumpcp"))
  
}

