#' Plot seasonal precipitation intensity
#'
#' Plots seasonal intensity calculated as max precip in a day of season / total precip in season. 
#' Current year seasons are plotted with a dot along wih a violin plot that has historical intensities 
#' (based on a reference period defined by 'ref_start_year' and 'ref_end_year')
#'
#' @param data An R dataset with AEMET Open data
#' @param selected_year Year of study
#' @param ref_start_year Start year of reference period
#' @param ref_end_year End year of reference period
#' @param max_date Max date of data
#' @returns A ggplot2 plot
#' @examples
#' IntensityPcpPlot(data, 2023, 1981, 2010, "2023-09-24")
IntensityPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate precip intensity in seasons of selected year
  selected_year_season_intensity_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
      date <= as.Date(paste0(as.numeric(selected_year), "-12-31"))) |>
    dplyr::mutate(season = dplyr::case_when(
      month %in% c("12", "01", "02") ~ "Winter",
      month %in% c("03", "04", "05") ~ "Spring",
      month %in% c("06", "07", "08") ~ "Summer",
      month %in% c("09", "10", "11") ~ "Autumn"
    )) |>
    dplyr::group_by(season) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE), sumdayspcp = sum(pcp > 0, na.rm = TRUE)) |>
    dplyr::mutate(intensity = sumpcp / sumdayspcp) |>
    dplyr::mutate(year_season = as.numeric(selected_year)) |> 
    dplyr::as_tibble()

  # Calculate precip intensity in seasons of reference period
  reference_season_intensity_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
      date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::mutate(season = dplyr::case_when(
      month %in% c("12", "01", "02") ~ "Winter",
      month %in% c("03", "04", "05") ~ "Spring",
      month %in% c("06", "07", "08") ~ "Summer",
      month %in% c("09", "10", "11") ~ "Autumn"
    )) |>
    dplyr::as_tibble() |>
    dplyr::mutate(season_aux = dplyr::case_when(
      lubridate::leap_year(date) == TRUE & lubridate::yday(date) <= 60 ~ 1,
      lubridate::leap_year(date) == FALSE & lubridate::yday(date) <= 59 ~ 1,
      TRUE ~ 0,
    )) |>
    dtplyr::lazy_dt() |>
    dplyr::mutate(year_season = as.numeric(year) - season_aux) |>
    dplyr::group_by(year_season, season) |> 
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE), sumdayspcp = sum(pcp > 0, na.rm = TRUE),
                     .groups = "keep") |> 
    dplyr::mutate(intensity = sumpcp / sumdayspcp) |>
    #    dplyr::group_by(season) |>
    #    dplyr::summarise(
    #      minintensity = min(intensity, na.rm = TRUE),
    #      medianintensity = median(intensity, na.rm = TRUE),
    #      maxintensity = max(intensity, na.rm = TRUE)
    #    ) |>
    dplyr::as_tibble()

  # Keep reference and selected year data separate so that selected year data is not included
  # in violin plot
  p <- ggplot2::ggplot(data = reference_season_intensity_pcp, 
                       aes(x = season, y = intensity, color = season)) +
    ggplot2::geom_violin(fill = "gray80", linewidth = 1, alpha = 0.5, na.rm = TRUE) +
    ggforce::geom_sina(alpha = 0.25, size = 3) + # to put points inside violin
    ggplot2::scale_color_brewer(palette = "Dark2") +
    #ggplot2::scale_fill_manual(values = c("#b2df8a", "#e31a1c", "#ff7f00", "#a6cee3")) +
    ggplot2::geom_boxplot(width = 0.1, color = "black", alpha = 0.2, na.rm = TRUE) +
    ggplot2::geom_point(data = selected_year_season_intensity_pcp, size = 3) +
    ggplot2::annotate(
      x = selected_year_season_intensity_pcp$season,
      y = selected_year_season_intensity_pcp$intensity,
      geom = "label",
      label = selected_year, fontface = "bold", size = 4,
      family = "sans", hjust = -0.35, vjust = 0
    ) +
    ggplot2::scale_x_discrete(limits = c("Winter", "Spring", "Summer", "Autumn"),
                             labels = c(paste0("Winter ", (as.numeric(selected_year) - 1) %% 100,
                                        "/", as.numeric(selected_year) %% 100), 
                                        "Spring", "Summer", "Autumn")) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation in Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Seasonal precipitation intensity vs. historical values (",
        ref_start_year, "-", ref_end_year, ")"
      ),
      caption = paste0(
        "Updated: ", max_date, " | Source: AEMET OpenData | Graph: @Pcontreras95 (Twitter)"
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25), 
      legend.position = "none"
    )

  return(list(p, rbind(reference_season_intensity_pcp, selected_year_season_intensity_pcp), "season", "intensity"))
  
}
