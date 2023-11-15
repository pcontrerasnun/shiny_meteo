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
  selected_year_season_intensity_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
      date <= as.Date(paste0(as.numeric(selected_year), "-12-31"))) |>
    dplyr::mutate(season = dplyr::case_when(
      month %in% c("12", "01", "02") ~ "4-winter",
      month %in% c("03", "04", "05") ~ "1-spring",
      month %in% c("06", "07", "08") ~ "2-summer",
      month %in% c("09", "10", "11") ~ "3-autumn"
    )) |>
    dplyr::group_by(season) |>
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE), maxpcp = max(pcp, na.rm = TRUE)) |>
    dplyr::mutate(intensity = maxpcp / sumpcp * 100) |>
    dplyr::as_tibble()

  reference_season_intensity_pcp <- data |>
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
      date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::mutate(season = dplyr::case_when(
      month %in% c("12", "01", "02") ~ "4-winter",
      month %in% c("03", "04", "05") ~ "1-spring",
      month %in% c("06", "07", "08") ~ "2-summer",
      month %in% c("09", "10", "11") ~ "3-autumn"
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
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE), maxpcp = suppressWarnings(max(pcp, na.rm = TRUE)),
                     .groups = "keep") |> # for some seasons (for example, spring 1928 Madrid Retiro)
    # there is no pcp data so calculating the max of no data returns a warning
    dplyr::mutate(intensity = maxpcp / sumpcp * 100) |>
    #    dplyr::group_by(season) |>
    #    dplyr::summarise(
    #      minintensity = min(intensity, na.rm = TRUE),
    #      medianintensity = median(intensity, na.rm = TRUE),
    #      maxintensity = max(intensity, na.rm = TRUE)
    #    ) |>
    dplyr::as_tibble()

  plot_data <- dplyr::left_join(selected_year_season_intensity_pcp,
    reference_season_intensity_pcp,
    by = "season"
  )

  p <- ggplot2::ggplot(data = reference_season_intensity_pcp, aes(x = season, y = intensity)) +
    ggplot2::geom_violin(aes(fill = season), alpha = 0.5, na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = c("#b2df8a", "#e31a1c", "#ff7f00", "#a6cee3")) +
    ggplot2::geom_boxplot(width = 0.1, color = "black", alpha = 0.2, na.rm = TRUE) +
    ggplot2::geom_point(data = selected_year_season_intensity_pcp) +
    ggplot2::annotate(
      x = selected_year_season_intensity_pcp$season,
      y = selected_year_season_intensity_pcp$intensity,
      geom = "text",
      label = selected_year,
      family = "sans", hjust = -0.35, vjust = 0
    ) +
    ggplot2::scale_x_discrete(limits = c("4-winter", "1-spring", "2-summer", "3-autumn"),
                             labels = c(paste0("winter ", (as.numeric(selected_year) - 1) %% 100,
                                        "/", as.numeric(selected_year) %% 100), 
                                        "spring", "summer", "autumn")) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitation in Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Seasonal precipitation intensity vs. historical values (",
        ref_start_year, " - ", ref_end_year, ")"
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

  return(p)
}
