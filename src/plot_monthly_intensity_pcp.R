

IntensityPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  selected_year_season_intensity_pcp <- data |> 
  dplyr::filter(fecha >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
                  fecha <= as.Date(paste0(as.numeric(selected_year), "-12-31"))) |>
  dplyr::mutate(season = dplyr::case_when(
    mes %in% c("12", "01", "02") ~ "4-invierno",
    mes %in% c("03", "04", "05") ~ "1-primavera",
    mes %in% c("06", "07", "08") ~ "2-verano",
    mes %in% c("09", "10", "11") ~ "3-otoño"
  )) |>
  dplyr::group_by(season) |> 
  dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE), maxpcp = max(pcp, na.rm = TRUE)) |> 
  dplyr::mutate(intensity = maxpcp/sumpcp * 100) |> 
  dplyr::as_tibble()

  reference_season_intensity_pcp <- data |> 
    dplyr::filter(fecha >= as.Date(paste0(ref_start_year, "-01-01")) &
                    fecha <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::mutate(season = dplyr::case_when(
      mes %in% c("12", "01", "02") ~ "4-invierno",
      mes %in% c("03", "04", "05") ~ "1-primavera",
      mes %in% c("06", "07", "08") ~ "2-verano",
      mes %in% c("09", "10", "11") ~ "3-otoño"
    )) |>
    dplyr::as_tibble() |>
    dplyr::mutate(season_aux = dplyr::case_when(
      lubridate::leap_year(fecha) == TRUE & lubridate::yday(fecha) <= 60 ~ 1,
      lubridate::leap_year(fecha) == FALSE & lubridate::yday(fecha) <= 59 ~ 1,
      TRUE ~ 0,
    )) |> 
    dtplyr::lazy_dt() |>
    dplyr::mutate(ano_season = as.numeric(ano) - season_aux) |>
    dplyr::group_by(ano_season, season) |> 
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE), maxpcp = max(pcp)) |> 
    dplyr::mutate(intensity = maxpcp/sumpcp * 100) |> 
    dplyr::group_by(season) |> 
    dplyr::summarise(
        minintensity = min(intensity, na.rm = TRUE),
        medianintensity = median(intensity, na.rm = TRUE),
        maxintensity = max(intensity, na.rm = TRUE)
    ) |> 
    dplyr::as_tibble()
  
  plot_data <- dplyr::left_join(selected_year_season_intensity_pcp,
                                reference_season_intensity_pcp, by = "season")
  
  p <- ggplot2::ggplot(data = plot_data, aes(x = season)) +
    ggplot2::geom_segment(aes(x = season, xend = season, y = 0, yend = intensity) , size=1, color="blue") +
    ggplot2::geom_ribbon(aes(ymin = minintensity, ymax = medianintensity, group = 1), alpha = 0.3,
                         color = "#d7191c", fill = "#d7191c", linetype = "51", lineend = "round",
                         linejoin = "round") +
    ggplot2::geom_ribbon(aes(ymin = medianintensity, ymax = maxintensity, group = 1), alpha = 0.3,
                         color = "blue", fill = "blue", linetype = "51", lineend = "round",
                         linejoin = "round")
  
  return(p)

}












#selected_year_monthly_numdays_pcp <- data |>
#  dplyr::filter(fecha >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
#    fecha <= as.Date(paste0(as.numeric(selected_year), "-12-31"))) |>
#  dplyr::group_by(ano, mes) |>
#  dplyr::summarise(numdayspcp = sum(pcp > 0, na.rm = TRUE)) |>
#  as_tibble()
#
#reference_monthly_numdays_pcp <- data |>
#  dplyr::filter(fecha >= as.Date(paste0(ref_start_year, "-01-01")) &
#    fecha <= as.Date(paste0(ref_end_year, "-12-31"))) |>
#  dplyr::group_by(ano, mes) |>
#  dplyr::summarise(numdayspcp = sum(pcp > 0, na.rm = TRUE)) |>
#  dplyr::group_by(mes) |>
#  dplyr::summarise(
#    minnumdayspcp = round(min(numdayspcp, na.rm = TRUE), 1),
#    maxnumdayspcp = round(max(numdayspcp, na.rm = TRUE), 1),
#    meannumdayspcp = round(mean(numdayspcp, na.rm = TRUE), 1)
#  ) |>
#  as_tibble()
#
#plot_data <- dplyr::left_join(selected_year_monthly_numdays_pcp,
#                              reference_monthly_numdays_pcp, by = "mes")
#
#ggplot2::ggplot(data = plot_data, aes(x = mes, y = numdayspcp)) +
#  ggplot2::scale_x_discrete(
#    limits = c(
#      "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"
#    ),
#    labels = c(
#      "ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"
#    )
#  ) + ggplot2::geom_line(aes(y = numdayspcp, group = 1)) +
#  ggplot2::geom_ribbon(aes(ymin = minnumdayspcp, ymax = meannumdayspcp, group = 1), alpha = 0.3,
#                       color = "#d7191c", fill = "#d7191c", linetype = "51", lineend = "round",
#                       linejoin = "round") +
#  ggplot2::geom_ribbon(aes(ymin = meannumdayspcp, ymax = maxnumdayspcp, group = 1))
