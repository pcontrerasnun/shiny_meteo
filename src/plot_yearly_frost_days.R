YearlyFrostDaysPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
plot_data <- data_temp |> 
  dtplyr::lazy_dt() |>
  dplyr::filter((date >= as.Date(paste0(ref_start_year, "-01-01")) &
                   date <= as.Date(paste0(ref_end_year, "-12-31"))) |
                  (date >= as.Date(paste0(as.numeric(selected_year), "-01-01")) & # Include year of study
                     date <= as.Date(paste0(as.numeric(selected_year), "-12-31")))) |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(frostdays = sum(tmin < 0, na.rm = TRUE)) |> 
  dplyr::as_tibble()

ggplot2::ggplot(data = plot_data, aes(x = year, y = frostdays)) +
  ggplot2::geom_line()
}