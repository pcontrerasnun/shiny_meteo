
DailyTmeanPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  # Calculate percentiles of tmean across every day of the year
  reference_daily_pcts_tmean <- data |> 
    dtplyr::lazy_dt() |>
    dplyr::filter(date >= as.Date(paste0(ref_start_year, "-01-01")) &
                    date <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dplyr::filter((date < as.Date(paste0(selected_year, "-01-01")) | # Not include year of study in calculations
                     date > as.Date(paste0(selected_year, "-12-31")))) |>
    dplyr::mutate(rolling_tmean = round(zoo::rollapply(tmean, width = 2 * 15 + 1, FUN = mean, 
                                                 align = "center", fill = NA), 1)) |> 
    dplyr::group_by(day, month) |> 
    dplyr::summarise(
      q00tmean = round(quantile(rolling_tmean, probs = 0.00, na.rm = TRUE), 1),
      q05tmean = round(quantile(rolling_tmean, probs = 0.05, na.rm = TRUE), 1),
      q20tmean = round(quantile(rolling_tmean, probs = 0.20, na.rm = TRUE), 1),
      q40tmean = round(quantile(rolling_tmean, probs = 0.40, na.rm = TRUE), 1),
      q50tmean = round(quantile(rolling_tmean, probs = 0.50, na.rm = TRUE), 1),
      q60tmean = round(quantile(rolling_tmean, probs = 0.60, na.rm = TRUE), 1),
      q80tmean = round(quantile(rolling_tmean, probs = 0.80, na.rm = TRUE), 1),
      q95tmean = round(quantile(rolling_tmean, probs = 0.95, na.rm = TRUE), 1),
      q100tmean = round(quantile(rolling_tmean, probs = 1, na.rm = TRUE), 1),
      .groups = "keep" # to avoid warning
    ) |>
    # We choose 2023 as year because it doesn't have 29th Feb
    dplyr::mutate(date = as.Date(paste("2023", month, day, sep = "-"), format = "%Y-%m-%d")) |> 
    dplyr::as_tibble()
  
  # Draw the plot
  p <- ggplot2::ggplot(data = reference_daily_pcts_tmean, aes(x = date)) +
    #ggplot2::geom_line(aes(y = tmean), linewidth = 0.85, lineend = "round", na.rm = TRUE) +
    ggplot2::geom_ribbon(aes(ymin = q80tmean, ymax = q100tmean),
                         alpha = 0.3, color = "#d7191c", fill = "#d7191c", linetype = "51", 
                         lineend = "round", linejoin = "round"
    ) +
    ggplot2::geom_ribbon(aes(ymin = q60tmean, ymax = q80tmean),
                         alpha = 0.1,
                         color = "#fdae61", fill = "#fdae61", linetype = "51", lineend = "round",
                         linejoin = "round"
    ) +
    ggplot2::geom_ribbon(aes(ymin = q20tmean, ymax = q40tmean),
                         alpha = 0.1,
                         color = "#abd9e9", fill = "#abd9e9", linetype = "51", lineend = "round",
                         linejoin = "round"
    ) +
    ggplot2::geom_ribbon(aes(ymin = q00tmean, ymax = q20tmean),
                         alpha = 0.3, color = "#2c7bb6",
                         fill = "#2c7bb6", linetype = "51", lineend = "round", linejoin = "round"
    ) 
}
