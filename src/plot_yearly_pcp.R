

YearlyPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  plot_data <- dataClean |>
    dplyr::filter((fecha >= as.Date(paste0(ref_start_year, "-01-01")) &
      fecha <= as.Date(paste0(ref_end_year, "-12-31"))) |
      (fecha >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
        fecha <= as.Date(paste0(as.numeric(selected_year), "-12-31")))) |> 
    dplyr::group_by(ano) |> 
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE)) |> 
    dplyr::as_tibble()
  
  
  p <- ggplot2::ggplot(data = plot_data, aes(x = ano, y = sumpcp)) +
    ggplot2::geom_col(aes(fill = sumpcp)) +
    ggplot2::scale_fill_gradient2(high = "#2c7bb6", low = "white")
}
