CumPcpPctsPlot <- function(data, selected_year, ref_start_year, ref_end_year) {
  pcts_pcp <- data |>
    dtplyr::filter(fecha >= as.Date(paste0(ref_start_year, "-01-01")) &
      fecha <= as.Date(paste0(ref_end_year, "-12-31"))) |>
    dtplyr::group_by(ano) |>
    dtplyr::mutate(cumsumpcp = cumsum(pcp)) |>
    dtplyr::arrange(ano, mes, dia) |>
    dtplyr::group_by(dia, mes) |>
    dtplyr::summarise(
      cumq00pcp = round(quantile(cumsumpcp, probs = 0.00, na.rm = TRUE), 1),
      cumq05pcp = round(quantile(cumsumpcp, probs = 0.05, na.rm = TRUE), 1),
      cumq20pcp = round(quantile(cumsumpcp, probs = 0.20, na.rm = TRUE), 1),
      cumq40pcp = round(quantile(cumsumpcp, probs = 0.40, na.rm = TRUE), 1),
      cumq50pcp = round(quantile(cumsumpcp, probs = 0.50, na.rm = TRUE), 1),
      cumq60pcp = round(quantile(cumsumpcp, probs = 0.60, na.rm = TRUE), 1),
      cumq80pcp = round(quantile(cumsumpcp, probs = 0.80, na.rm = TRUE), 1),
      cumq95pcp = round(quantile(cumsumpcp, probs = 0.95, na.rm = TRUE), 1),
      cumq100pcp = round(quantile(cumsumpcp, probs = 1, na.rm = TRUE), 1)
    ) |>
    dtplyr::arrange(mes, dia)
}
