

#' AEMET Open data cleaning
#'
#' It basically makes column 'prec' as numeric instead of character (new column is named 'pcp')
#' and creates useful auxiliar columns for further analyses such as ano, mes and dia.
#' From all the columns in the dataset that the AEMET API returns, it selects only the ones that
#' are going to be used.
#'
#' @param data An R dataset
#' @returns An R clean dataset
#' @examples
#' DataCleaning(data)
DataCleaning <- function(data) {
  dataClean <- data |>
    dtplyr::lazy_dt() |>
    dplyr::select(fecha, prec) |>
    dplyr::mutate(dia = format(fecha, "%d")) |>
    dplyr::mutate(mes = format(fecha, "%m")) |>
    dplyr::mutate(ano = format(fecha, "%Y")) |>
    dplyr::mutate(pcp = (ifelse(prec == "Ip", "0,0", prec))) |> # 'Ip' means precipitacion < 0.1mm
    dplyr::mutate(pcp = gsub(",", ".", pcp)) |> # Change commas with dots, necessary for numeric
    # conversion
    dplyr::mutate(pcp = as.numeric(pcp))

  return(dataClean)
}
