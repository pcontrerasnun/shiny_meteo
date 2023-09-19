library(dtplyr)

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
#' dataCleaning(data)
dataCleaning <- function(data) {
  dataClean <- data |> 
    dtplyr::lazy_dt() |> 
    dtplyr::select(fecha, prec) |> 
    dtplyr::mutate(dia = format(fecha, '%d')) |> 
    dtplyr::mutate(mes = format(fecha, '%m')) |> 
    dtplyr::mutate(ano = format(fecha, '%Y')) |> 
    dtplyr::mutate(pcp = (ifelse(prec == 'Ip', '0,0', prec))) |> # 'Ip' means precipitacion < 0.1mm
    dtplyr::mutate(pcp = gsub(',', '.', pcp)) |> # Change commas with dots, necessary for numeric 
    # conversion
    dtplyr::mutate(pcp = as.numeric(pcp))
}