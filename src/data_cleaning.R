#' AEMET Open data cleaning
#'
#' It basically makes column 'prec' as numeric instead of character (new column is named 'pcp')
#' and creates useful auxiliar columns for further analyses such as ano, mes and dia.
#' From all the columns in the dataset that the AEMET API returns, it selects only the ones that
#' are going to be used.
#'
#' @param data An R dataset with AEMET Open data
#' @returns An R clean dataset
#' @examples
#' DataCleaning(data)
DataCleaning <- function(data) {
  # Join last 4 days of that data that general API doesn't provide
  path <- file.path('~/Escritorio/aemet/')
  files <- list.files(path, pattern = "\\.csv$")
  
  last_data <- data.frame()
  
  for (i in files) {
    tmp <- read.csv(paste0(path, i))
    last_data <- rbind(last_data, tmp)
  }
  
  last_data_clean <- last_data |> 
    dplyr::select(-X) |> 
    dplyr::distinct() |> 
    dplyr::arrange(fint) |> 
    dplyr::mutate(fecha = lubridate::ymd_hms(fint)) |> 
    dplyr::mutate(fecha = fecha - lubridate::hours(7)) |> 
    dplyr::mutate(fecha = format(fecha, '%Y-%m-%d')) |> 
    dplyr::group_by(fecha) |> 
    dplyr::summarise(prec = sum(prec)) |> 
    dplyr::ungroup()
  
  data_clean <- data |>
    dtplyr::lazy_dt() |>
    dplyr::select(fecha, prec) |> 
    dplyr::as_tibble() |> 
    rbind(last_data_clean) |> 
    dplyr::distinct(fecha, .keep_all = TRUE) |> # remove duplicated rows, keep last
    dtplyr::lazy_dt() |>
    dplyr::mutate(dia = format(fecha, "%d")) |>
    dplyr::mutate(mes = format(fecha, "%m")) |>
    dplyr::mutate(ano = format(fecha, "%Y")) |>
    dplyr::mutate(pcp = (ifelse(prec == "Ip", "0,0", prec))) |> # 'Ip' means precipitacion < 0.1mm
    dplyr::mutate(pcp = gsub(",", ".", pcp)) |> # Change commas with dots, necessary for numeric
    # conversion
    dplyr::mutate(pcp = as.numeric(pcp))

  return(data_clean)
}
