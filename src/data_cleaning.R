#' AEMET Open data cleaning
#'
#' It basically makes column 'prec' as numeric instead of character (new column is named 'pcp')
#' and creates useful auxiliar columns for further analyses such as ano, mes and dia.
#' From all the columns in the dataset that the AEMET API returns, it selects only the ones that
#' are going to be used.
#' 
#' Additionaly, function gets data for last four days (which are not available in general
#' AEMET API) from local storage and integrates them into historical data
#'
#' @param data An R dataset with AEMET Open data
#' @returns An R clean dataset
#' @examples
#' DataCleaning(data)
DataCleaning <- function(data) {
  # Join last 4 days of data that general API doesn't provide
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
    dplyr::arrange(fint) |> # UTC
    dplyr::mutate(fecha = lubridate::ymd_hms(fint)) |> 
    dplyr::mutate(fecha = fecha - lubridate::hours(7)) |> # Transform data from 00-24 to 07-07
    dplyr::mutate(fecha = format(fecha, '%Y-%m-%d')) |> 
    dplyr::group_by(fecha) |> 
    dplyr::summarise(prec = sum(prec)) |> 
    dplyr::ungroup()
  
  # Clean historical data and join with last 4 days data
  data_clean <- data |>
    dtplyr::lazy_dt() |>
    dplyr::select(fecha, prec) |> 
    dplyr::as_tibble() |> 
    rbind(last_data_clean) |> # Join last 4 days data
    dplyr::distinct(fecha, .keep_all = TRUE) |> # Remove duplicated rows, keep last
    dtplyr::lazy_dt() |>
    dplyr::mutate(day = format(fecha, "%d")) |>
    dplyr::mutate(month = format(fecha, "%m")) |>
    dplyr::mutate(year = format(fecha, "%Y")) |>
    dplyr::rename(date = fecha) |> 
    dplyr::mutate(pcp = (ifelse(prec == "Ip", "0,0", prec))) |> # 'Ip' means precipitacion < 0.1mm
    dplyr::mutate(pcp = gsub(",", ".", pcp)) |> # Change commas with dots, necessary for numeric
    # conversion
    dplyr::mutate(pcp = as.numeric(pcp)) |> 
    dplyr::select(-prec) |> 
    as_tibble()

  return(list(data_clean, tail(last_data$fint, 1)))
}
