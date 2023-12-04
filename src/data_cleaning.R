#' AEMET OpenData cleaning
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
#' @returns An R clean dataset, max date in dataset
#' @examples
#' DataCleaning(data)
DataCleaning <- function(data) {
  # Join last 4 days of data that general API doesn't provide
  path <- file.path('~/Escritorio/aemet/')
  files <- list.files(path, pattern = "\\.csv$")
  
  last_data <- data.frame()
  
  for (i in tail(files, 4)) { # only 8 last files, enough to fill the gap of 4 days
    tmp <- read.csv(paste0(path, i))
    last_data <- rbind(last_data, tmp)
  }
  
  ## Clean precipitation data
  #last_data_pcp_clean <- last_data |> 
  #  dtplyr::lazy_dt() |>
  #  dplyr::select(-X) |> 
  #  dplyr::distinct() |> 
  #  dplyr::arrange(fint) |> # UTC
  #  dplyr::mutate(fecha = lubridate::ymd_hms(fint)) |> 
  #  dplyr::mutate(fecha = fecha - lubridate::hours(7)) |> # Transform data from 00-24 to 07-07
  #  dplyr::mutate(fecha = format(fecha, '%Y-%m-%d')) |> 
  #  dplyr::group_by(fecha) |> 
  #  dplyr::summarise(prec = sum(prec)) |> 
  #  dplyr::ungroup() |> 
  #  dplyr::as_tibble()
  #
  ## Clean temperature data
  #last_data_temp_clean <- last_data |> 
  #  dtplyr::lazy_dt() |>
  #  dplyr::select(-X) |> 
  #  dplyr::distinct() |> 
  #  dplyr::arrange(fint) |> # UTC
  #  dplyr::mutate(fecha = lubridate::ymd_hms(fint)) |> 
  #  dplyr::mutate(fecha = fecha - lubridate::hours(1)) |> # Too long to explain, just trust me
  #  dplyr::mutate(fecha = format(fecha, '%Y-%m-%d')) |> 
  #  dplyr::group_by(fecha) |> 
  #  dplyr::summarise(tmin = min(tamin), tmax = max(tamax)) |> 
  #  dplyr::mutate(tmed = round((tmax + tmin) / 2, 1)) |> 
  #  dplyr::as_tibble()
  #
  ## Consolidate precipitation and temperature last data
  #last_data_clean <- left_join(last_data_pcp_clean, last_data_temp_clean, by = "fecha")
  
  # Clean historical data and join with last 4 days data
  data_clean <- data |>
    dtplyr::lazy_dt() |>
    dplyr::select(fecha, prec, tmin, tmax, tmed) |> 
    #dplyr::as_tibble() |> 
    #rbind(last_data_clean) |> # Join last 4 days of data
    dtplyr::lazy_dt() |>
    dplyr::distinct(fecha, .keep_all = TRUE) |> # Remove duplicated rows, keep first
    dplyr::mutate(day = format(fecha, "%d")) |>
    dplyr::mutate(month = format(fecha, "%m")) |>
    dplyr::mutate(year = format(fecha, "%Y")) |>
    dplyr::rename(date = fecha) |> 
    dplyr::rename(tmean = tmed) |> 
    dplyr::mutate(pcp = (ifelse(prec == "Ip", "0,0", prec))) |> # 'Ip' means precipitacion < 0.1mm
    dplyr::mutate(pcp = gsub(",", ".", pcp)) |> # Change commas with dots, necessary for numeric conversion
    dplyr::mutate(pcp = as.numeric(pcp)) |> 
    dplyr::select(-prec) |> 
    dplyr::arrange(date) |> 
    dplyr::as_tibble()

  return(list(data_clean, tail(last_data$fint, 1)))
}
