library(climaemet)
library(dplyr)
library(dtplyr)
library(lubridate)
library(rdrop2)
library(readr)

# ************************** WARNING ************************** #
# ANTES DE AÑADIR ESTACION CREAR SU CARPETA EN LOCAL Y DROPBOX
# ************************** WARNING ************************** #
stations <- c("3195")
ref_start_date <- Sys.Date() - 365 
ref_end_date <- Sys.Date() # Get current date

# ~/aemet_data/
for (station in stations) {
  # --------------
  # LAST 24 HOURS
  # --------------
  # Get last 24h of data
  print(paste0("Getting last 24h of data for station ", station))
  last_24h_data <- climaemet::aemet_last_obs(station = station, verbose = TRUE)
  
  # Save last 24h of data
  file <- paste0("~/Escritorio/aemet/", station, "/", format(Sys.time(),"%Y%m%d_%H%M%S"), "_", station, "_last24h.csv.gz")
  readr::write_csv(
    last_24h_data, 
    file = file
  )
  print(paste0("Saved last 24h of data for station ", station, ": ", file))
  
  # --------------
  # LAST 4 DAYS
  # --------------
  # Join last 4 days of data that general API doesn"t provide
  print(paste0("Joining last 4 days of data for station ", station))
  path <- file.path(paste0("~/Escritorio/aemet/", station, "/"))
  files <- list.files(path, pattern = "last24h")
  
  # Initialize empty dataframe
  last_4days_data <- data.frame()
  
  for (file in tail(files, 8)) { # Only 8 last files, enough to fill the gap of 4 days
    tmp <- readr::read_csv(paste0(path, file), show_col_types = FALSE)
    last_4days_data <- rbind(last_4days_data, tmp)
  }
  
  # Clean precipitation data
  print(paste0("Cleaning last 4 days of precip. data for station ", station))
  last_4days_data_pcp_clean <- last_4days_data |> 
    dtplyr::lazy_dt() |>
    # dplyr::select(-X) |> 
    dplyr::distinct() |> 
    dplyr::arrange(fint) |> # UTC
    dplyr::mutate(fecha = lubridate::ymd_hms(fint)) |> 
    dplyr::mutate(fecha = fecha - lubridate::hours(7)) |> # Transform data from 00-24 to 07-07
    dplyr::mutate(fecha = format(fecha, "%Y-%m-%d")) |> 
    dplyr::group_by(fecha) |> 
    dplyr::summarise(prec = sum(prec)) |> 
    dplyr::ungroup() |> 
    dplyr::as_tibble()
  
  # Clean temperature data
  print(paste0("Cleaning last 4 days of temp. data for station ", station))
  last_4days_data_temp_clean <- last_4days_data |> 
    dtplyr::lazy_dt() |>
    #dplyr::select(-X) |> 
    dplyr::distinct() |> 
    dplyr::arrange(fint) |> # UTC
    dplyr::mutate(fecha = lubridate::ymd_hms(fint)) |> 
    dplyr::mutate(fecha = fecha - lubridate::hours(1)) |> # Too long to explain, just trust me
    dplyr::mutate(fecha = format(fecha, "%Y-%m-%d")) |> 
    dplyr::group_by(fecha) |> 
    dplyr::summarise(tmin = min(tamin), tmax = max(tamax)) |> 
    dplyr::mutate(tmed = round((tmax + tmin) / 2, 1)) |> 
    dplyr::as_tibble()
  
  # Consolidate precipitation and temperature last data
  print(paste0("Consolidating last 4 days of data (temp. + precip.) for station ", station))
  last_4days_data_clean <- dplyr::left_join(last_4days_data_pcp_clean, last_4days_data_temp_clean, by = "fecha")
  
  # ----------------------------------
  # LAST 365 DAYS (MINUS LAST 4 DAYS)
  # ----------------------------------
  # Get last year data (minus 4 last days)
  print(paste0("Getting last year data (minus 4 last days) for station ", station))
  last_365days_data <- climaemet::aemet_daily_clim(
    station = station, start = ref_start_date, end = ref_end_date, verbose = TRUE)
  
  # ----------------------------------
  # LAST 365 DAYS (WITH LAST 4 DAYS)
  # ----------------------------------
  # Join last year data with last 4 days data and do some cleaning
  print(paste0("Joining last year data and last 4 days data for station ", station))
  last_365days_data_clean <- last_365days_data |> 
    dtplyr::lazy_dt() |>
    dplyr::select(fecha, prec, tmin, tmax, tmed) |> 
    dplyr::as_tibble() |> 
    rbind(last_4days_data_clean) |> # Join last 4 days of data
    dtplyr::lazy_dt() |>
    dplyr::mutate(fecha = as.character(fecha)) |> 
    dplyr::distinct(fecha, .keep_all = TRUE) |> # Remove duplicated rows, keep first
    dplyr::as_tibble()
  
  # Save data
  file <- paste0("~/Escritorio/aemet/", station, "/", format(Sys.time(),"%Y%m%d_%H%M%S"), "_", station, "_last365days.csv.gz")
  readr::write_csv(
    last_365days_data_clean, 
    file = file
  )
  print(paste0("Saved full last year data for station ", station, ": ", file))
  
  # -------------------
  # ALL AVAILABLE DATA
  # -------------------
  # Get historical data
  files <- list.files(path, pattern = "historical")
  print(paste0("Loading from local historical data for station ", station, ": ", tail(files, 1)))
  historical_data <- readr::read_csv(paste0(path, tail(files, 1)), show_col_types = FALSE)
  
  # Join full last year data (last365days + last4days) and historical data
  print(paste0('Joining historical data and full last year data for station ', station))
  final_data <- historical_data |> 
    dtplyr::lazy_dt() |>
    dplyr::select(fecha, prec, tmin, tmax, tmed) |> 
    dplyr::as_tibble() |> 
    rbind(last_365days_data_clean) |> # Join full last year of data
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
  
  # Save data
  file <- paste0("~/Escritorio/aemet/", station, "/", format(Sys.time(),"%Y%m%d_%H%M%S"), "_", station, "_complete.csv.gz")
  readr::write_csv(
    final_data, 
    file = file
  )
  print(paste0("Saved all available data for station ", station, ": ", file))
  
  # Upload to Dropbox
  files <- list.files(path, pattern = "complete")
  print(paste0("Uploading to Dropbox all available data for station ", station, ": ", tail(files, 1)))
  tryCatch({
    setTimeLimit(10)
    rdrop2::drop_upload(
      file = paste0("~/Escritorio/aemet/", station, "/", tail(files, 1)),
      path = paste0("aemet_data/", station, "/"))
    },
    error = function(e) {
      print(e)
      print(paste("Could not upload file:", tail(files, 1), "Trying again"))
      rdrop2::drop_upload(
        file = paste0("~/Escritorio/aemet/", station, "/", tail(files, 1)),
        path = paste0("aemet_data/", station, "/"))
    })
  
}
