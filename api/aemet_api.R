#' ---
#' title: Meteo app
#' author: Pablo Contreras
#' date: 2023-12-26
#' description: Script that consolidates AEMET OpenData API data
#' ---

library(climaemet, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(dtplyr, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(rdrop2, warn.conflicts = FALSE, quietly = TRUE)
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(telegram.bot, warn.conflicts = FALSE, quietly = TRUE)

# ************************** WARNING ************************** #
# ANTES DE AÑADIR ESTACION CREAR SU CARPETA EN LOCAL Y DROPBOX
# Y GENERAR ANTES HISTORICAL FILE
# ************************** WARNING ************************** #
#stations <- c("3195", "3129", "2462")
station <- "3129"
ref_start_date <- Sys.Date() - 365 
ref_end_date <- Sys.Date() # Get current date

# ~/aemet_data/
for (station in stations) {
  # --------------
  # LAST 24 HOURS
  # --------------
  # Get last 24h of data
  print(paste0("Getting from AEMET API last 24h of data for station ", station))
  last_24h_data <- climaemet::aemet_last_obs(station = station, verbose = TRUE)
  
  # Clean last 24h of data
  if ("geo850" %in% colnames(last_24h_data)) {
    last_24h_data <- last_24h_data |> 
      mutate(geo850value = unlist(last_24h_data$geo850$value)) |> 
      mutate(geo850present = unlist(last_24h_data$geo850$present)) |> 
      select(-geo850)
  }
  
  # Save last 24h of data
  file <- paste0("~/Escritorio/aemet/", station, "/", format(Sys.time(),"%Y%m%d_%H%M%S"), "_", station, "_last24h.csv.gz")
  readr::write_csv(
    last_24h_data, 
    file = file
  )
  print(paste0("Saved in local storage last 24h of data for station ", station, ": ", file))
  
  # --------------
  # LAST 4 DAYS
  # --------------
  # Join last 4 days of data that general API doesn"t provide
  print(paste0("Joining last 4 days of data for station ", station))
  path <- file.path(paste0("~/Escritorio/aemet/", station, "/"))
  files <- list.files(path, pattern = "last24h")
  
  # Initialize empty dataframe
  last_4days_data <- data.frame()
  
  for (file in tail(files, 40)) { # Only 40 last files, enough to fill the gap of 4 days
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
  print(paste0("Getting from AEMET API last year data (minus 4 last days) for station ", station))
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
  print(paste0("Saved in local storage full last year data for station ", station, ": ", file))
  
  # -------------------
  # ALL AVAILABLE DATA
  # -------------------
  # Get historical data
  files <- list.files(path, pattern = "historical")
  print(paste0("Loading from local storage historical data for station ", station, ": ", tail(files, 1)))
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
  
  # --------------------------------
  # FIX MISSING PRECIPITATION DATA 
  # --------------------------------
  if ((sum(is.na(final_data[final_data$date >= as.Date("2023-08-31") & final_data$date <= as.Date("2023-11-22"), ]$pcp)) == 84)) {
    if(station == "3195") {
      print(paste0('Fixing precipitation data for autumn 2023 for station ', station))
      date <- c("2023-09-02", "2023-09-03", "2023-09-04", "2023-09-05", "2023-09-08", "2023-09-09", 
                "2023-09-10", "2023-09-14", "2023-09-15", "2023-09-16", "2023-09-17", "2023-09-21", 
                "2023-10-13", "2023-10-15", "2023-10-16", "2023-10-17", "2023-10-18", "2023-10-19",
                "2023-10-22", "2023-10-23", "2023-10-24", "2023-10-26", "2023-10-28", "2023-10-29",
                "2023-10-30", "2023-11-01", "2023-11-02", "2023-11-03", "2023-11-04", "2023-11-07",
                "2023-11-08", "2023-11-10")
      pcp <- c(31.2, 66.5, 9.1, 6.2, 2.8, 3.6, 7.6, 18.2, 13.3, 0.3, 6.3, 0.7, 2.4, 2.9, 0.8, 
               2.4, 6.8, 107.8, 41.2, 2.2, 1.8, 3.9, 2.0, 6.1, 0.3, 1.4, 17.9, 1.0, 1.5, 0.3, 
               0.9, 2.1)
      fix_data_pcp <- data.frame(date = as.Date(date), pcp = pcp)
  
      # Fix data
      positions <- match(fix_data_pcp$date, final_data$date)
      final_data$pcp[positions] <- fix_data_pcp$pcp
    }
  }
  
  # -------------------------------
  # FIX MISSING TEMPERATURE DATA
  # -------------------------------
  if ((sum(is.na(final_data[final_data$date == as.Date("2024-02-07"), ]$tmean)) == 1)) {
    if (station == "3195") {
      print(paste0('Fixing missing temperature data for station ', station))
      date <- c("2024-02-07")
      tmean <- c(8.3)
      tmin <- c(5.3)
      tmax <- c(11.3)
      fix_data_temp <- data.frame(date = as.Date(date), tmin = tmin, tmax = tmax, tmean = tmean)
      
      # Fix data
      positions <- match(fix_data_temp$date, final_data$date)
      final_data$tmin[positions] <- fix_data_temp$tmin
      final_data$tmax[positions] <- fix_data_temp$tmax
      final_data$tmean[positions] <- fix_data_temp$tmean
    }
  }
  
  # -----------------------
  # TELEGRAM NOTIFICATION
  # -----------------------
  bot = Bot(token = bot_token('aemetAlertsBot'))
  if (sum(is.na(subset(final_data, year == lubridate::year(Sys.Date()))$pcp)) -
      sum(is.na(subset(final_data, date == as.Date("2024-01-19"))$pcp & station == "3129")) > 0) { # For Madrid - Aeropuerto we can't fix that day
    message <- paste0("Found NA values for pcp in ", lubridate::year(Sys.Date()), " for station ", station)
    bot$sendMessage(chat_id = '111783899', text = message)
  }
  if (sum(is.na(subset(final_data, year == lubridate::year(Sys.Date()))$tmin)) > 0) {
    message <- paste0("Found NA values for tmin in ", lubridate::year(Sys.Date()), " for station ", station)
    bot$sendMessage(chat_id = '111783899', text = message)
  }
  if (sum(is.na(subset(final_data, year == lubridate::year(Sys.Date()))$tmax)) > 0) {
    message <- paste0("Found NA values for tmax in ", lubridate::year(Sys.Date()), " for station ", station)
    bot$sendMessage(chat_id = '111783899', text = message)
  }
  if (sum(is.na(subset(final_data, year == lubridate::year(Sys.Date()))$tmean)) > 0) {
    message <- paste0("Found NA values for tmean in ", lubridate::year(Sys.Date()), " for station ", station)
    bot$sendMessage(chat_id = '111783899', text = message)
  }
  
  # ------------
  # SAVE DATA
  # ------------
  file <- paste0("~/Escritorio/aemet/", station, "/", format(Sys.time(),"%Y%m%d_%H%M%S"), "_", station, "_complete.csv.gz")
  readr::write_csv(
    final_data, 
    file = file
  )
  print(paste0("Saved in local storage all available data for station ", station, ": ", file))
  
  # Upload to Dropbox
  files <- list.files(path, pattern = "complete")
  print(paste0("Uploading to Dropbox all available data for station ", station, ": ", tail(files, 1)))
  tryCatch({
    setTimeLimit(5)
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
