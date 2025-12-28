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
# Y GENERAR ANTES HISTORICAL FILE (A MANO). RELLENAR TBN MISSINGS_DICT
# TBN AL STATIONS_DICT Y CHOICES DE app.R (aemet_munic())
# ************************** WARNING ************************** #
default_stations <- c("3195", "3129", "2462", "C430E", "1208H", "1249X", "1059X")
ref_start_date <- Sys.Date() - 365 
ref_end_date <- Sys.Date() # Get current date

# Console arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  stations <- args
} else {
  stations <- default_stations
}

missings_dict <- list(
  "3195" = list(pcp_na = 931, tmin_na = 218, tmax_na = 216, tmean_na = 218),
  "3129" = list(pcp_na = 435, tmin_na = 3479, tmax_na = 3480, tmean_na = 3480),
  "2462" = list(pcp_na = 32, tmin_na = 8, tmax_na = 7, tmean_na = 8),
  "C430E" = list(pcp_na = 28, tmin_na = 427, tmax_na = 408, tmean_na = 432),
  "1208H" = list(pcp_na = 2, tmin_na = 2, tmax_na = 2, tmean_na = 2),
  "1249X" = list(pcp_na = 18, tmin_na = 15, tmax_na = 15, tmean_na = 15),
  "1059X" = list(pcp_na = 49, tmin_na = 1194, tmax_na = 1193, tmean_na = 1194)
)


for (station in stations) {
  
  tryCatch({
    # --------------
    # LAST 24 HOURS
    # --------------
    # Get last 24h of data
    print(paste0("Getting from AEMET API last 24h of data for station ", station))
    tryCatch({
      last_24h_data <- climaemet::aemet_last_obs(station = station)
    },
    error = function(e) {
      print(e)
      print(paste("Could not get last 24h of data for station", station, "from AEMET API. Trying again"))
      last_24h_data <- climaemet::aemet_last_obs(station = station)
    })
    
    # Clean last 24h of data
    if ("geo850" %in% colnames(last_24h_data) && is.list(last_24h_data$geo850)) {
      last_24h_data <- last_24h_data |> 
        dplyr::mutate(geo850value = unlist(last_24h_data$geo850$value)) |> 
        dplyr::mutate(geo850present = unlist(last_24h_data$geo850$present)) |> 
        dplyr::select(-geo850)
    }
    
    # Save last 24h of data
    file <- paste0("~/aemet_data/", station, "/", format(Sys.time(),"%Y%m%d_%H%M%S"), "_", station, "_last24h.csv.gz")
    readr::write_csv(
      last_24h_data, 
      file = file
    )
    print(paste0("Saved in local storage last 24h of data for station ", station, ": ", file))
  
    }, error = function(e) {
      error_message <- paste0("Error processing station ", station, ": ", conditionMessage(e))
      print(error_message)
      bot <- Bot(token = bot_token('aemetAlertsBot'))
      chat_id <- '111783899'
      bot$sendMessage(chat_id = chat_id, text = error_message)
    })

}

for (station in stations) {
  
  tryCatch({
    # --------------
    # LAST 4 DAYS
    # --------------
    # Join last 4 days of data that general API doesn"t provide
    print(paste0("Joining last 4 days of data for station ", station))
    path <- file.path(paste0("~/aemet_data/", station, "/"))
    files <- list.files(path, pattern = "last24h")
    
    # Initialize empty dataframe
    last_4days_data <- data.frame()
    
    for (file in tail(files, 14)) { # Only 14 last files, enough to fill the gap of 4 days
      tmp <- readr::read_csv(paste0(path, file), show_col_types = FALSE) |> 
        dplyr::select(fint, prec, tamin, ta, tamax)
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
      dplyr::summarise(prec = sum(prec, na.rm = TRUE)) |> 
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
    last_4days_data_clean <- dplyr::right_join(last_4days_data_pcp_clean, last_4days_data_temp_clean, by = "fecha") |> 
      dplyr::mutate(prec = ifelse(fecha == max(fecha) & is.na(prec), 0, prec)) |> # For 0, 3 and 6am files, turn NA into 0 after right join
      dplyr::mutate(priority = 3)
    
    # ----------------------------------
    # LAST 365 DAYS (MINUS LAST 4 DAYS)
    # ----------------------------------
    # Get last year data (minus 4 last days)
    print(paste0("Getting from AEMET API last year data (minus 4 last days) for station ", station))
    tryCatch({
      last_365days_data <- climaemet::aemet_daily_clim(
          station = station, start = ref_start_date, end = ref_end_date)
    },
    error = function(e) {
      print(e)
      print(paste("Could not get last year data (minus 4 last days) for station", station, "from AEMET API. Trying again"))
      last_365days_data <- climaemet::aemet_daily_clim(
        station = station, start = ref_start_date, end = ref_end_date)
    })
    
    # ----------------------------------
    # LAST 365 DAYS (WITH LAST 4 DAYS)
    # ----------------------------------
    # Join last year data with last 4 days data and do some cleaning
    print(paste0("Joining last year data and last 4 days data for station ", station))
    last_365days_data_clean <- last_365days_data |> 
      dtplyr::lazy_dt() |>
      dplyr::select(fecha, prec, tmin, tmax, tmed) |> 
      dplyr::slice(if(is.na(prec[n()])) -n() else 1:n()) |> # If value of prec for last date is NA, delete row (it will be filled with 24h data)
      dplyr::mutate(priority = 2) |> 
      dplyr::as_tibble() |> 
      rbind(last_4days_data_clean) |> # Join last 4 days of data
      dtplyr::lazy_dt() |>
      dplyr::mutate(fecha = as.character(fecha)) |> 
      dplyr::arrange(fecha, priority) |> # Arrange by priority
      dplyr::distinct(fecha, .keep_all = TRUE) |> # Remove duplicated rows, keep first
      dplyr::as_tibble()
    
    # Save data
    file <- paste0("~/aemet_data/", station, "/", format(Sys.time(),"%Y%m%d_%H%M%S"), "_", station, "_last365days.csv.gz")
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
    historical_data <- readr::read_csv(paste0(path, tail(files, 1)), show_col_types = FALSE) |> 
      dplyr::mutate(priority = 1)
    
    # Join full last year data (last365days + last4days) and historical data
    print(paste0('Joining historical data and full last year data for station ', station))
    final_data <- historical_data |> 
      dtplyr::lazy_dt() |>
      dplyr::select(fecha, prec, tmin, tmax, tmed, priority) |> 
      dplyr::as_tibble() |> 
      rbind(last_365days_data_clean) |> # Join full last year of data
      dtplyr::lazy_dt() |>
      dplyr::arrange(fecha, priority) |> # Arrange by priority
      dplyr::distinct(fecha, .keep_all = TRUE) |> # Remove duplicated rows, keep first
      dplyr::mutate(day = format(fecha, "%d")) |>
      dplyr::mutate(month = format(fecha, "%m")) |>
      dplyr::mutate(year = format(fecha, "%Y")) |>
      dplyr::rename(date = fecha) |> 
      dplyr::rename(tmean = tmed) |> 
      dplyr::mutate(pcp = (ifelse(prec == "Ip", "0,0", as.character(prec)))) |> # 'Ip' means precipitacion < 0.1mm
      dplyr::mutate(pcp = gsub(",", ".", pcp)) |> # Change commas with dots, necessary for numeric conversion
      dplyr::mutate(pcp = as.numeric(pcp)) |> 
      dplyr::select(-prec, -priority) |> 
      dplyr::arrange(date) |> 
      dplyr::as_tibble()
    
    # --------------------------------
    # FIX MISSING PRECIPITATION DATA 
    # --------------------------------
    if (station == "3195") {
      if ((sum(is.na(final_data[final_data$date >= as.Date("2023-08-31") & final_data$date <= as.Date("2023-11-22"), ]$pcp)) == 84)) {
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
    
    if (station == "C430E") {
      if ((sum(is.na(final_data[final_data$date %in% c(as.Date("2024-03-21")), ]$pcp)) == 1)) {
        print(paste0('Fixing precipitation data for station ', station))
        date <- c("2024-03-21")
        pcp <- c(10.6)
        fix_data_pcp <- data.frame(date = as.Date(date), pcp = pcp)
        
        # Fix data
        positions <- match(fix_data_pcp$date, final_data$date)
        final_data$pcp[positions] <- fix_data_pcp$pcp
      }
    }
  
    if (station == "2462") {
      if ((sum(is.na(final_data[final_data$date %in% c(as.Date("2024-03-21")), ]$pcp)) == 1)) {
        print(paste0('Fixing precipitation data for station ', station))
        date <- c("2024-03-21")
        pcp <- c(0.0)
        fix_data_pcp <- data.frame(date = as.Date(date), pcp = pcp)
        
        # Fix data
        positions <- match(fix_data_pcp$date, final_data$date)
        final_data$pcp[positions] <- fix_data_pcp$pcp
      }
    }
  
    # -------------------------------
    # FIX MISSING TEMPERATURE DATA
    # -------------------------------
    if (station == "3195") {
      if ((sum(is.na(final_data[final_data$date %in% c(as.Date("2024-02-07"), as.Date("2024-03-06"), 
                                                       as.Date("2024-03-07"), as.Date("2024-04-29"),
                                                       as.Date("2024-04-30")), ]$tmean)) == 5)) {
        print(paste0('Fixing missing temperature data for station ', station))
        date <- c("2024-02-07", "2024-03-06", "2024-03-07", "2024-04-29", "2024-04-30")
        tmean <- c(8.3, 6.9, 8.9, 12.5, 13.8)
        tmin <- c(5.3, 3.6, 5.2, 9.1, 9.3)
        tmax <- c(11.3, 10.2, 12.6, 15.9, 18.2)
        fix_data_temp <- data.frame(date = as.Date(date), tmin = tmin, tmax = tmax, tmean = tmean)
        
        # Fix data
        positions <- match(fix_data_temp$date, final_data$date)
        final_data$tmin[positions] <- fix_data_temp$tmin
        final_data$tmax[positions] <- fix_data_temp$tmax
        final_data$tmean[positions] <- fix_data_temp$tmean
      }
    }
    
    if (station == "2462") {
      if ((sum(is.na(final_data[final_data$date %in% c(as.Date("2024-03-28")), ]$tmean)) == 1)) {
        print(paste0('Fixing missing temperature data for station ', station))
        date <- c("2024-03-28")
        tmean <- c(0.9)
        tmin <- c(-2.0)
        tmax <- c(3.8)
        fix_data_temp <- data.frame(date = as.Date(date), tmin = tmin, tmax = tmax, tmean = tmean)
        
        # Fix data
        positions <- match(fix_data_temp$date, final_data$date)
        final_data$tmin[positions] <- fix_data_temp$tmin
        final_data$tmax[positions] <- fix_data_temp$tmax
        final_data$tmean[positions] <- fix_data_temp$tmean
      }
    }
    
    if (station == "C430E") {
      if ((sum(is.na(final_data[final_data$date %in% c(as.Date("2024-03-28")), ]$tmean)) == 1)) {
        print(paste0('Fixing missing temperature data for station ', station))
        date <- c("2024-03-28")
        tmean <- c(5.3)
        tmin <- c(0.3)
        tmax <- c(10.3)
        fix_data_temp <- data.frame(date = as.Date(date), tmin = tmin, tmax = tmax, tmean = tmean)
        
        # Fix data
        positions <- match(fix_data_temp$date, final_data$date)
        final_data$tmin[positions] <- fix_data_temp$tmin
        final_data$tmax[positions] <- fix_data_temp$tmax
        final_data$tmean[positions] <- fix_data_temp$tmean
      }
    }
    
    # -------------------------------------
    # TELEGRAM NOTIFICATION - DATA QUALITY
    # -------------------------------------
    bot <- Bot(token = bot_token('aemetAlertsBot'))
    chat_id <- '111783899'
    if (sum(is.na(final_data$pcp)) > missings_dict[[station]]$pcp_na) {
      message <- paste0("Found new NA values for pcp for station ", station)
      bot$sendMessage(chat_id = chat_id, text = message)
    }
    if (sum(is.na(final_data$tmin)) > missings_dict[[station]]$tmin_na) {
      message <- paste0("Found new NA values for tmin for station ", station)
      bot$sendMessage(chat_id = chat_id, text = message)
    }
    if (sum(is.na(final_data$tmax)) > missings_dict[[station]]$tmax_na) {
      message <- paste0("Found new NA values for tmax for station ", station)
      bot$sendMessage(chat_id = chat_id, text = message)
    }
    if (sum(is.na(final_data$tmean)) > missings_dict[[station]]$tmean_na) {
      message <- paste0("Found new NA values for tmean for station ", station)
      bot$sendMessage(chat_id = chat_id, text = message)
    }
    
    # ------------
    # SAVE DATA
    # ------------
    file <- paste0("~/aemet_data/", station, "/", format(Sys.time(),"%Y%m%d_%H%M%S"), "_", station, "_complete.csv.gz")
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
        file = paste0("~/aemet_data/", station, "/", tail(files, 1)),
        path = paste0("aemet_data/", station, "/"))
      },
      error = function(e) {
        print(e)
        print(paste("Could not upload file:", tail(files, 1), "Trying again"))
        rdrop2::drop_upload(
          file = paste0("~/aemet_data/", station, "/", tail(files, 1)),
          path = paste0("aemet_data/", station, "/"))
      })
    
    # Collect garbage - free RAM
    gc()
  
  }, error = function(e) {
    error_message <- paste0("Error processing station ", station, ": ", conditionMessage(e))
    print(error_message)
    bot <- Bot(token = bot_token('aemetAlertsBot'))
    chat_id <- '111783899'
    bot$sendMessage(chat_id = chat_id, text = error_message)
  })
  
  cat("\n")
}

