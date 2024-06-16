#' ---
#' title: Meteo app
#' author: Pablo Contreras
#' date: 2023-12-26
#' description: Script that consolidates historical AEMET OpenData API data
#' ---

library(climaemet, warn.conflicts = FALSE, quietly = TRUE)
library(rdrop2, warn.conflicts = FALSE, quietly = TRUE)
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(telegram.bot, warn.conflicts = FALSE, quietly = TRUE)

# ************************** WARNING ************************** #
# ANTES DE AÑADIR ESTACION CREAR SU CARPETA EN LOCAL Y DROPBOX
# ************************** WARNING ************************** #
default_stations <- c("3195", "3129", "2462", "C430E", "1208H", "1249X")
ref_start_date <- "1900-01-01" 
ref_end_date <- Sys.Date() # Get current date
intervals_6m <- seq(from = ref_start_date, to = ref_end_date, by = "6 months") # AEMET API only allows max 6 months per call
# Add ref_end_date to vector if it is not present already
if (tail(intervals_6m, 1) != ref_end_date) {
  intervals_6m <- c(intervals_6m, ref_end_date)
}
# Create date pairs
date_pairs <- list()
for (i in 1:(length(intervals_6m) - 1)) {
  date_pairs[[i]] <- list(start = intervals_6m[i], end = intervals_6m[i + 1] - 1)
}
# Caso especial para el último par de fechas
date_pairs[[length(intervals_6m) - 1]]$end <- ref_end_date

# Console arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  stations <- args
} else {
  stations <- default_stations
}

tryCatch({
  for (station in stations) {
    print(paste0("Getting from AEMET API historical data (from ", ref_start_date, " up to ", Sys.Date(), ") for station ", station))
    
    if(station == "1249X") {
      data1 <- data.frame()
      for (i in 1:length(date_pairs)) {
        data1_tmp <- climaemet::aemet_daily_clim(
          station = station, start = date_pairs[[i]]$start, end = date_pairs[[i]]$end, verbose = TRUE)
        data1 <- rbind(data1, data1_tmp)
      }
      data2 <- data.frame()
      for (i in 1:length(date_pairs)) {
        data2_tmp <- climaemet::aemet_daily_clim(
          station = station, start = date_pairs[[i]]$start, end = date_pairs[[i]]$end, verbose = TRUE)
        data2 <- rbind(data2, data2_tmp)
      }
      
      historical_data <- rbind(data1, data2) |> 
        dplyr::distinct(fecha, .keep_all = TRUE) |> # Remove duplicated rows, keep first
        dplyr::arrange(fecha)
      
    } else {
      historical_data <- data.frame()
      for (i in 1:length(date_pairs)) {
        historical_data_tmp <- climaemet::aemet_daily_clim(
          station = station, start = date_pairs[[i]]$start, end = date_pairs[[i]]$end, verbose = TRUE)
        historical_data <- rbind(historical_data, historical_data_tmp)
      }
    }
  
    # Save data
    print(paste0("Saving in local storage historical data (from ", ref_start_date, " up to ", Sys.Date(), ") for station ", station))
    readr::write_csv(
      historical_data, 
      file = paste0("~/aemet_data/", station, "/", format(Sys.time(),"%Y%m%d_%H%M%S"), "_", station, "_historical.csv.gz")
    )
  }
}, error = function(e) {
  error_message <- paste0("An error happened in script aemet_api_hist.R: ", conditionMessage(e))
  bot <- Bot(token = bot_token('aemetAlertsBot'))
  chat_id <- '111783899'
  bot$sendMessage(chat_id = chat_id, text = error_message)
})
