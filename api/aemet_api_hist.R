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

# ************************** WARNING ************************** #
# ANTES DE AÑADIR ESTACION CREAR SU CARPETA EN LOCAL Y DROPBOX
# ************************** WARNING ************************** #
default_stations <- c("3195", "3129", "2462", "C430E", "1208H", "1249X")
ref_start_date <- "1900-01-01" 
ref_end_date <- Sys.Date() # Get current date

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  stations <- args
} else {
  stations <- default_stations
}

for (station in stations) {
  print(paste0("Getting from AEMET API historical data (from ", ref_start_date, " up to ", Sys.Date(), ") for station ", station))
  
  if(station == "1249X") {
    data1 <- climaemet::aemet_daily_clim(
      station = station, start = ref_start_date, end = ref_end_date, verbose = TRUE)
    data2 <- climaemet::aemet_daily_clim(
      station = "1249I", start = ref_start_date, end = ref_end_date, verbose = TRUE)
    
    historical_data <- rbind(data1, data2) |> 
      dplyr::distinct(fecha, .keep_all = TRUE) |> # Remove duplicated rows, keep first
      dplyr::arrange(fecha)
    
  } else {
    historical_data <- climaemet::aemet_daily_clim(
      station = station, start = ref_start_date, end = ref_end_date, verbose = TRUE)
  }

  # Save data
  print(paste0("Saving in local storage historical data (from ", ref_start_date, " up to ", Sys.Date(), ") for station ", station))
  readr::write_csv(
    historical_data, 
    file = paste0("~/aemet_data/", station, "/", format(Sys.time(),"%Y%m%d_%H%M%S"), "_", station, "_historical.csv.gz")
  )
  
}
