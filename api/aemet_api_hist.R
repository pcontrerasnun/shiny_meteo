#' ---
#' title: Meteo app
#' author: Pablo Contreras
#' date: 2023-12-26
#' description: Script that consolidates historical AEMET OpenData API data
#' ---

library(climaemet)
library(rdrop2)
library(readr)

# ************************** WARNING ************************** #
# ANTES DE AÑADIR ESTACION CREAR SU CARPETA EN LOCAL Y DROPBOX
# ************************** WARNING ************************** #
stations <- c("3195")
ref_start_date <- "1920-01-01" 
ref_end_date <- Sys.Date() # Get current date

for (station in stations) {
  print(paste0("Getting from AEMET API historical data (from ", ref_start_date, " up to ", Sys.Date(), ") for station ", station))
  historical_data <- climaemet::aemet_daily_clim(
    station = station, start = ref_start_date, end = ref_end_date, verbose = TRUE)
  
  # Save data
  print(paste0("Saving in local storage historical data (from ", ref_start_date, " up to ", Sys.Date(), ") for station ", station))
  readr::write_csv(
    historical_data, 
    file = paste0("~/Escritorio/aemet/", station, "/", format(Sys.time(),"%Y%m%d_%H%M%S"), "_", station, "_historical.csv.gz")
  )
  
}
