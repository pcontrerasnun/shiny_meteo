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
# TBN AÑADIR ID DE ESTACIÓN EN SCRIPTS DE SUNLIGHT Y CLEANUP
# GENERAR TBN SUNLIGHTTIMES FILE
# ************************** WARNING ************************** #

# Como el API es una mierda y no deja importar todo el histórico lo que voy a
# a hacer es coger el ultimo fichero '_historical' de cada estación y pegarle los
# 6 meses siguientes a la última fecha disponible en el fichero '_historical'

default_stations <- c("3195", "3129", "2462", "C430E", "1208H", "1249X", "1059X") # indicativo

# Console arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  stations <- args
} else {
  stations <- default_stations
}

tryCatch({
  for (station in stations) {
    
    # Get historical data from last file available
    path <- file.path(paste0("~/aemet_data/", station, "/"))
    files <- list.files(path, pattern = "historical")
    print(paste0("Loading from local storage historical data for station ", station, ": ", tail(files, 1)))
    historical_data <- readr::read_csv(paste0(path, tail(files, 1)), show_col_types = FALSE) |> 
      dplyr::mutate(indicativo = as.numeric(indicativo))
    
    ref_start_date <- max(historical_data$fecha) # Ultima fecha disponible en el fichero
    ref_end_date <- max(historical_data$fecha) + lubridate::days(182) # Ultima fecha + 6 meses
    
    print(paste0("Getting from AEMET API data (from ", ref_start_date, " up to ", ref_end_date, ") for station ", station))
    
    data1 <- climaemet::aemet_daily_clim(
      station = station, start = ref_start_date, end = ref_end_date) |> 
      dplyr::mutate(indicativo = as.numeric(indicativo)) |> 
      dplyr::mutate(prec = as.character(prec)) |> 
      dplyr::mutate(horatmax = as.character(horatmax)) |> 
      dplyr::mutate(horaHrMax = as.character(horaHrMax)) |> 
      dplyr::mutate(horaHrMin = as.character(horaHrMin))
    
    historical_data_new <- dplyr::bind_rows(historical_data, data1) |> 
      dplyr::distinct(fecha, .keep_all = TRUE) |> # Remove duplicated rows, keep first
      dplyr::arrange(fecha)
  
    # Save data
    print(paste0("Saving in local storage historical data (from ", ref_start_date, " up to ", ref_end_date, ") for station ", station))
    readr::write_csv(
      historical_data_new, 
      file = paste0("~/aemet_data/", station, "/", format(Sys.time(),"%Y%m%d_%H%M%S"), "_", station, "_historical.csv.gz")
    )
  }
}, error = function(e) {
  error_message <- paste0("An error happened in script aemet_api_hist.R: ", conditionMessage(e))
  bot <- Bot(token = bot_token('aemetAlertsBot'))
  chat_id <- '111783899'
  bot$sendMessage(chat_id = chat_id, text = error_message)
})

