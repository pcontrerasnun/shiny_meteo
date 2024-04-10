#' ---
#' title: Meteo app
#' author: Pablo Contreras
#' date: 2024-04-09
#' description: Script that deletes old "_last24h.csv.gz" files
#' ---

library(lubridate, warn.conflicts = FALSE, quietly = TRUE)

default_stations <- c("3195", "3129", "2462", "C430E", "1208H", "1249X")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  stations <- args
} else {
  stations <- default_stations
}

clean_files <- function(station) {

  file_path <- paste0("~/aemet_data/", station)
  file_list <- list.files(path = file_path, pattern = "*last24h.csv.gz", full.names = TRUE)
  current_date <- Sys.Date()
  
  for (file in file_list) {
    # Extraer fecha y hora del nombre del fichero
    file_date <- as.Date(substr(basename(file), 1, 8), format = "%Y%m%d")
    file_hour <- substr(basename(file), 10, 11)
    
    file_age <- as.numeric(current_date - file_date)
    
    # Si el fichero tiene más de 30 días, borrarlo
    if (file_age > 30) {
      print(paste0("Deleting file: ", file))
      unlink(file)
    }
    
    # Si tiene menos de 30 días pero no es de las 00:00 o 12:00, borrarlo
    # a menos que sea el fichero más reciente del día actual
    else if (file_date < current_date & !file_hour %in% c("00","12")) {
      # Obtener el fichero más reciente para este día
      todays_files <- list.files(path = file_path, pattern = paste0("^", format(Sys.Date(), "%Y%m%d"), "_\\d{6}_\\d{4}_last24h\\.csv\\.gz$"), full.names = TRUE)
      most_recent <- todays_files[which.max(file.info(todays_files)$mtime)]
      
      # Borrar el fichero si no es el más reciente
      if (file != most_recent) {
        print(paste0("Deleting file: ", file))
        unlink(file)
      }
    }
  }
}

# Aplicar la función a cada station
sapply(stations, clean_files)
