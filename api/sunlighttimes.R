#' ---
#' title: Meteo app
#' author: Pablo Contreras
#' date: 2024-01-12
#' description: Script that gets sun light times
#' ---

library(climaemet, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(suncalc, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(readr, warn.conflicts = FALSE, quietly = TRUE)

# ************************** WARNING ************************** #
# ANTES DE AÑADIR ESTACION CREAR SU CARPETA EN LOCAL Y DROPBOX
# ************************** WARNING ************************** #
default_stations <- c("3195", "3129", "2462", "C430E", "1208H", "1249X")
ref_start_date <- "1900-01-01" 

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  stations <- args
} else {
  stations <- default_stations
}

aemet_stations <- climaemet::aemet_stations()

for (station in stations) {
  
  print(paste0("Getting sun light times for station ", station))
  # To get longitude and latitude of the station
  selected_station <- aemet_stations |>  dplyr::filter(indicativo %in% station)
  
  sunlighttimes <- suncalc::getSunlightTimes(
    date = seq.Date(from = as.Date(ref_start_date), 
                    to = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31")), by = "day"), 
    lat = selected_station$latitud, lon = selected_station$longitud, tz = "CET", 
    keep = c("sunrise", "sunset", "solarNoon", "dusk", "dawn")) |> 
    # We need to convert to character, otherwise write_csv converts timestamps to UTC
    dplyr::mutate(
      sunrise = as.character(sunrise),
      sunset = as.character(sunset),
      solarNoon = as.character(solarNoon),
      dusk = as.character(dusk),
      dawn = as.character(dawn)
    )
  
  # Save to local storage
  file <- paste0("~/aemet_data/", station, "/", format(Sys.time(),"%Y%m%d_%H%M%S"), "_", station, "_sunlighttimes.csv.gz")
  readr::write_csv(
    sunlighttimes, 
    file = file
  )
  print(paste0("Saved in local storage sun light times for station ", station, ": ", file))
  
  # Upload to Dropbox
  path <- file.path(paste0("~/aemet_data/", station, "/"))
  files <- list.files(path, pattern = "sunlighttimes")
  print(paste0("Uploading to Dropbox sun light times data for station ", station, ": ", tail(files, 1)))
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
  
}
