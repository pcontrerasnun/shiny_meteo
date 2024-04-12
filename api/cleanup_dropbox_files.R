#' ---
#' title: Meteo app
#' author: Pablo Contreras
#' date: 2024-04-11
#' description: Script that deletes old files in Dropbox
#' ---

library(rdrop2, warn.conflicts = FALSE, quietly = TRUE)
library(telegram.bot, warn.conflicts = FALSE, quietly = TRUE)

default_stations <- c("3195", "3129", "2462", "C430E", "1208H", "1249X")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  stations <- args
} else {
  stations <- default_stations
}

clean_files_dropbox <- function(station) {
  
  files <- rdrop2::drop_search(paste0(station, "_complete"), max_results = 1000)
  
  if (length(files$matches) > 0) {
    files_metadata <- lapply(files$matches, function(match) match$metadata)
    # Convertir la lista de metadatos en un data frame
    files_df <- do.call(rbind, lapply(files_metadata, as.data.frame, stringsAsFactors = FALSE))
    # Ordenar los archivos por fecha de modificación en orden descendente
    sorted_files <- files_df[order(files_df$server_modified, decreasing = TRUE), ]
    
    # Obtener los nombres de los archivos a eliminar (todos excepto los últimos 100)
    files_to_delete <- sorted_files$path_display[101:nrow(sorted_files)]
    
    # Eliminar los archivos seleccionados
    if (length(files_to_delete) > 0) {
      cat("Deleting files for", station, ":\n")
      for (file in files_to_delete) {
        rdrop2::drop_delete(file, verbose = TRUE)
        cat(file, "\n")
      }
      cat("\n")
    } else {
      cat("No hay archivos para eliminar en la estación", station, "\n")
    }
  } else {
    cat("No se encontraron archivos en la estación", station, "\n")
  }
}

# Procesar cada estación
tryCatch({
  for (station in stations) {
    clean_files_dropbox(station)
  }
}, error = function(e) {
  error_message <- paste0("An error happened in script cleanup_dropbox_files.R: ", conditionMessage(e))
  bot <- Bot(token = bot_token('aemetAlertsBot'))
  chat_id <- '111783899'
  bot$sendMessage(chat_id = chat_id, text = error_message)
})
