#' ---
#' title: Meteo app
#' author: Pablo Contreras
#' date: 2024-12-22
#' description: Script that deletes old logs
#' ---

# Ruta al directorio de logs
log_directory <- "/tmp/aemet_logs"

# Obtener la lista de archivos .log y .err
log_files <- list.files(path = log_directory, pattern = "\\.(log|err)$", full.names = TRUE)

# Obtener la fecha actual
current_date <- Sys.Date()

# Función para determinar si un archivo tiene más de 6 meses
is_older_than_6_months <- function(file_name) {
  # Extraer la fecha del nombre del archivo
  # Suponiendo que el formato es YYYYMMDD_HHMM
  date_part <- sub(".*_(\\d{8})_\\d{4}\\.(log|err)$", "\\1", basename(file_name))
  
  # Convertir a fecha
  file_date <- as.Date(date_part, format = "%Y%m%d")
  
  # Calcular la diferencia en meses
  diff_months <- as.numeric(difftime(current_date, file_date, units = "weeks")) / 4.345
  
  # Retornar TRUE si la diferencia es mayor a 6 meses
  return(diff_months > 6)
}

# Filtrar archivos para eliminar
files_to_delete <- Filter(is_older_than_6_months, log_files)

# Eliminar archivos
for (file in files_to_delete) {
  file.remove(file)
  cat("Deleted:", file, "\n")
}

if (length(files_to_delete) == 0) {
  cat("No files older than 6 months were found.")
}
