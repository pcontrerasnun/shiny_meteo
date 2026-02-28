#' ---
#' title: Meteo app
#' author: Pablo Contreras
#' date: 2024-04-11
#' description: Script for personal debugging, you can ignore this file.
#' ALWAYS LEAVE ALL THE CODE COMMENTED OR YOU MAY HAVE ERRORS
#' ---

#data_clean <- rdrop2::drop_read_csv("/aemet_data/1249X/20260228_060150_1249X_complete.csv.gz", colClasses = c(day = "character", month = "character", date = "Date"))
#data_clean2 <- rdrop2::drop_read_csv("/aemet_data/C430E/20240408_000108_C430E_complete.csv.gz", colClasses = c(day = "character", month = "character", date = "Date"))
#data_clean <- FetchAEMETData(station = "3195") # 2462 C430E 3195
#data_temp <- CleanTempData(data = data_clean[[1]])
#data_pcp <- CleanPcpData(data = data_clean[[1]])
#
#data <- climaemet::aemet_daily_clim(
#  station = "1059X", start = Sys.Date()-180, end = Sys.Date(), verbose = TRUE)
#
#data <- climaemet::aemet_last_obs(station = "3129", verbose = TRUE)
#
#sum(is.na(data_clean$pcp)) # 14
#sum(is.na(data_clean$tmin)) # 13
#sum(is.na(data_clean$tmax)) # 13
#sum(is.na(data_clean$tmean)) # 13
#
#data <- readr::read_csv('/home/pablo/Descargas/20240213_185529_C430E_historical.csv.gz', show_col_types = FALSE)
#data <- readr::read_csv('/home/ubuntu/aemet_data/C430E/20240412_060136_C430E_last24h.csv.gz', show_col_types = FALSE)
#
#archivos <- list.files('/home/ubuntu/aemet_data/1249X/', full.names = TRUE)
#archivos_last24 <- archivos[grep("last24", archivos)]
#for (archivo in archivos_last24) {
#data <- read.csv(archivo, header = TRUE) |> 
#cat("Archivo:", basename(archivo), "\tNúmero de columnas:", ncol(data), "\n")
#}

# DESCARGAR ULT FICHERO DISPONIBLE DE DROPBOX PARA TODAS LAS STATIONS 
# Y MIRAR NUMERO DE MISSINGS
#stations <- c("3195", "3129", "2462", "C430E", "1208H", "1249X", "1059X")
#for (station in stations) {
#  files <- rdrop2::drop_search(paste0(station, "_complete"), max_results = 1000)
#  
#  files_metadata <- lapply(files$matches, function(match) match$metadata)
#  files_df <- do.call(rbind, lapply(files_metadata, as.data.frame, stringsAsFactors = FALSE))
#  sorted_files <- files_df[order(files_df$server_modified, decreasing = TRUE), ]
#  last_file_name <- sorted_files[1,]$path_lower
#  last_file_data <- rdrop2::drop_read_csv(last_file_name, 
#                                          colClasses = c(day = "character", month = "character", date = "Date"))  
#  print(paste0("---STATION ", station, " ---"))
#  print(paste0("PCP: ", sum(is.na(last_file_data$pcp)))) 
#  print(paste0("TMIN: ", sum(is.na(last_file_data$tmin)))) 
#  print(paste0("TMAX: ", sum(is.na(last_file_data$tmax)))) 
#  print(paste0("TMEAN: ", sum(is.na(last_file_data$tmean)))) 
#}

