#' ---
#' title: Meteo app
#' author: Pablo Contreras
#' date: 2024-04-11
#' description: Script for personal debugging, you can ignore this file.
#' ALWAYS LEAVE ALL THE CODE COMMENTED OR YOU MAY HAVE ERRORS
#' ---

#data_clean <- rdrop2::drop_read_csv("/aemet_data/C430E/20240412_060139_C430E_complete.csv.gz", colClasses = c(day = "character", month = "character", date = "Date"))
#data_clean2 <- rdrop2::drop_read_csv("/aemet_data/C430E/20240408_000108_C430E_complete.csv.gz", colClasses = c(day = "character", month = "character", date = "Date"))
#data_clean <- FetchAEMETData(station = "3195") # 2462 C430E 3195
#data_temp <- CleanTempData(data = data_clean[[1]])
#data_pcp <- CleanPcpData(data = data_clean[[1]])
#
#data <- climaemet::aemet_daily_clim(
#  station = "0200E", start = "1900-01-01", end = Sys.Date(), verbose = TRUE)
#
#data <- climaemet::aemet_last_obs(station = "3129", verbose = TRUE)
#
#sum(is.na(data_clean$pcp)) # 14
#sum(is.na(data_temp$tmin)) # 13
#sum(is.na(data_temp$tmax)) # 13
#sum(is.na(data_temp$tmean)) # 13
#
#data <- readr::read_csv('/home/pablo/Descargas/20240213_185529_C430E_historical.csv.gz', show_col_types = FALSE)
#data <- readr::read_csv('/home/ubuntu/aemet_data/C430E/20240412_060136_C430E_last24h.csv.gz', show_col_types = FALSE)

