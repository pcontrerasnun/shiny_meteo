FetchAEMETData <- function(station) {
  # Get historical data from Dropbox
  search <- rdrop2::drop_search(paste0(station, "_complete"))
  file <- search$matches[[1]]$metadata$path_display # Get last historical file
  print(paste0("Downloading from Dropbox historical data for station ", station, ": ", file))
  data_clean <- rdrop2::drop_read_csv(
    file, colClasses = c(day = "character", month = "character", date = "Date"))
  
  # Calculate datetime last data based on file name
  max_date <- paste(format(lubridate::floor_date(strptime(sub(".*/(\\d{8}_\\d{6}).*", "\\1", file), format = "%Y%m%d_%H%M%S"), unit = "hours"),
                           "%Y-%m-%d %H:%M"), "UTC")
  
  return(list(data_clean, max_date))
}