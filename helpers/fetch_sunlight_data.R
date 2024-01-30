FetchSunlightData <- function(station) {
  # Get sunlight times from Dropbox
  search <- rdrop2::drop_search(paste0(station, "_sunlighttimes"))
  file <- search$matches[[1]]$metadata$path_lower # Get last sunlight file
  print(paste0("Downloading from Dropbox sunlight data for station ", station, ": ", file))
  data_sunlight <- rdrop2::drop_read_csv(
    file, colClasses = c(date = "Date", sunrise = "POSIXct", sunset = "POSIXct", solarNoon = "POSIXct",
                         dawn = "POSIXct", dusk = "POSIXct"))
  
  return(data_sunlight)
}