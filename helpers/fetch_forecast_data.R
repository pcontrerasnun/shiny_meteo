FetchForecastData <- function(mun_code) {
  # Get forecast data
  forecast <- climaemet::aemet_forecast_daily(mun_code)
  
  # Clean forecast data
  data_forecast <- climaemet::aemet_forecast_tidy(forecast, "temperatura") |> 
    dplyr::rename(tmax = temperatura_maxima, tmin = temperatura_minima, date = fecha) |> 
    dplyr::mutate(tmean = (tmax + tmin)/2) |> 
    dplyr::select(date, tmin, tmax, tmean)
  
  return(data_forecast)
}