FetchForecastData <- function(mun_code) {
  # Get forecast data
  forecast <- climaemet::aemet_forecast_daily(mun_code)
  
  return(forecast)
}