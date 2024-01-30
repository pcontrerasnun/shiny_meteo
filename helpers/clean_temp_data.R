CleanTempData <- function(data) {
  data_temp <- data |> # Remove years with more than 50% of tmean values missing
    dplyr::group_by(year) |> dplyr::mutate(missing_tmean = mean(is.na(tmean))) |> 
    dplyr::ungroup() |> dplyr::filter(missing_tmean < 0.5) |> 
    dplyr::select(date, day, month, year, tmin, tmax, tmean)
  
  return(data_temp)
}