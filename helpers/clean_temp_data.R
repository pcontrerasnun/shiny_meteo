CleanTempData <- function(data) {
  data_temp <- data |>
    dplyr::group_by(year) |> 
    dplyr::mutate(missing_tmean = mean(is.na(tmean))) |> 
    dplyr::ungroup() |> 
    dplyr::filter(missing_tmean < 0.5) |> # Remove years with more than 50% of tmean values missing
    dplyr::select(date, day, month, year, tmin, tmax, tmean)
  
  return(data_temp)
}