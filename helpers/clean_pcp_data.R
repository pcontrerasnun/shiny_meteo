CleanPcpData <- function(data) {
  data_pcp <- data |> 
    dplyr::group_by(year) |> 
    dplyr::mutate(missing_pcp = mean(is.na(pcp))) |> 
    dplyr::ungroup() |> 
    dplyr::filter(missing_pcp < 0.5) |> # Remove years with more than 50% of pcp values missing
    dplyr::select(date, day, month, year, pcp)
  
  return(data_pcp)
}