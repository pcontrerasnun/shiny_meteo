CleanPcpData <- function(data) {
  data_pcp <- data |> # Remove years with more than 50% of pcp values missing
    dplyr::group_by(year) |> dplyr::mutate(missing_pcp = mean(is.na(pcp))) |> 
    dplyr::ungroup() |> dplyr::filter(missing_pcp < 0.5) |> 
    dplyr::select(date, day, month, year, pcp)
  
  return(data_pcp)
}