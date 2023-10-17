

YearlyPcpPlot <- function(data, selected_year, ref_start_year, ref_end_year, max_date) {
  reference_anual_total_pcp <- data |>
    dplyr::filter((fecha >= as.Date(paste0(ref_start_year, "-01-01")) &
                     fecha <= as.Date(paste0(ref_end_year, "-12-31"))) |
                    (fecha >= as.Date(paste0(as.numeric(selected_year), "-01-01")) &
                       fecha <= as.Date(paste0(as.numeric(selected_year), "-12-31")))) |>
    dplyr::filter((ano != '1928') & (ano != '1938')) |> # WARNING: only for Madrid Retiro
    dplyr::group_by(ano) |> 
    dplyr::summarise(sumpcp = sum(pcp, na.rm = TRUE)) |> 
    dplyr::as_tibble() |> 
    dplyr::arrange(sumpcp)
  
  # Calculate histogram data
  h <- hist(reference_anual_total_pcp$sumpcp, breaks = 10)
  df_points = data.frame(
    x = unlist(sapply(1:length(h$mids), function(i) rep(h$mids[i], each = h$counts[i]))),
    y = unlist(sapply(h$counts, function(c) 0.5:(c-0.5)))
  )
  
  # Join data
  plot_data = cbind(df_points, reference_anual_total_pcp)
  
  # Draw the plot
  p <- ggplot2::ggplot(data = plot_data, aes(x = sumpcp)) +
    ggplot2::geom_histogram(breaks = h$breaks, color = "black", fill = "white") +
    ggplot2::geom_label(aes(x = x, y = y, label = ano)) +
    ggplot2::geom_label(data = plot_data[which(plot_data$ano == as.character(selected_year)), ],
                        aes(x = x, y = y, label = ano), color = "red", fontface = "bold") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "mm"), breaks = h$breaks) +
    ggthemes::theme_hc(base_size = 15) +
    ggplot2::labs(
      x = "", y = "", title = paste0("Precipitación en Madrid - Retiro ", selected_year),
      subtitle = paste0(
        "Precipitación anual acumulada comparada con valores históricos (",
        ref_start_year, "-", ref_end_year, ")"
      ),
      caption = paste0(
        "Actualizado: ", max_date, ", Fuente: AEMET OpenData, Elab. propia (@Pcontreras95)"
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1, face = "bold", family = "sans", size = 35),
      plot.subtitle = ggplot2::element_text(hjust = 1, size = 25), legend.position = "none"
    )
  
  return(p)
  
#  p <- ggplot2::ggplot(data = plot_data, aes(x = ano, y = sumpcp)) +
#    ggplot2::geom_col(aes(fill = sumpcp)) +
#    ggplot2::scale_fill_gradient2(high = "#2c7bb6", low = "white")
#  
#  library(MASS)
#  ajuste <- fitdistr(plot_data$sumpcp, "normal")
#  media_estimada_ajustada <- ajuste$estimate[1]
#  desviacion_estimada_ajustada <- ajuste$estimate[2]
#  
#  x <- seq(media_estimada_ajustada - 3 * desviacion_estimada_ajustada, media_estimada_ajustada + 3 * desviacion_estimada_ajustada, length = 100)
#  y <- dnorm(x, mean = media_estimada_ajustada, sd = desviacion_estimada_ajustada)
}
