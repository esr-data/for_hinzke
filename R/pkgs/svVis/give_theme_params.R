

give_theme_params <- function(){

  # browser()

  return(
    list(
      sv0 = list(base_color = "#e0e8ed", bold_color = "#87aaba", text_family = "Calibri", upper_title = TRUE),
      sv1 = list(base_color = "#e0e8ed", bold_color = "#87aaba", text_family = "Calibri", upper_title = TRUE),
      sv2 = list(base_color = "#e0e8ed", bold_color = "#87aaba", text_family = "Calibri", upper_title = TRUE),
      kic = list(base_color = "#000000", bold_color = "#3A2A78", text_family = "Trebuchet MS"),
      mnt = list(base_color = "#e0e8ed", bold_color = "#87aaba", text_family = "Trebuchet MS", margins = TRUE, vjust_axis_title = TRUE),
      hfd = list(base_color = "#e0e8ed", bold_color = "#000000", text_family = "Arial", margins = TRUE, vjust_axis_title = TRUE)
    )
  )
}
