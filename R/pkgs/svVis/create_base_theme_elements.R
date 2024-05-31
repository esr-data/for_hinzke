box::use(
  ggplot2[theme_minimal, theme, element_text, element_line]
)
  

create_base_theme_elements <- function(
    base_color, bold_color, text_family,
    margins = FALSE, vjust_axis_title = FALSE,
    upper_title = FALSE) { # info: param upper_title is only for do.call in give_theme_and_color_set().
  
  
  theme_minimal() + theme(
    plot.margin = if (margins) unit(c(t = 1, r = 1, b = 1, l = 1), "cm") else NULL,
    panel.grid = element_line(color = base_color),
    text = element_text(color = bold_color, family = text_family),
    axis.title.x = if (vjust_axis_title) element_text(vjust = -1) else NULL,
    axis.title.y = if (vjust_axis_title) element_text(vjust = 5) else NULL,
    axis.text = element_text(color = bold_color, family = text_family, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(face = "italic"),
    plot.caption = element_text(vjust = 1),
    plot.caption.position = "plot"
  )
}
