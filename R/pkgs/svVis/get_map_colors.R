box::use(
  grDevices[colorRamp,rgb]
)

get_map_colors <- function(var, theme) {
  
  # browser()
  
  var_type <- class(var)
  if (var_type %in% c("numeric", "integer")) {
    start_color <- theme$color[2]
    end_color <- theme$color[1]
    color_ramp <- colorRamp(c(start_color, end_color))
    met_var_range <- range(var)
    colors <-
      rgb(color_ramp((var - met_var_range[1]) / diff(met_var_range)), maxColorValue = 255)
    
  } else if (var_type %in% c("factor", "character")) {
    colors <- theme$color
    unique_values <- unlist(unique(var))
    colorscale <-
      setNames(colors[1:length(unique_values)], unique_values)
    colors <- colorscale[var]
  }
  return(colors)
}
