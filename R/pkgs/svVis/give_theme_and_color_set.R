
box::use(
  . / give_color_set_list[give_color_set_list],
  . / give_theme_params[give_theme_params],
  . / create_base_theme_elements[create_base_theme_elements]
)

give_theme_and_color_set <- function(theme_and_color_set){

  # browser()

  color_set_list <- give_color_set_list()

  if (!theme_and_color_set %in% names(color_set_list)) {
    warning(
      "Theme/Farbset nicht gefunden. Einen \u00DCberblick \u00FCber alle unterst\u00FCtzten
      Themes/Farbsets erh\u00E4lst Du mit Hilfe von
      svVis::show_svVis_themes_and_color_sets(). Das SV-Standardtheme sv0_allmain
      wird verwendet."
    )
    theme_and_color_set <- "sv0_allmain"
  }

  theme_praefix <- sub("_.+$", "", theme_and_color_set)

  theme_params <- give_theme_params()[[theme_praefix]]
  theme_selection <- do.call(create_base_theme_elements, theme_params)
  color_selection <- color_set_list[[theme_and_color_set]]

  return(
    list(
      theme = theme_selection,
      color = color_selection,
      theme_info = theme_params
    )
  )
}

