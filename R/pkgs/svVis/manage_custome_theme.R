
box::use(
  . / give_theme_and_color_set[give_theme_and_color_set]
)

manage_custome_theme <- function(theme_and_color_set = theme_and_color_set, custom_theme_and_color_set = NULL) {


  if (is.null(custom_theme_and_color_set)) {
    selected_theme <- give_theme_and_color_set(theme_and_color_set)

  } else if (

    is.list(custom_theme_and_color_set) &
    length(custom_theme_and_color_set) >= 3 &
    inherits(custom_theme_and_color_set[[1]], "theme") &
    is.character(custom_theme_and_color_set[[2]]) &
    all(grepl("^#[0-9A-Fa-f]{6}$", custom_theme_and_color_set[[2]])) &
    is.list(custom_theme_and_color_set[[3]]) # some more proofs could be make, but ...
  ) {
    selected_theme <- custom_theme_and_color_set

  } else{
    stop(
      "Error: Paramter custom_theme_and_color_set entspricht nicht den Vorgaben, siehe Funktionsdokumentation
      zu give_theme_and_color_set."
    )
  }

  return(selected_theme)
}
