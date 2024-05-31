
box::use(
  stringr[str_c],
  stringi[stri_wrap]
)

transform_title_letter_case_by_theme <- function(plot_title, selected_theme = selected_theme) {
  ifelse(
    selected_theme[["theme_info"]][["upper_title"]],
    str_c(stri_wrap(toupper(plot_title), width = 160), collapse = "\n"),
    str_c(stri_wrap(plot_title, width = 160), collapse = "\n")
  )
}
