box::use(
  . / manage_custome_theme[manage_custome_theme],
  magrittr[`%>%`],
  officer[
  fp_border
],
flextable[
  flextable,
  color,
  bg,
  border_outer,
  border_inner_h,
  border_inner_v,
  autofit,
  align,
  merge_v
],
dplyr[
  mutate,
  across,
  where
])

create_flextable <- function(df, theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL) {

  selected_theme <- manage_custome_theme(theme_and_color_set, custom_theme_and_color_set)
  header_color <- selected_theme$color[1]

  df <- df %>%
    mutate(across(where(is.numeric), ~ format(., decimal.mark = ",", big.mark = ".", scientific = FALSE)))

  flextable(df) %>%
    bg(bg = header_color, part = "header") %>%
    color(color = "white", part = "header") %>%
    border_outer(part = "all", border = fp_border(color = "black")) %>%
    border_inner_h(part = "all", border = fp_border(color = "black")) %>%
    border_inner_v(part = "all", border = fp_border(color = "black")) %>%
    autofit() %>%
    align(align = "center", part = "head") %>%
    align(j = 2:ncol(df), align = "center", part = "body")

}
