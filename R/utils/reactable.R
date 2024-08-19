
box::use(
  reactable[reactableLang, reactableTheme]
)

#' @noRd
get_reactable_theme <- function(){
  reactableTheme(
    headerStyle =
      list(
        "&" = list(
          "background-color" = "var(--blue)",
          "border"           = "none",
          "color"            = "white",
          "font-family"      = "var(--font-family-bold)",
          "font-weight"      = "400"
        ),
        "&:hover" =
          list(
            "background-color" = "var(--primary)"
          )
      ),
    paginationStyle =
      list(
        "&" =
          list(
            "font-size"     = "var(--font-size-small);",
            "border-top"    = "1.5px solid var(--blue);",
            "border-bottom" = "1.5px solid var(--blue);",
            "padding"       = "6px;",
            "margin-top"    = "8px;"
          )
      )
  )
}

#' @noRd
get_reactable_lang <- function(){
  reactableLang(
    pageInfo          = "{rowStart} bis {rowEnd} von {rows} Einträgen",
    pagePreviousLabel = "Vorherige Seite",
    pageNextLabel     = "Nächste Seite",
    pageNext          = "weiter",
    pagePrevious      = "zurück",
    searchPlaceholder = "Suche",
    searchLabel       = "Suche"
  )
}
