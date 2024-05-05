#' Necessary Packages/Functions

box::use(
  shiny[NS, moduleServer, fluidPage, div, markdown]
)

#' Missing description
#' @export

module_impressum_ui <- function(id = "impressum", label = "m_impressum") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      div(
        style = "margin: 40px; max-width: 800px;",
        markdown(readLines("md/impressum.md"))
      )
    )
  )
}
