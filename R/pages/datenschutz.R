#' Necessary Packages/Functions

box::use(
  shiny[NS, moduleServer, fluidPage, div, markdown]
)

#' Missing description
#' @export

module_datenschutz_ui <- function(id = "datenschutz", label = "m_datenschutz") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      div(
        style = "margin: 40px; max-width: 800px;",
        markdown(readLines("md/datenschutz.md"))
      )
    )
  )
}
