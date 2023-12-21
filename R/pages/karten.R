#' Necessary Packages/Functions

box::use(
  ../../R/utils/utils[draw_under_construction],
  shiny[
    NS, moduleServer,
    fluidPage, tagList, h2, div
  ]
)

#' Missing description
#' @export

module_karten_ui <- function(id = "karten", label = "m_karten") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      h2("Karten"),
      draw_under_construction()
    )
  )
}
