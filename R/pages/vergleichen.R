#' Necessary Packages/Functions

box::use(
  ../../R/utils/ui[draw_under_construction],
  shiny[
    NS, moduleServer,
    fluidPage, tagList, h2, div
  ]
)

#' Missing description
#' @export

module_vergleichen_ui <- function(id = "vergleichen", label = "m_vergleichen") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      h2("Vergleichen"),
      draw_under_construction()
    )
  )
}
