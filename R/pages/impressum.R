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

module_impressum_ui <- function(id = "impressum", label = "m_impressum") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      h2("Impressum"),
      draw_under_construction()
    )
  )
}
