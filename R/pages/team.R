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

module_team_ui <- function(id = "team", label = "m_team") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      h2("SV Data"),
      draw_under_construction()
    )
  )
}
