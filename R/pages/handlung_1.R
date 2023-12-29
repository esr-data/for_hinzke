#' Necessary Packages/Functions

box::use(
  ../../R/utils/ui[draw_under_construction],
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, tagList, h2, div
  ]
)

#' Missing description
#' @export

module_handlung_1_ui <- function(id = "handlung_1", label = "m_handlung_1") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      h2("Handlungsfeld - Bildung & Kompetenzen"),
      draw_under_construction()
    )
  )
}

#' Missing description
#' @export

module_handlung_1_server <- function(id = "handlung_1", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

    }
  )
}
