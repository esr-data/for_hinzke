#' Necessary Packages/Functions

box::use(
  ../../R/utils/utils[draw_under_construction],
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, tagList, h2, div
  ]
)

#' Missing description
#' @export

module_handlung_2_ui <- function(id = "handlung_2", label = "m_handlung_2") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      h2("Handlungsfeld - Forschung & Innovation"),
      draw_under_construction()
    )
  )
}

#' Missing description
#' @export

module_handlung_2_server <- function(id = "handlung_2", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

    }
  )
}
