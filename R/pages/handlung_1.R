#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, tagList, h2
  ]
)

#' Missing description
#' @export

module_handlung_1_ui <- function(id = "handlung_1", label = "m_handlung_1") {
  ns <- NS(id)
  tagList(
    fluidPage(
      h2("handlung_1!")



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
