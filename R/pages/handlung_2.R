#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, tagList, h2
  ]
)

#' Missing description
#' @export

module_handlung_2_ui <- function(id = "handlung_2", label = "m_handlung_2") {
  ns <- NS(id)
  tagList(
    fluidPage(
      h2("handlung_2!")



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
