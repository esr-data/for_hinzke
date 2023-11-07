#' Missing description
#' @noRd

module_reader_ui <- function(id = "reader", label = "m_home") {
  ns <- NS(id)
  tagList(
    fluidPage(
      column(
        width = 10,
        h2("Home!")
      )
    )
  )
}

#' Missing description
#' @noRd

module_reader_server <- function(id = "reader", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

    }
  )
}
