#' Missing description
#' @noRd

module_explorer_ui <- function(id = "explorer", label = "m_explorer") {
  ns <- NS(id)
  tagList(
    fluidPage(
      h2("Explorer!")



    )
  )
}

#' Missing description
#' @noRd

module_explorer_server <- function(id = "explorer", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

    }
  )
}
