#' Missing description
#' @noRd

module_studies_ui <- function(id = "studies", label = "m_studies") {
  ns <- NS(id)
  tagList(
    fluidPage(
      h2("Studies!")



    )
  )
}

#' Missing description
#' @noRd

module_studies_server <- function(id = "studies", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

    }
  )
}
