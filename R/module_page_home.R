#' Missing description
#' @noRd

module_home_ui <- function(id = "home", label = "m_home") {
  ns <- NS(id)
  tagList(
    fluidPage(
      h2("Home!")



    )
  )
}

#' Missing description
#' @noRd

module_home_server <- function(id = "home", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

    }
  )
}
