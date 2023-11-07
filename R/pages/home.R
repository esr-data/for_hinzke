#' Missing description
#' @noRd

module_home_ui <- function(id = "home", label = "m_home") {
  ns <- NS(id)
  tagList(
    fluidPage(
      div(
        class = "panel-content",
        shiny::markdown(readLines("md/willkommen.md"))
      )
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
