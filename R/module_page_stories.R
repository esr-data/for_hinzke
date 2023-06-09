#' Missing description
#' @noRd

module_stories_ui <- function(id = "stories", label = "m_stories") {
  ns <- NS(id)
  tagList(
    fluidPage(
      h2("Stories!")



    )
  )
}

#' Missing description
#' @noRd

module_stories_server <- function(id = "stories", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

    }
  )
}
