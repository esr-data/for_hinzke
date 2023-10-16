#' Missing description
#' @noRd

module_stories_ui <- function(id = "stories", label = "m_stories", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      h2(paste("Stories", type, "!")),
      load_embedded_quarto("_site")
    )
  )
}

#' Missing description
#' @noRd

module_stories_server <- function(id = "stories", con, type = "all") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

    }
  )
}
