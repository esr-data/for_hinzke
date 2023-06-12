#' Missing description
#' @noRd

module_explorer_ui <- function(id = "explorer", label = "m_explorer", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      h2(paste("Explorer", type, "!"))



    )
  )
}

#' Missing description
#' @noRd

module_explorer_server <- function(id = "explorer", con, type = "all") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

    }
  )
}
