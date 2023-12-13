#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer,
    fluidPage, tagList, h2
  ]
)

#' Missing description
#' @export

module_datenschutz_ui <- function(id = "datenschutz", label = "m_datenschutz") {
  ns <- NS(id)
  tagList(
    fluidPage(
      h2("Datenschutz")

    )
  )
}
