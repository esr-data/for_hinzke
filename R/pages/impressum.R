#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer,
    fluidPage, tagList, h2
  ]
)

#' Missing description
#' @export

module_impressum_ui <- function(id = "impressum", label = "m_impressum") {
  ns <- NS(id)
  tagList(
    fluidPage(
      h2("Impressum")

    )
  )
}
