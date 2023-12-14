#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer,
    fluidPage, tagList, h2
  ]
)

#' Missing description
#' @export

module_vergleichen_ui <- function(id = "vergleichen", label = "m_vergleichen") {
  ns <- NS(id)
  tagList(
    fluidPage(
      h2("vergleichen")

    )
  )
}
