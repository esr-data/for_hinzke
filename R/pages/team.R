#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer,
    fluidPage, tagList, h2
  ]
)

#' Missing description
#' @export

module_team_ui <- function(id = "team", label = "m_team") {
  ns <- NS(id)
  tagList(
    fluidPage(
      h2("SV Data Team")

    )
  )
}
