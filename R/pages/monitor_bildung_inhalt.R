#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer, tags,
    fluidPage, tagList, fluidRow,
    h1, h2, h3, h4, br, p, img,
    HTML, div, column,
    selectInput,
    conditionalPanel,
    radioButtons,
    numericInput,
    sidebarLayout,
    sidebarPanel,
    mainPanel,
    actionButton,
    uiOutput
  ],
  purrr[map2, map],
  DBI[dbGetQuery, dbConnect],
  RSQLite[SQLite],
  dplyr[...],
  shinyWidgets[radioGroupButtons, pickerInput, updatePickerInput],
  ../../R/utils/monitor_content[get_content_monitor_bildung]
)

content_list <- get_content_monitor_bildung()

#' Missing description
#' @export

module_monitor_bildung_inhalt_ui <- function(id = "monitor_bildung_indikator", label = "m_monitor_bildung_indikator") {
  ns <- NS(id)
  tagList(
    fluidPage(
      div(
        class = "panel-content"
        # Inhalte
      )
    )
  )
}
