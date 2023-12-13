#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, tagList,
    markdown, actionButton,
    h2, div, icon
  ],
  shiny.router[change_page]
)

#' Missing description
#' @noRd

module_explorer_ui <- function(id = "explorer", label = "m_explorer", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      div(
        class = "panel-content",
        h2("Explorer", style = "text-align: center;"),
        draw_explorer_details(type),
        div(
          id = "exp_auswahl",
          style = "display: flex; flex-direction: row; flex-wrap: wrap;",
          actionButton(ns("suche"),       label = "Suchen",      class = "link_button", icon = icon("magnifying-glass")),
          actionButton(ns("indikatoren"), label = "Indikatoren", class = "link_button", icon = icon("chart-pie")),
          actionButton(ns("vergleichen"), label = "Vergleichen", class = "link_button", icon = icon("chart-bar")),
          actionButton(ns("datensatz"),   label = "Datensätze",  class = "link_button", icon = icon("chart-bar"))
        )
      )
    )
  )
}

#' Missing description
#' @noRd

module_explorer_server <- function(id = "explorer", type = "all") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$indikatoren, {change_page("indikator?in_hd=0")})
    }
  )
}

#' Missing description
#' @noRd

draw_explorer_details <- function(type){
  color_class <- "keine_handlung_triangle"
  text_in_box <- markdown(readLines("md/explorer_standard.md"))

  if (type == "handlung1") {
    color_class <- "handlung1_triangle"
  } else if (type == "handlung2") {
    color_class <- "handlung2_triangle"
  }

  return(
    div(
      style = "display: flex; flex-direction: row; margin: 1%;",
      div(class = color_class, style = "width: 200px; height: auto; min-width: 200px; min-height: 200px;"),
      div(
        style = "padding: 0 30px;",
        div(style = "width: 100%;", text_in_box)
      )
    )
  )
}
