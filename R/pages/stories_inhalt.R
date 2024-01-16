#' Necessary Packages/Functions

box::use(
  ../../R/utils/stories[get_story_by_id],
  ../../R/utils/routing[get_hf_param],
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, fluidRow, tagList,
    uiOutput, renderUI,
    div, HTML,
    actionButton
  ],
  shiny.router[get_query_param, get_page, change_page]
)

#' Missing description
#' @export

module_stories_inhalt_ui <- function(id = "stories_inhalt", label = "m_stories_inhalt") {
  ns <- NS(id)
  fluidPage(
    div(
      style = "max-width: 1200px; padding: 50px; margin: 20px auto; border: 2px solid var(--grey); background-color: white",
      div(
        style = "margin-left: 90%;",
        actionButton(ns("back_button"), "zurück")
      ),
      uiOutput(ns("stories"))
    )
  )
}

#' Missing description
#' @export

module_stories_inhalt_server <- function(id = "stories_inhalt") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(
        get_query_param(), {
          if (get_page() == "stories_inhalt"){
            param_st <- get_query_param("st")
            ui_stories <-
              paste0(
                "<embed src='stories/",
                get_story_by_id(param_st),
                "' style = 'width: 100%; height: 90vh;'>"
              ) |>
              HTML()
            output$stories <- renderUI({ui_stories})
          }
        }, ignoreNULL = FALSE
      )

      observeEvent(
        input$back_button, {
          param_hf <- get_hf_param()
          if (param_hf != "") param_hf <- sprintf("?hf=%s", param_hf)
          change_page(paste0("stories", param_hf))
        }
      )

    }
  )
}
