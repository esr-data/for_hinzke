#' Necessary Packages/Functions

box::use(
  ../../R/utils/stories[draw_cards],
  ../../R/utils/earthworm[read_markdown_cache],
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, fluidRow, tagList,
    uiOutput, renderUI,
    h1,
    div, HTML,
    actionButton
  ],
  shiny.router[get_query_param, get_page]
)

#' Missing description
#' @export

module_stories_ui <- function(id = "stories", label = "m_stories", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      div(
        class = "panel-content",
        uiOutput(ns("header")),
        uiOutput(ns("beschreibung")),
        uiOutput(ns("stories"))
      )
    )
  )
}

#' Missing description
#' @export

module_stories_server <- function(id = "stories", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(
        get_query_param(), {
          if (get_page() == "stories"){

            param_hf <- get_query_param("hf")

            if (is.null(param_hf)) param_hf <- 0
            if (length(param_hf) != 1) param_hf <- 0
            if (is.na(param_hf)) param_hf <- 0
            if (!(param_hf %in% 0:2)) param_hf <- 0

            if (param_hf == 1){
              ui_stories <- draw_cards(1)
              hf_header <- HTML("Stories zu<br>Bildung & Kompetenzen")
              css_class_add <- " hf1"
            } else if (param_hf == 2){
              ui_stories <- draw_cards(2)
              hf_header <- HTML("Stories zu<br>Forschung & Innovation")
              css_class_add <- " hf2"
            } else {
              ui_stories <- draw_cards(0)
              hf_header <- HTML("Datenanalysen<br>aus dem Stifterverband")
              css_class_add <- ""
            }

            output$header <-
              renderUI({
                div(
                  class = "subpage-title",
                  h1(hf_header, class = paste0("subpage-title-headline", css_class_add)),
                  div(class = paste0("header-title-clipgraph", css_class_add))
                )
              })

            output$beschreibung <- renderUI({read_markdown_cache("storybeschreibung")})

            output$stories <- renderUI({div(class = "cards-in-box", ui_stories)})
          }
        }, ignoreNULL = FALSE
      )
    }
  )
}



