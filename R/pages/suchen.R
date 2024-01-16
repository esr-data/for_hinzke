#' Necessary Packages/Functions

box::use(
  ../../R/utils/routing[add_param_in_url],
  ../../R/utils/database[get_query, search_database, get_cache_labels],
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, fluidRow, tagList,
    uiOutput,
    h2, h3, div, icon,
    renderUI,
    reactiveValues,
    HTML
  ],
  shinyWidgets[searchInput, updateSearchInput],
  shinycssloaders[withSpinner],
  reactable[reactable, reactableLang, colDef],
  shiny.router[get_page, get_query_param, change_page],
  utils[URLencode, URLdecode]
)

#' Missing description
#' @noRd

module_suchen_ui <- function(id = "suchen", label = "m_suchen", type = "all") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      style = "min-height: 800px;",
      div(
        style = "display: flex; flex-direction: row; margin: 1%;",
        div(
          class = "keine_handlung_triangle",
          style = "width: 200px; height: auto; min-width: 200px; min-height: 200px;"
        ),
        div(
          style = "width: 100%",
          h2(
            style = "text-align: center",
            "Suche"
          ),
          div(
            style = "max-width: 700px; padding: 10px; display: flex; margin: 0 auto;",
            searchInput(
              inputId     = ns("suchen"),
              placeholder = "Begriffe in der Datenbank suchen ...",
              btnSearch   = icon("search"),
              btnReset    = icon("remove"),
              width       = "100%"
            ) #|>
              # as.character() |>
              # gsub(pattern = "!important", replacement = "", fixed = TRUE) |>
              # HTML()
          )
        )
      ),
      div(
        style = "padding: 10px; margin-top: 20px; margin-bottom: 20px;",
        withSpinner(
          uiOutput(ns("results")),
          type = 4,
          color = "#195365"
        )
      )
    )
  )
}

#' Missing description
#' @noRd

module_suchen_server <- function(id = "suchen", type = "all") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns


      current <-
        reactiveValues(
          parameter = list(
            suchwort = "start"
          ),
          suche = ""
        )

      observeEvent(
        get_query_param(), {
          if (get_page() == "suchen"){

            suchwort <- get_query_param("term")
            if (is.null(suchwort)){
              suchwort <- ""
            } else if (length(suchwort) != 1){
              suchwort <- ""
            } else if (is.na(suchwort)){
              suchwort <- ""
            }

            if (suchwort != current$parameter$suchwort){
              current$parameter$suchwort <- suchwort

              input_suchen <- input$suchen
              if (is.null(input_suchen)) input_suchen <- ""
              if (any(is.na(input_suchen)) | length(input_suchen) != 1) input_suchen <- ""

              if (URLdecode(suchwort) != input_suchen){
                updateSearchInput(
                  session = session,
                  inputId = "suchen",
                  value = URLdecode(suchwort)
                )
              }

              if (suchwort != ""){
                current$suche <- URLdecode(suchwort)
              } else {
                current$suche <- ""
              }
            }
          }
        }
      )

      observeEvent(
        input$suchen, {
          if (get_page() == "suchen"){
            change_page(
              add_param_in_url(
                current_url  = session$clientData$url_hash,
                current_page = "suchen",
                parameter    = "term",
                value        = URLencode(input$suchen),
                old_value    = get_query_param("term")
              )
            )
          }
        }
      )

      observeEvent(
        current$suche, {
          if (current$suche != ""){
            output$results <-
              renderUI({
                results <- search_database(current$suche)
                div(
                  h3(
                    style = "margin-bottom: 28px;",
                    sprintf("Suchergebnisse für '%s':", current$suche)
                  ),
                  draw_result_table(results)
                )
              })
          } else {
            output$results <- renderUI({HTML("")})
          }
        }
      )

    }
  )
}

draw_result_table <- function(results){

  results <-
    results[,c("db_content", "column", "ranking_weight")] |>
    as.data.frame()

  search_columns <- get_cache_labels()
  results$column <- search_columns$label[match(results$column, search_columns$bez)]

  reactable(
    results,
    highlight = TRUE,
    defaultPageSize = 25,
    columns = list(
      db_content = colDef(
        name = "Treffer",
        minWidth = 250
      ),
      column = colDef(
        name = "Gefunden in"
      ),
      ranking_weight = colDef(
        name = "Trefferqualität",
        defaultSortOrder = "desc",
        cell = function(value) {
          color <- ifelse(value > .9, "#91BEA0", ifelse(value > .7, "#b5bfc5", "#e73f0c"))
          sprintf(
            "<div style = 'border: 1px solid black; background-color: #b5bfc544; width: %s'><div style = 'background-color: %s; height: 20px; width: %s'></div></div>",
            "100%",
            color,
            paste0(value * 100, "%")
          )
        },
        html = TRUE
      )
    ),
    language =
      reactableLang(
        pageNext = "Vor",
        pagePrevious = "Zurück",
        pageInfo = "{rowStart}\u2013{rowEnd} von {rows} Treffern",
        noData = "Keine Einträge in der Datenbank gefunden."
      )
  )
}
