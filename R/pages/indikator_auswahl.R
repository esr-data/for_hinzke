box::use(
  ../../R/utils/network[get_network_data, draw_network],
  ../../R/utils/routing[recode_parameter],
  ../../R/utils/database[get_query, get_sql],
  ../../R/utils/reactable[get_reactable_lang, get_reactable_theme],
  ../../R/utils/ui[get_waiter],
  shiny[moduleServer, NS, observeEvent, fluidPage, div, h2, h3, uiOutput, renderUI, reactiveValues, tags, HTML],
  shiny.router[get_page, change_page,get_query_param],
  visNetwork[visNetworkOutput, renderVisNetwork, visGetPositions, visNetworkProxy],
  reactable[reactable, reactableOutput, renderReactable, colDef, reactableTheme],
  shinyjs[runjs],
  urltools[param_set]
)


# Globals:
URL_PATH <- "indikator_auswahl"

#' @export
get_globals <- function(){
  list(
    url_path = URL_PATH
  )
}

#' @export
module_ui <- function(id = URL_PATH, label = paste0(URL_PATH, "_m")) {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      id    = ns("content"),
      div(
        h3("Auswahl einer Variable", style = "text-align: center;")
      ),
      div(
        class = "network-panel",
        style = "height: 500px; width: 100%; background-color: white; margin: 20px;", #padding: 2px; box-shadow: 0px 0px 5px 0px var(--grey);
        visNetworkOutput(ns("network"), width = "100%", height = "100%")
      ),
      div(
        style = "margin: 50px 20px 0 20px;",
        reactableOutput(ns("tabelle"))
      )
    )
  )
}

#' @export
module_server <- function(id = URL_PATH){
  moduleServer(
    id,
    function(input, output, session) {
      ns      <- session$ns
      network <- reactiveValues(daten = NULL)
      waiter  <- get_waiter(ns, "content")

      observeEvent(
        get_page(), {
          if (get_page() == URL_PATH & is.null(network$daten)){
            waiter$show()
            network$daten <- get_network_data()
          }
        }
      )

    observeEvent(
        network$daten, {
          output$network <- renderVisNetwork(draw_network(network$daten, ns("current_node_id"), 34))
          runjs(readLines("js/rename_visnetwork_dropdown.js") |> paste(collapse = " "))
          waiter$hide()
        }, ignoreNULL = TRUE
      )

      observeEvent(
        input$current_node_id, {
          if (get_page() %in% URL_PATH){
            current_url <- session$clientData$url_hash
            node <- unlist(input$current_node_id$nodes)
            if (is.null(node)){
              id <- ""
            } else {
              id <- network$daten$nodes$id[network$daten$nodes$id %in% node]
            }

            new_url <-
              param_set(
                urls  = current_url,
                key   = "tag",
                value = id
              )

            if (new_url != current_url){
              change_page(new_url)
            }
          }
        }
      )

      observeEvent(
        get_query_param(), {
          if (get_page() %in% URL_PATH){

            parameter_tag <-
              get_query_param("tag") |>
              recode_parameter()

            output$tabelle <-
              parameter_tag |>
              get_daten() |>
              get_reactable() |>
              renderReactable()

          }
        }
      )

    }
  )
}

get_daten <- function(parameter_tag){
  if (parameter_tag == ""){
    return(
      get_query(sprintf(get_sql("variable_by_tag"), ""))
    )
  } else {
    return(
      get_query(sprintf(get_sql("variable_by_tag"), paste("WHERE tag_link_id =", parameter_tag)))
    )
  }
}

get_reactable <- function(daten){

  variable_id       <- daten$variable_id
  daten$zeit        <- paste0("von ", daten$zeit_min, " bis ", daten$zeit_max)

  reactable(
    daten[,c("beschr", "n", "zeit")],
    highlight       = FALSE,
    searchable      = TRUE,
    filterable      = FALSE,
    borderless      = TRUE,
    fullWidth       = FALSE,
    defaultPageSize = 10,
    language        = get_reactable_lang(),
    theme           =
      reactableTheme(
        headerStyle     = list("&" = list("display" = "none")),
        paginationStyle =
          list(
            "&" =
              list(
                "font-size"     = "var(--font-size-small);",
                "border-top"    = "1.5px solid var(--blue);",
                "border-bottom" = "1.5px solid var(--blue);",
                "padding"       = "6px;",
                "margin-top"    = "8px;"
              )
          )
      ),
    columns = list(
      beschr =
        colDef(
          name = "Verfügbare Variablen",
          cell = function(value, index) {
            url <- paste0("/#!/indikator?variable=", daten$variable_id[index])
            div(
              class = "info_link",
              div("weiter zu", class = "info"),
              tags$a(href = url, target = "_blank", as.character(value))
            )
          },
          width = 800
        ),
      n =
        colDef(
          name = "",
          cell = function(value, index) paste(format(value, big.mark = ".", decimal.mark = ","), "Einträge"),
          width = 175
        ),
      zeit = colDef(name = "", width = 175)
    )
  )
}
