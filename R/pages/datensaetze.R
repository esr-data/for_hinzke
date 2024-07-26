#' Necessary Packages/Functions

box::use(
  ../../R/utils/ui[draw_under_construction, create_callout_info],
  ../../R/utils/database[get_query, get_sql],
  ../../R/utils/earthworm[read_markdown],
  yaml[read_yaml],
  shiny[
    NS, moduleServer,
    fluidPage, h2, h3,
    icon, div, p,
    tagList, actionButton,
    renderUI, uiOutput, HTML, img,
    observeEvent, reactiveValues
  ],
  shinyWidgets[checkboxGroupButtons],
  shiny.router[change_page, get_page, get_query_param],
  urltools[param_get, param_set],
  shinycssloaders[withSpinner],
  shinyjs[
    removeCssClass,
    addCssClass
  ],
  reactable[reactable, renderReactable]#,
  #apexcharter[apex, aes, renderApexchart]
)

#'@noRd
get_available_datensaetze <- function(){
  meta_daten <- read_yaml("yml/explorer/datensatz.yml")
  output <-
    data.frame(
      id           = names(meta_daten),
      nr           = 1:length(meta_daten),
      label        = unlist(lapply(meta_daten, \(x) x$label)),
      beschreibung = unlist(lapply(meta_daten, \(x) x$beschreibung)),
      sql          = unlist(lapply(meta_daten, \(x) x$sql))
    )
  rownames(output) <- 1:nrow(output)
  return(output)
}

DATENSAETZE <- get_available_datensaetze()


#' Missing description
#' @export

module_datensaetze_ui <- function(id = "datensaetze", label = "m_datensaetze") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      style = "min-width: 1100px;",
      h2("Verfügbare Datensätze", style = "text-align: center"),
      div(
        style = "min-width: 600px; margin-top: 42px; display: flex; flex-direction: row",
        div(
          style = "display: flex; flex-direction: row",
          div(
            class = "library",
            style = "width: 420px; padding: 40px 0 0 40px;  z-index: 1;",
            get_datensaetze_button(ns)
          ),
          div(
            id    = ns("div_auswahl"),
            class = "eingerueckt",
            uiOutput(ns("ausgewaehlt"))
          )
        )
      ),
      uiOutput(ns("ergebnis")), #withSpinner()
      uiOutput(ns("info"))
    )
  )
}

#' Missing description
#' @export

module_datensaetze_server <- function(id = "datensaetze", label = "m_datensaetze"){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      current <-
        reactiveValues(
          parameter = list(
            id      = "start"
          )
        )

      output$ausgewaehlt <- render_hilfe()
      output$info        <- render_info()

      observeEvent(
        get_query_param(), {
          if (get_page() == "datensaetze"){

            param_id <- get_query_param("id")
            if (is.null(param_id)) param_id <- ""
            if (!(param_id %in% DATENSAETZE$id)) param_id <- ""

            if (param_id != current$parameter$id){

              current$parameter$id <- param_id
              daten                <- get_daten(param_id)

              if (param_id != ""){

                addCssClass(param_id, "markiert")
                removeCssClass("div_auswahl", "eingerueckt")

                output$info        <- renderUI(HTML(""))
                output$ergebnis    <- render_tabelle(daten)
                output$ausgewaehlt <- render_auswahl(param_id, daten, ns)

              } else {

                addCssClass("div_auswahl", "eingerueckt")

                output$ausgewaehlt <- render_hilfe()
                output$info        <- render_info()
                output$ergebnis    <- renderUI(HTML(""))

              }

              for (i in DATENSAETZE$id){
                if (!(i %in% param_id)){
                  removeCssClass(i, "markiert")
                }
              }

            }

          }
        }
      )

      observeEvent(
        input$select_dataset, {

          input_select <- abs(input$select_dataset)
          if (input_select %in% DATENSAETZE$nr){
            if (get_page() %in% "datensaetze"){

              current_url <- session$clientData$url_hash

              old_id <- as.vector(unlist(param_get(current_url, "id")))
              new_id <- DATENSAETZE$id[match(input_select, DATENSAETZE$nr)]

              if (!is.na(old_id)){
                if (old_id %in% new_id){
                  new_id <- ""
                }
              }

              new_url <-
                param_set(
                  urls  = current_url,
                  key   = "id",
                  value = new_id
                )

              if (new_url != current_url){
                change_page(new_url)
              }

            }
          }
        }
      )



    }
  )
}

#'@noRd
get_datensaetze_button <- function(ns){
  tagList(
    apply(
      DATENSAETZE, 1,
      \(x) {
        actionButton(
          ns(x["id"]),
          x["label"],
          class = "book",
          onclick = sprintf("selectDataset(%s)", x["nr"])
        )
      }
    )
  )
}

#'@noRd
render_hilfe <- function(){
  HTML(paste(readLines("www/img/datensatz_auswahl.svg"), collapse = " ")) |>
    renderUI()
}

#'@noRd
render_info <- function(){
  div(
    style = "display: flex; justify-content: center; margin-top: 24px",
    create_callout_info(read_markdown("erklarung-der-datensatze"))
  ) |>
  renderUI()
}

#'@noRd
render_auswahl <- function(id, daten, ns){
  match_id <- match(id, DATENSAETZE$id)

  div(
    style =
    "background-color: #EAE9E3;
    padding: 8px 40px;
    box-shadow: 1px 1px 5px 0.5px var(--light-grey);",
    h3(DATENSAETZE$label[match_id]),
    div(class = "strich"),
    p(DATENSAETZE$beschreibung[match_id]),
    div(
      actionButton(
        ns("speichern"),
        icon  = icon("floppy-disk", class = "fa-solid"),
        label = "",
        class = "button_icon"
      ),
      actionButton(
        ns("teilen"),
        icon  = icon("share-nodes"),
        label = "",
        class = "button_icon"
      ),
      actionButton(
        ns("bookmark"),
        icon  = icon("bookmark", class = "fa-solid"),
        label = "",
        class = "button_icon"
      )
    ),
    div(
      style = "margin: 40px",
      "TODO Facts"
    )
  ) |>
    renderUI()
}

#'@noRd
render_tabelle <- function(daten){
  renderUI(
    div(
      style = "margin-top: 40px",
      renderReactable(reactable(daten))
    )
  )
}

#'@noRd
get_daten <- function(param_id){
  if (param_id %in% "") return(data.frame())
  daten <- suppressWarnings(try(get_sql(DATENSAETZE$sql[match(param_id, DATENSAETZE$id)], TRUE)))
  if (class(daten) %in% "try-error") return(data.frame(x = "Daten nicht gefunden"))
  daten <<- daten
  return(daten)
}
