#' Necessary Packages/Functions

box::use(
  ../../R/utils/ui[
    draw_under_construction,
    create_callout_info,
    draw_zurueck_button,
    draw_save_and_share_buttons
  ],
  ../../R/utils/database[get_query, get_sql],
  ../../R/utils/reactable[get_reactable_lang, get_reactable_theme],
  ../../R/utils/earthworm[read_markdown],
  yaml[read_yaml],
  shiny[
    NS, moduleServer,
    fluidPage, h2, h3, h4,
    icon, div, p,
    tagList, actionButton,
    renderUI, uiOutput, HTML, img, tags,
    observeEvent, reactiveValues, reactiveVal,
    markdown
  ],
  shinyWidgets[checkboxGroupButtons, radioGroupButtons, sliderTextInput],
  shiny.router[change_page, get_page, get_query_param],
  urltools[param_get, param_set, path],
  shinycssloaders[withSpinner],
  shinyjs[
    removeCssClass,
    addCssClass
  ],
  reactable[reactable, renderReactable, reactableOutput, JS, colDef],
  rlist[list.append],
  jsonlite[toJSON, fromJSON]
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
      sql          = unlist(lapply(meta_daten, \(x) x$sql)),
      hf           = unlist(lapply(meta_daten, \(x) x$handlungsfeld)),
      optionen     = unlist(lapply(meta_daten, \(x) toJSON(x$optionen)))
    )
  rownames(output) <- 1:nrow(output)
  return(output)
}

#' Globals:
DATENSAETZE <- get_available_datensaetze()

#' @export
module_datensaetze_ui <- function(id = "datensaetze", label = "m_datensaetze") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      style = "min-width: 1100px;",
      div(
        style = "display: flex; flex-direction: row; justify-content: space-between;",
        div(
          id    = ns("triangle"),
          class = "keine_handlung_triangle",
          style = "width: 200px; height: auto; min-width: 200px; min-height: 200px;"
        ),
        uiOutput(
          style = "text-align: center; margin: auto 0; width: 100%; padding-right: 40%; padding-left: 40px;",
          ns("ueberschrift")
        ),
        div(draw_zurueck_button())
      ),
      div(
        style = "min-width: 600px; margin-top: 42px; display: flex; flex-direction: row",
        div(
          style = "display: flex; flex-direction: row",
          div(
            class = "library",
            style = "width: 420px; padding: 40px 0 0 40px;  z-index: 1;",
            render_datensaetze_button(ns)
          ),
          div(
            id    = ns("div_auswahl"),
            class = "eingerueckt",
            uiOutput(ns("ausgewaehlt"))
          )
        )
      ),
      div(
        id    = ns("reacttable_output"),
        style = "margin-top: 40px",
        uiOutput(ns("tab_ueberschrift")),
        withSpinner(reactableOutput(ns("ergebnis"))),
        uiOutput(ns("tab_fussnote"))
      ),
      div(
        id = ns("info"),
        render_info()
      )
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
          parameter  = list(
            hf       = "start",
            id       = "start",
            filter   = "start",
            time_min = "start",
            time_max = "start"
          )
        )

      observeEvent(
        get_page(), {
          if (get_page() %in% "datensaetze"){
            if (is.null(get_query_param())){

              current_url <- session$clientData$url_hash

              new_url <-
                param_set(
                  urls  = current_url,
                  key   = "hf",
                  value = 0
                ) |>
                param_set(
                  key   = "id",
                  value = ""
                )

              if (new_url != current_url){
                change_page(new_url)
              }

            }
          }
        }
      )

      observeEvent(
        get_query_param(), {

          if (get_page() %in% "datensaetze"){

            url      <- session$clientData$url_hash
            start    <- current$parameter$hf %in% "start"
            ereignis <- c()

            # Handlungsfeld
            param_hf <- param_get(url, "hf")[1,1]
            if (is.null(param_hf))    param_hf <- 0
            if (!(param_hf %in% 0:2)) param_hf <- 0

            if (!(param_hf %in% current$parameter$hf)){
              current$parameter$hf <- param_hf
              ereignis <- c(ereignis, "hf")
            }

            # Datensatz-ID
            param_id <- param_get(url, "id")[1,1]
            if (is.null(param_id)) param_id <- ""
            if (!(param_id %in% DATENSAETZE$id)) param_id <- ""

            if (!(param_id %in% current$parameter$id)){
              current$parameter$id <- param_id
              ereignis <- c(ereignis, "id")
            }

            # Filter
            param_filter <- param_get(url, "filter")[1,1]
            if (is.null(param_filter))    param_filter <- 1
            if (!(param_filter %in% 1:8)) param_filter <- 1

            if (!(param_filter %in% current$parameter$filter)){
              current$parameter$filter <- param_filter
              ereignis <- c(ereignis, "filter")
            }

            # Zeit-Minimalwert
            param_tmin <- unlist(param_get(url, "tmin")[1,1])
            if (is.null(param_tmin))    param_tmin <- 0
            if (any(is.na(param_tmin))) param_tmin <- 0

            if (!(param_tmin %in% current$parameter$time_min)){
              current$parameter$time_min <- param_tmin
              ereignis <- c(ereignis, "tmin")
            }

            # Zeit-Maximalwert
            param_tmax <- unlist(param_get(url, "tmax")[1,1])
            if (is.null(param_tmax))    param_tmax <- 0
            if (any(is.na(param_tmax))) param_tmax <- 0

            if (!(param_tmax %in% current$parameter$time_max)){
              current$parameter$time_max <- param_tmax
              ereignis <- c(ereignis, "tmax")
            }

            # Ereignis stattgefunden

            if (length(ereignis) > 0){

              if ("hf" %in% ereignis){

                if (current$parameter$hf %in% 1) {

                  output$ueberschrift <- render_h("Verfügbare Datensätze zu Bildung & Kompetenzen", "var(--color-bildung)")
                  addCssClass("triangle",    "handlung1_triangle")
                  removeCssClass("triangle", "handlung2_triangle")
                  removeCssClass("triangle", "keine_handlung_triangle")

                } else if (current$parameter$hf %in% 2) {

                  output$ueberschrift <- render_h("Verfügbare Datensätze zu Forschung & Innovation", "var(--color-forschung)")
                  addCssClass("triangle",    "handlung2_triangle")
                  removeCssClass("triangle", "handlung1_triangle")
                  removeCssClass("triangle", "keine_handlung_triangle")

                } else {

                  output$ueberschrift <- render_h("Alle verfügbaren Datensätze", "var(--blue)")
                  addCssClass("triangle",    "keine_handlung_triangle")
                  removeCssClass("triangle", "handlung1_triangle")
                  removeCssClass("triangle", "handlung2_triangle")

                }

                if (current$parameter$hf %in% 1:2){
                  id_ja   <- DATENSAETZE$id[DATENSAETZE$hf %in% current$parameter$hf]
                  id_nein <- DATENSAETZE$id[!(DATENSAETZE$hf %in% current$parameter$hf)]
                } else {
                  id_ja   <- DATENSAETZE$id
                  id_nein <- c()
                }

                for (i in id_ja){
                  removeCssClass(i, "no_display")
                }

                for (i in id_nein){
                  addCssClass(i, "no_display")
                }

                if (current$parameter$id %in% id_nein){
                  current$parameter$id <- ""
                  ereignis <- c(ereignis, "id")
                }

              }

              if ("id" %in% ereignis){

                for (i in DATENSAETZE$id){
                  if (!(i %in% current$parameter$id)){
                    removeCssClass(i, "markiert")
                  }
                }

                if (!(current$parameter$id %in% "")){

                  addCssClass("hilfe", "no_opacity")
                  addCssClass(current$parameter$id, "markiert")
                  addCssClass("info", "no_display")
                  removeCssClass("div_auswahl", "eingerueckt")
                  removeCssClass("reacttable_output", "no_display")

                  zeit <-
                    "SELECT DISTINCT(date_part('year', Zeit)) as jahr FROM datensatz_%s ORDER BY jahr DESC" |>
                    sprintf(current$parameter$id) |> #
                    get_query() |>
                    unlist()

                  zeit_min <- min(zeit)
                  zeit_max <- max(zeit)
                  if (length(zeit) > 2){
                    zeit_between <- as.vector(zeit[2])
                  } else {
                    zeit_between <- zeit_min
                  }

                  if (!start){
                    current$parameter$filter   <- 1
                  }

                  if (is.na(current$parameter$time_min) | !start){
                    current$parameter$time_min <- zeit_between
                  }

                  if (is.na(current$parameter$time_max) | !start){
                    current$parameter$time_max <- zeit_max
                  }

                  output$ausgewaehlt <-
                    render_auswahl(
                      current$parameter$id,
                      filter_auswahl = current$parameter$filter,
                      zeit_start     = zeit_min,
                      zeit_ende      = zeit_max,
                      zeit_auswahl   = c(zeit_between, zeit_max),
                      ns
                    )

                } else {

                  addCssClass("div_auswahl", "eingerueckt")
                  addCssClass("reacttable_output", "no_display")
                  removeCssClass("info", "no_display")
                  output$ausgewaehlt <- render_hilfe(ns)

                }

              }

              if (any(c("tmin", "tmax", "filter", "id") %in% ereignis)){

                if (
                  !(
                    current$parameter$id %in% "" |
                    is.na(current$parameter$time_min) | current$parameter$time_min == 0 |
                    is.na(current$parameter$time_max) | current$parameter$time_max == 0
                  )
                ){

                  filtern <- jsonlite::fromJSON(DATENSAETZE$optionen[match(current$parameter$id, DATENSAETZE$id)], simplifyVector = FALSE)
                  filtern <- unlist(filtern[[as.numeric(current$parameter$filter)]])

                  if (all(!is.na(c(current$parameter$time_min, current$parameter$time_max)))){
                    zeit <- sprintf("date_part('year', zeit) >= %s AND date_part('year', zeit) <= %s", current$parameter$time_min, current$parameter$time_max)
                  }

                  query <-
                    sprintf(
                      "SELECT *
                         FROM datensatz_%s
                         WHERE %s %s",
                      current$parameter$id,
                      zeit,
                      filtern
                    )

                  tabelle <- get_query(query)
                  if (nrow(tabelle) < 1){
                    tabelle <- data.frame("Information" = "Keine Daten gefunden")
                    output$ergebnis <- renderReactable(reactable(tabelle))
                  } else {
                    tabelle <- get_tabelle(tabelle)
                    tabelle <<- tabelle
                    tabelle$daten$details         <- ""
                    output$tab_ueberschrift <-
                      render_tab_ueberschrift(
                        current$parameter$id,
                        current$parameter$time_min,
                        current$parameter$time_max,
                        current$parameter$filter,
                        tabelle$konstant
                      )
                    output$tab_fussnote     <- render_tab_fussnote(tabelle$quelle)
                    output$ergebnis         <- render_tabelle(tabelle$daten)
                  }



                }

              }

              new_url <-
                param_set(
                  urls  = url,
                  key   = "hf",
                  value = current$parameter$hf
                ) |>
                param_set(
                  key   = "id",
                  value = current$parameter$id
                ) |>
                param_set(
                  key   = "filter",
                  value = current$parameter$filter
                ) |>
                param_set(
                  key   = "tmin",
                  value = current$parameter$time_min
                ) |>
                param_set(
                  key   = "tmax",
                  value = current$parameter$time_max
                )

              if (new_url != url){
                change_page(new_url)
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
              old_id      <- as.vector(unlist(param_get(current_url, "id")))
              new_id      <- DATENSAETZE$id[match(input_select, DATENSAETZE$nr)]

              if (!is.na(old_id)){
                if (old_id %in% new_id){
                  new_id <- ""
                }
              }

              new_url <- param_set(urls  = current_url, key   = "id", value = new_id)

              if (new_url != current_url){
                change_page(new_url)
              }

            }
          }

        }
      )

      observeEvent(
        input$filter, {
          if (get_page() %in% "datensaetze"){

            current_url <- session$clientData$url_hash
            new_url     <- param_set(urls  = current_url, key   = "filter", value = input$filter)

            if (new_url != current_url){
              change_page(new_url)
            }
          }
        }
      )

      observeEvent(
        input$zeit, {

          if (get_page() %in% "datensaetze"){

            current_url <- session$clientData$url_hash

            min_zeit    <- min(as.numeric(input$zeit))
            max_zeit    <- max(as.numeric(input$zeit))

            if (is.na(min_zeit)) min_zeit <- 0
            if (is.na(max_zeit)) max_zeit <- min_zeit

            new_url <-
              param_set(
                urls  = current_url,
                key   = "tmin",
                value = min_zeit
              ) |>
              param_set(
                key   = "tmax",
                value = max_zeit
              )

            if (new_url != current_url){
              change_page(new_url)
            }

          }

        }
      )


    }
  )
}

#'@noRd
render_datensaetze_button <- function(ns){
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
render_hilfe <- function(ns){
  div(
    id = ns("hilfe"),
    style = "transition: all .6s ease; opactiy: 1;",
    readLines("www/img/datensatz_auswahl.svg") |>
      paste(collapse = " ") |>
      HTML()
  ) |>
    renderUI()
}

#'@noRd
render_info <- function(){
  div(
    style = "display: flex; justify-content: center; margin-top: 24px",
    create_callout_info(read_markdown("erklarung-der-datensatze")),
  )
}

#'@noRd
render_auswahl <- function(id, filter_auswahl, zeit_start, zeit_ende, zeit_auswahl, ns){

  match_id <- match(id, DATENSAETZE$id)
  choices  <- names(fromJSON(DATENSAETZE$optionen[match_id]))

  if (!(all(zeit_auswahl %in% zeit_start:zeit_ende))){
    zeit_auswahl <- (zeit_ende - 1):zeit_ende
  }

  if (!(filter_auswahl %in% 1:length(choices))){
    filter_auswahl <- 1
  }

  div(
    style =
    "background-color: #EAE9E3;
    padding: 8px 40px;
    box-shadow: 1px 1px 5px 0.5px var(--light-grey);",
    h3(DATENSAETZE$label[match_id]),
    div(class = "strich"),
    markdown(DATENSAETZE$beschreibung[match_id]),
    div(
      style = "margin: 20px 0; display: flex; flex-direction: row; flex-wrap: wrap; justify-content: space-between;",
      div(
        style = "margin: 10px 40px 10px 0",
        radioGroupButtons(
          inputId      = ns("filter"),
          label        = "Optionen",
          selected     = filter_auswahl,
          choiceNames  = choices,
          choiceValues = 1:length(choices),
          checkIcon = list(
            yes =
              tags$i(
                class = "fa fa-check-square",
                style = "color: steelblue"
              ),
            no = tags$i(
              class = "fa fa-square-o",
              style = "color: steelblue"
            )
          )
        )
      ),
      div(
        style = "margin: 10px 40px 10px 0",
        sliderTextInput(
          inputId  = ns("zeit"),
          label    = "Wähle eine Zeitspanne",
          choices  = zeit_start:zeit_ende,
          selected = (zeit_auswahl[1]):(zeit_auswahl[2])
        )
      ),
      div(
        style = "margin: 10px 40px 10px 0",
        draw_save_and_share_buttons(ns)
      )
    )
  ) |>
    renderUI()
}

#'@noRd
get_tabelle <- function(daten){

  daten$id        <- NULL
  daten$Zeit      <- NULL

  daten           <- daten[,apply(daten, 2, \(.) any(!is.na(.)))]

  konstant <- names(daten)[!(names(daten) %in% c("Wert", "Variable", "Jahr", "Wert", "Einheit", "Quelle"))]
  konstant <- konstant[unlist(lapply(konstant, \(.) all(!is.na(daten[,.])) & length(unique(daten[,.])) == 1))]
  konstante_werte <- c()

  if (!is.null(konstant)){
    for (i in konstant){
      konstante_werte <- c(konstante_werte, sprintf("%s (%s)", daten[1, i], i))
      daten[,i] <- NULL
    }
  }

  quelle <- unique(unlist(strsplit(unique(daten$Quelle), ", ")))
  daten$Quelle <- NULL
  names(daten) <- gsub("g_", "", names(daten))

  return(
    list(
      daten    = daten,
      quelle   = quelle,
      konstant = konstante_werte
    )
  )
}

#'@noRd
render_h <- function(text, color){
  h2(text, style = sprintf("color: %s;", color)) |>
    renderUI()
}

#'@noRd
get_daten <- function(param_id){
  if (param_id %in% "") return(data.frame())
  daten <- suppressWarnings(try(get_sql(DATENSAETZE$sql[match(param_id, DATENSAETZE$id)], TRUE)))
  if (class(daten) %in% "try-error") return(data.frame("Fehler" = "Daten nicht gefunden"))
  return(daten)
}

#'@noRd
get_daten_cache <- function(param_id){
  if (param_id %in% "") return(data.frame())
  "SELECT * FROM datensatz_%s WHERE jahr = (SELECT max(jahr) FROM datensatz_%s)" |>
    sprintf(param_id, param_id) |>
    get_query()
}

#'@noRd
render_tab_ueberschrift <- function(label, zeit_min, zeit_max, filter, konstant){
  i <- match(label, DATENSAETZE$id)
  div(
    "Tabelle: %s %s-%s (%s)" |>
      sprintf(
        DATENSAETZE$label[i],
        zeit_min,
        zeit_max,
        names((fromJSON(DATENSAETZE$optionen[match(label, DATENSAETZE$id)], simplifyVector = FALSE))[[i]])
      ) |>
      h4() ,
    "Alle Werte beziehen sich auf %s." |>
      sprintf(paste(konstant, collapse = ", ")) |>
      p(style = "font-size: var(--font-size-small); font-style: italic;")
  ) |>
    renderUI()
}

#'@noRd
render_tab_fussnote <- function(quelle){
  div(
    style = "margin-top: 20px;",
    "Quelle(n): %s" |>
      sprintf(paste(quelle, collapse = ", ")) |>
      p()
  ) |>
    renderUI()
}


render_tabelle <- function(tabelle){

  max_nchar <- apply(tabelle, 2, \(.) max(nchar(format(.))))
  max_nchar <- round(max_nchar / sum(max_nchar) * 1000, 0)
  max_nchar[max_nchar > 500] <- 500
  max_nchar[max_nchar < 75]  <- 75
  define_cols <- list()
  for (i in 1:length(max_nchar)){
    if (names(max_nchar)[i] != "details"){
      define_cols <- list.append(define_cols, x = colDef(minWidth = unname(max_nchar[i]))) #width = max_nchar[i]
    }
  }
  names(define_cols) <- names(max_nchar)[names(max_nchar) != "details"]
  define_cols <-
    list.append(
      define_cols,
      details =
        colDef(
          name = "",
          sortable = FALSE,
          cell = function() htmltools::tags$button(tags$i(class = "fa-solid fa-plus"), class = "reactable-button-details"),
          width = 50
        )
    )
  tabelle <- tabelle[,c(1, ncol(tabelle), 2:(ncol(tabelle) - 1))]

  renderReactable(
    reactable(
      highlight       = TRUE,
      filterable      = FALSE,
      borderless      = TRUE,
      fullWidth       = TRUE,
      defaultPageSize = 25,
      theme           = get_reactable_theme(),
      language        = get_reactable_lang(),
      rowClass        = "small-font",
      tabelle,
      columns = define_cols,
      #   list(
      #   details = colDef(
      #     name = "",
      #     sortable = FALSE,
      #     cell = function() htmltools::tags$button(tags$i(class = "fa-solid fa-plus"), class = "reactable-button-details")
      #   )
      # ),
      onClick = JS("function(rowInfo, column) {
    // Only handle click events on the 'details' column
    if (column.id !== 'details') {
      return
    }

    // Display an alert dialog with details for the row
    window.alert('Details for row ' + rowInfo.index + ':\\n' + JSON.stringify(rowInfo.values, null, 2))

    // Send the click event to Shiny, which will be available in input$show_details
    // Note that the row index starts at 0 in JavaScript, so we add 1
    if (window.Shiny) {
      Shiny.setInputValue('show_details', { index: rowInfo.index + 1 }, { priority: 'event' })
    }
  }")
    )
  )
}
