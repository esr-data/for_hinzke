box::use(
  ../../R/utils/database[get_query, get_sql],
  ../../R/utils/ui[get_waiter, send_message],
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, div,
    h4, p, HTML, tagList, actionButton,
    uiOutput, renderUI, reactiveValues
  ],
  shiny.router[get_page, change_page],
  shinyjs[runjs, addCssClass, removeCssClass],
  urltools[param_set, param_get],
  stats[aggregate]
)

# Globals:
URL_PATH <- "suchen_auswahl"
TABELLE  <- get_query("SELECT * FROM tabelle WHERE bez IN ('variable', 'reichweite', 'reichweite_typ', 'wert_einheit')")

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
      style = "min-height: 500px",
      uiOutput(ns("auswahl"))
    )
  )
}

#' @export
module_server <- function(id = URL_PATH){
  moduleServer(
    id,
    function(input, output, session) {

      ns      <- session$ns
      waiter  <- get_waiter(ns, "content")
      current <- reactiveValues(auswahl = c(), daten = data.frame())

      observeEvent( # UI laden und Auswahl zur Verfügung stellen
        get_page(), {
          alles_richtig <- FALSE # Wenn Input vorhanden ist und die URL passt -> TRUE

          if (get_page() == URL_PATH){
            waiter$show()

            input <- param_get(session$clientData$url_hash, "input")$input

            if (any(!is.na(input))){
              alles_richtig <- TRUE

              current$daten   <- get_daten(input)
              current$auswahl <- make_auswahl_ok(current$daten, c())
              output$auswahl  <- renderUI(draw_auswahl(current$daten, current$auswahl, ns))

              output$auswahl_reichweiten <- renderUI(get_reichweite(current$daten, current$auswahl, ns))
              runjs("Shiny.onInputChange('suchen_auswahl-select_suche', 'trigger');")
            }

            waiter$hide()
          }

          if (!alles_richtig){
            current$daten   <- data.frame()
            current$auswahl <- c()
            output$auswahl  <- renderUI(HTML(""))
            if (get_page() == URL_PATH){
              send_message("Keine Suchergebnisse zur Auswahl gefunden!")
              change_page("suchen")
            }
          }

        }
      )

      observeEvent(
        input$weiter, {
          if (get_page() == URL_PATH){
            input <- param_get(session$clientData$url_hash, "input")$input
            change_page(sprintf("suchergebnisse?input=%s", input))
          }
        }
      )

      observeEvent(
        input$select_suche, {
          if (get_page() == URL_PATH){
            id <- input$select_suche
            if (id != ""){
              auswahl <- unlist(current$auswahl)

              if (id != "trigger"){
                if (id %in% auswahl){

                  if (is_auswahl_ok(current$daten, auswahl[!(auswahl %in% id)])){
                    removeCssClass(paste0("auswahl_", id), "btn-warning")
                    current$auswahl <- auswahl[!(auswahl %in% id)]
                  } else {
                    toastr_success("Saved to database")
                  }

                } else {

                  if (is_auswahl_ok(current$daten, auswahl[!(auswahl %in% id)])){
                    addCssClass(paste0("auswahl_", id), "btn-warning")
                    current$auswahl <- c(auswahl, id)
                  } else {
                    toastr_success("Saved to database")
                  }

                }
              }

              runjs("Shiny.onInputChange('suchen_auswahl-select_suche', '');")
            }
          }
        }
      ) # ENDE observeEvent: input$select_suche


    } # ENDE SERVER-FUNKTION
  )
}

get_daten <- function(ergebnis_id){

  ergebnis_list <- lapply(unlist(strsplit(ergebnis_id, "_")), \(.) .)
  ergebnis_list <- lapply(ergebnis_list, \(.) unlist(strsplit(., "-")))

  sprintf(
    fmt = get_sql("suchen_auswahl"),
    ergebnis_list |>
      lapply(\(.) sprintf("(tabelle_id = %s AND reihe_id = %s)", .[1], .[2])) |>
      unlist() |>
      paste(collapse = " AND "),
    paste(TABELLE$id, collapse = ", "),
    TABELLE$id[TABELLE$bez == "reichweite"],
    TABELLE$id[TABELLE$bez == "wert_einheit"],
    TABELLE$id[TABELLE$bez == "reichweite_typ"],
    TABELLE$id[TABELLE$bez == "variable"]
  ) |>
    get_query()
}

draw_auswahl <- function(daten, auswahl, ns){
  div(
    actionButton(ns("weiter"), "Mit Auswahl weiter zu Ergebnisse"),
    h4("Variablen:"),
    p("Hinweis: Es muss mindestens eine ausgewählt sein!"),
    get_buttons(daten[daten$tabelle_id %in% TABELLE$id[TABELLE$bez == "variable"],], auswahl, ns),
    h4("Region:"),
    p("Hinweis: Es muss mindestens eine ausgewählt sein!"),
    get_buttons(daten[grepl("a", daten$gruppe_id) & daten$tabelle_id %in% TABELLE$id[TABELLE$bez == "reichweite_typ"],], auswahl, ns),
    uiOutput(ns("auswahl_region")),
    h4("Gruppierungsmerkmale:"),
    p("Hinweis: Es muss mindestens eine ausgewählt sein!"),
    get_buttons(daten[grepl("b", daten$gruppe_id) & daten$tabelle_id %in% TABELLE$id[TABELLE$bez == "reichweite_typ"],], auswahl, ns),
    uiOutput(ns("auswahl_reichweiten")),
    h4("Einheit:"),
    p("Hinweis: Es muss mindestens eine ausgewählt sein!"),
    get_buttons(daten[daten$tabelle_id %in% TABELLE$id[TABELLE$bez == "wert_einheit"],], auswahl, ns)
  )
}

get_button <- function(input, auswahl = c(), ns){

  if (input[["id"]] %in% auswahl){
    class <- c("auswahl_button", "btn-warning")
  } else {
    class <- "auswahl_button"
  }

  actionButton(
    inputId = ns(paste0("auswahl_", input[["id"]])),
    class   = class,
    label   = input[["beschr"]],
    onclick = sprintf("selectSearch('%s')", input[["id"]])
  )
}

get_buttons <- function(daten, auswahl = c(), ns){
  daten |>
    apply(1, get_button, ns = ns, auswahl = auswahl) |>
    tagList()
}

get_reichweite <- function(daten, auswahl = c(), ns){

  daten_auswahl <- daten[!is.na(daten$is_region) & !daten$is_region & daten$id %in% auswahl,]
  if (nrow(daten_auswahl) < 1) return(HTML(""))

  reichweiten <- daten[daten$reichweite_typ_id %in% gsub("b", "", daten_auswahl$gruppe_id),]
  if (nrow(reichweiten) < 1) return(HTML(""))

  tagList(
    lapply(
      unique(reichweiten$reichweite_typ_id),
      \(.){
        tagList(
          h4(daten[daten$gruppe_id == paste0("b", .), "beschr"]),
          get_buttons(reichweiten[reichweiten$reichweite_typ_id %in% .,], auswahl, ns)
        )
      }
    )
  )
}

show_auswahl <- function(auswahl){
  for (id in auswahl){
    addCssClass(paste0("auswahl_", id), "btn-warning")
  }
}

is_auswahl_ok <- function(daten, auswahl){
  daten$reichweite_typ_id <- ifelse(is.na(daten$reichweite_typ_id), 0, daten$reichweite_typ_id)
  all(TABELLE$id %in% aggregate(prozent ~ tabelle_id + reichweite_typ_id, daten[daten$id %in% auswahl,], sum)$tabelle_id)
}

make_auswahl_ok <- function(daten, auswahl){

  daten$reichweite_typ_id <- ifelse(is.na(daten$reichweite_typ_id), 0, daten$reichweite_typ_id)
  daten$in_auswahl        <- daten$id %in% auswahl
  daten$is_region         <- ifelse(is.na(daten$is_region), FALSE, daten$is_region)

  # Keine Variable ausgewählt?
  if (!any(daten$in_auswahl & daten$tabelle_id == TABELLE$id[TABELLE$bez == "variable"])){
    daten$in_auswahl[daten$gruppe_id %in% sort(daten$gruppe_id[daten$tabelle_id == TABELLE$id[TABELLE$bez == "variable"]])[1]] <- TRUE
  }

  # Keine Wert-Einheit ausgewählt?
  if (!any(daten$in_auswahl & daten$tabelle_id == TABELLE$id[TABELLE$bez == "wert_einheit"])){
    daten$in_auswahl[daten$gruppe_id %in% sort(daten$gruppe_id[daten$tabelle_id == TABELLE$id[TABELLE$bez == "wert_einheit"]])[1]] <- TRUE
  }

  # Keine Region ausgewählt (reichweite_typ)?
  if (!any(daten$in_auswahl & daten$tabelle_id == TABELLE$id[TABELLE$bez == "reichweite_typ"] & daten$is_region)){
    daten$in_auswahl[daten$gruppe_id %in% sort(daten$gruppe_id[daten$tabelle_id == TABELLE$id[TABELLE$bez == "reichweite_typ"]])[1]] <- TRUE
  }

  # Keine konkrete oder falsche Region ausgewählt (reichweite)?
  nicht_ausgewaehlte_regionen <- daten$reichweite_typ_id %in% gsub("[ab]", "", daten$gruppe_id[daten$is_region & !daten$in_auswahl])
  daten$in_auswahl[nicht_ausgewaehlte_regionen] <- FALSE

  ausgewaehlte_regionen <- daten$reichweite_typ_id %in% gsub("[ab]", "", daten$gruppe_id[daten$is_region & daten$in_auswahl])
  if (!any(daten$in_auswahl[ausgewaehlte_regionen])){
    if ("Deutschland" %in% daten$beschr[ausgewaehlte_regionen]){
      daten$in_auswahl[daten$beschr %in% "Deutschland"] <- TRUE
    } else {
      daten$in_auswahl[daten$gruppe_id %in% sort(daten$gruppe_id[ausgewaehlte_regionen])[1]] <- TRUE
    }
  }

  # Kein notwendiger Reichweiten-Typ (außer Region) ausgewählt?
  reichweite_typ_mit_100 <-
    daten$tabelle_id %in% TABELLE$id[TABELLE$bez %in% "reichweite_typ"] &
    !daten$is_region &
    abs(daten$prozent - 100) < .0001
  daten$in_auswahl[reichweite_typ_mit_100 & !daten$in_auswahl] <- TRUE

  # Reichweite, die zu nicht ausgewählten Typen gehören
  reichweite_ohne_typ_in_auswahl <-
    daten$in_auswahl &
    daten$tabelle_id %in% TABELLE$id[TABELLE$bez %in% "reichweite"] &
    daten$in_auswahl[match(daten$reichweite_typ_id, gsub("[ab]", "", daten$gruppe_id))]
  daten$in_auswahl[reichweite_ohne_typ_in_auswahl] <- FALSE

  # Reichweiten-Gruppen, die zu Typen mit 100% in Daten gehört
  reichweite_typ_mit_100_ohne_reichweiten <-
    reichweite_typ_mit_100 &
    lapply(
      1:nrow(daten),
      \(.){
        if (!reichweite_typ_mit_100[.]) return(FALSE)
        sum(
          daten$reichweite_typ_id %in% gsub("[ab]", "", daten$gruppe_id[.]) &
            daten$in_auswahl
        ) == 0
      }
    ) |>
    unlist()

  neue_reichweiten_in_auswahl <-
    lapply(
      gsub("[ab]", "", daten$gruppe_id[reichweite_typ_mit_100_ohne_reichweiten]),
      \(.){
        sort(daten$id[daten$reichweite_typ_id %in% .])[1]
      }
    ) |>
    unlist()
  daten$in_auswahl[daten$id %in% neue_reichweiten_in_auswahl] <- TRUE

  return(daten$id[daten$in_auswahl])
}

