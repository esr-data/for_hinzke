#' Necessary Packages/Functions

box::use(
  ../../R/utils/ui[draw_under_construction],
  ../../R/utils/database[get_query],
  ../../R/utils/string[preprocess_str],
  ../../R/utils/routing[add_param_in_url],
  shiny[
    NS, moduleServer, observeEvent, observe,
    fluidPage, fluidRow, tagList,
    uiOutput, renderUI,
    div, HTML, h5, h3,
    actionButton,
    reactiveValues
  ],
  shiny.router[get_query_param, get_page, change_page],
  shinyWidgets[switchInput],
  reactable[reactable, colDef]
)

#' Missing description
#' @export

module_suche_ergebnis_ui <- function(id = "suche_ergebnis", label = "m_suche_ergebnis") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      style = "display flex; flex-direction: column;",
      div(
        uiOutput(ns("info"))
      ),
      div(
        style = "display flex; flex-direction: row;",
        switchInput(
          inputId  = ns("aktuell"),
          onLabel  = "Ja",
          offLabel = "Nein",
          value    = TRUE
        )

      ),
      div(
        uiOutput(ns("ergebnisse"))
      )
    )
  )
}

#' Missing description
#' @export

module_suche_ergebnis_server <- function(id = "suche_ergebnis") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      current <-
        reactiveValues(
          parameter = list(
            input    = "",
            akt      = 1
          )
        )

      daten <-
        reactiveValues(
          view = data.frame()
        )

      observeEvent(
        get_query_param(), {
          if (get_page() == "suchergebnisse"){

            change      <- FALSE
            ergebnis_id <- get_query_param("input")

            if (!grepl("^[0-9_-]+$", ergebnis_id)){
              ergebnis_id <- NA
            }

            if (is.null(ergebnis_id)){
              change_page("suchen")
            } else if (is.na(ergebnis_id) | nchar(ergebnis_id) == 0){
              change_page("suchen")
            }

            if (ergebnis_id != current$parameter$input){
              current$parameter$input <- ergebnis_id
              output$info             <- renderUI({draw_info(ergebnis_id)})
              daten$view              <- get_data(ergebnis_id)
              change                  <- TRUE
            }

            param_akt <-
              get_query_param("akt") |>
              indikator_recode_parameter_boolean()

            if (param_akt != current$parameter$akt){
              current$parameter$akt <- param_akt
              change                <- TRUE
            }

            if (change){
              output$ergebnisse <-
                renderUI({
                  draw_reactable(
                    daten$view,
                    only_new = ifelse(param_akt, TRUE, FALSE)
                  )
                })
            }
          }
        },
        ignoreNULL = FALSE
      )

      observeEvent(
        input$aktuell, {
          current_url <- session$clientData$url_hash
          new_url <-
            add_param_in_url(
              current_url  = current_url,
              current_page = "suchergebnisse",
              parameter    = "akt",
              value        = as.numeric(input$aktuell),
              old_value    = get_query_param("akt")
            )
          if (new_url != current_url){change_page(new_url)}
        }
      )

    }
  )
}

draw_info <- function(ergebnis_id){
  h3(paste(unlist(ergebnis_id), collapse = "_"))
}

get_data <- function(ergebnis_id){

  ergebnis_list <- lapply(unlist(strsplit(ergebnis_id, "_")), \(.) .)
  ergebnis_list <- lapply(ergebnis_list, \(.) unlist(strsplit(., "-")))

  view_daten <-
    ergebnis_list |>
    lapply(\(.) sprintf("vdd.daten_id IN (SELECT DISTINCT daten_id FROM view_daten_link WHERE (tabelle_id = %s AND reihe_id = %s))", .[1], .[2])) |>
    unlist() |>
    paste(collapse = " AND ") |>
    sprintf(
      fmt =
        "SELECT *
         FROM view_daten_detailed vdd
         LEFT JOIN view_daten_struktur vds ON vdd.daten_id = vds.daten_id
         WHERE %s AND vdd.wert != '.a)'
         ORDER BY vds.zeit_ende DESC"
    ) |>
    get_query()

  view_daten$sortierung <- NA
  for (i in unique(view_daten$variable)){
    . <- view_daten$daten_id[view_daten$variable == i]
    . <- match(view_daten$daten_id, .)
    view_daten$sortierung <- ifelse(!is.na(.), ., view_daten$sortierung)
    rm(.)
  }

  view_daten <- view_daten[,!apply(view_daten, 2, \(.) all(is.na(.)))]
  view_daten <- view_daten[order(view_daten$sortierung * -1, view_daten$zeit_ende, view_daten$tiefe, decreasing = TRUE),]

  return(view_daten)
}

draw_reactable <- function(view_daten, only_new = TRUE){

  if (nrow(view_daten) < 1){
    return(reactable(data.frame(x = "keine Daten")))
  }

  if (only_new) view_daten <- view_daten[!duplicated(view_daten$gruppen_id),]

  reichweite_typ <- get_query("SELECT beschr FROM reichweite_typ")
  reichweite_typ$beschr_preprocess <- gsub(" ", "__", preprocess_str(get_query("SELECT beschr FROM reichweite_typ")$beschr))
  reichweite_variable <- names(view_daten)[names(view_daten) %in% reichweite_typ$beschr_preprocess]

  view_daten$reichweite <- apply(view_daten[,reichweite_variable], 1, \(.) ifelse(all(is.na(.)), "", paste(.[!is.na(.)], collapse = ", ")))
  react_daten <- view_daten[,c("variable", "zeit", "reichweite", "wert", "wert_einheit")]
  reactable(
    react_daten,
    filterable = TRUE,
    columns =
      list(
        variable = colDef(
          name     = "Indikator",
          minWidth = 400,
          width    = 500
        ),
        zeit = colDef(
          name  = "Zeit",
          width = 100
        ),
        reichweite = colDef(
          name = "Gruppen",
          minWidth = 300,
          width    = 400
        ),
        wert = colDef(
          name  = "Wert",
          width = 100
        ),
        wert_einheit = colDef(
          name  = "Einheit",
          width = 100
        )
      )
  )

}

indikator_recode_parameter_boolean <- function(x, vec = FALSE){ #TODO in utils packen
  if (is.null(x)) return(1)
  if (length(x) != 1) return(1)
  x <- suppressWarnings(as.integer(x))
  if (any(is.na(x))) return(1)
  if (!(x %in% 0:1)) return(1)
  return(x)
}
