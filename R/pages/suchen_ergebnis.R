#' Necessary Packages/Functions

box::use(
  ../../R/utils/ui[draw_under_construction, draw_zurueck_button],
  ../../R/utils/database[get_query],
  ../../R/utils/string[preprocess_str],
  ../../R/utils/routing[add_param_in_url],
  ../../R/pkgs/svNum/numeric[roundFive],
  shiny[
    NS, moduleServer, observeEvent, observe,
    fluidPage, fluidRow, tagList,
    uiOutput, renderUI,
    div, HTML, h5, h3, p,
    actionButton,
    reactiveValues,
    icon
  ],
  shinyjs[
    removeCssClass,
    addCssClass
  ],
  shiny.router[get_query_param, get_page, change_page],
  shinyWidgets[switchInput, updateSwitchInput],
  reactable[reactable, colDef, reactableLang, reactableTheme],
  shinycssloaders[withSpinner],
  stats[aggregate],
  urltools[param_get, param_set]
)

VIEW_DATEN_LIMIT_TO_REDUCE <- 1000

#' Missing description
#' @export

module_suche_ergebnis_ui <- function(id = "suche_ergebnis", label = "m_suche_ergebnis") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      style = "display flex; flex-direction: column;",
      draw_zurueck_button(),
      div(
        uiOutput(ns("info"))
      ),
      div(
        style = "justify-content: space-around; display: flex; flex-direction: row; background-color: var(--light-grey); padding: 16px 20px 1px 20px; margin: 32px 0;",
        div(
          class = "menu-box",
          p("Nur aktuelle Daten:", style = "margin-bottom: 4px; width: 170px;"),
          switchInput(
            inputId  = ns("switch_aktuell"),
            onLabel  = "Ja",
            offLabel = "Nein",
            value    = TRUE
          )
        ),
        div(
          class = "menu-box",
          p("Daten filtern:", style = "margin-bottom: 4px; width: 170px;"),
          switchInput(
            inputId  = ns("switch_filtern"),
            onLabel  = "Ja",
            offLabel = "Nein",
            value    = TRUE
          )
        ),
        div(
          class = "menu-box",
          id    = ns("div_switch_alle"),
          p("Daten reduzieren:", style = "margin-bottom: 4px; width: 170px;"),
          switchInput(
            inputId  = ns("switch_alle"),
            onLabel  = "Ja",
            offLabel = "Nein",
            value    = FALSE
          )
        ),
        div(
          class = "menu-box",
          id    = ns("div_switch_hochschule"),
          p("Hochschule:", style = "margin-bottom: 4px; width: 170px;"),
          switchInput(
            inputId  = ns("switch_hochschule"),
            onLabel  = "Ja",
            offLabel = "Nein",
            value    = FALSE
          )
        ),
        div(
          class = "menu-box",
          p("Speichern & Teilen:", style = "margin-bottom: 4px; width: 170px;"),
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
          )
        )
      ),
      div(
        withSpinner(
          uiOutput(ns("ergebnisse")),
          type = 4,
          color = "#195365",
          id = ns("ergebnisse_spinner")
        )
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
            akt      = "",
            all      = "",
            hschl    = "",
            filter   = ""
          )
        )

      daten <-
        reactiveValues(
          view = data.frame()
        )

      observeEvent(
        daten$view, {
          if ("hochschule" %in% names(daten$view)){
            removeCssClass("div_switch_hochschule", "div_hide")
          } else {
            addCssClass("div_switch_hochschule", "div_hide")
          }

          if (nrow(daten$view) > VIEW_DATEN_LIMIT_TO_REDUCE){
            removeCssClass("div_switch_alle", "div_hide")
          } else {
            addCssClass("div_switch_alle", "div_hide")
          }
        }
      )

      observeEvent(
        input$switch_aktuell, {
          if (get_page() %in% "suchergebnisse"){
            current_url <- session$clientData$url_hash
            new_url     <-
              param_set(
                urls = current_url,
                key = "akt",
                value = as.numeric(input$switch_aktuell)
              )
            if (new_url != current_url){
              change_page(new_url)
            }
          }
        }
      )

      observeEvent(
        input$switch_alle, {
          if (get_page() %in% "suchergebnisse"){
            current_url <- session$clientData$url_hash
            new_url <-
              param_set(
                urls = current_url,
                key = "all",
                value = as.numeric(input$switch_alle)
              )
            if (new_url != current_url){
              change_page(new_url)
            }
          }
        }
      )

      observeEvent(
        input$switch_hochschule, {
          if (get_page() %in% "suchergebnisse"){
            current_url <- session$clientData$url_hash
            new_url     <-
              param_set(
                urls = current_url,
                key = "hschl",
                value = as.numeric(input$switch_hochschule)
              )
            if (new_url != current_url){
              change_page(new_url)
            }
          }
        }
      )

      observeEvent(
        input$switch_filtern, {
          if (get_page() %in% "suchergebnisse"){
            current_url <- session$clientData$url_hash
            new_url     <-
              param_set(
                urls = current_url,
                key = "filter",
                value = as.numeric(input$switch_filtern)
              )
            if (new_url != current_url){
              change_page(new_url)
            }
          }
        }
      )

      observeEvent(
        get_query_param(), {
          if (get_page() %in% "suchergebnisse"){

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
              recode_parameter_boolean()

            if (param_akt != current$parameter$akt){
              current$parameter$akt <- param_akt
              change                <- TRUE
              updateSwitchInput(session, "switch_aktuell", ifelse(param_akt, TRUE, FALSE))
            }

            param_all <-
              get_query_param("all") |>
              recode_parameter_boolean(default = 0)

            if (param_all != current$parameter$all){
              current$parameter$all <- param_all
              change                <- TRUE
              updateSwitchInput(session, "switch_alle", ifelse(param_all, TRUE, FALSE))
            }

            param_hschl <-
              get_query_param("hschl") |>
              recode_parameter_boolean(default = 0)

            if (param_hschl != current$parameter$hschl){
              current$parameter$hschl <- param_hschl
              change                  <- TRUE
              updateSwitchInput(session, "switch_hochschule", ifelse(param_hschl, TRUE, FALSE))
            }

            param_filter <-
              get_query_param("filter") |>
              recode_parameter_boolean(default = 0)

            if (param_filter != current$parameter$filter){
              current$parameter$filter <- param_filter
              change                   <- TRUE
              updateSwitchInput(session, "switch_filtern", ifelse(param_filter, TRUE, FALSE))
            }

            if (change){
              output$ergebnisse <-
                renderUI({
                  draw_reactable(
                    view_daten  = daten$view,
                    only_new    = ifelse(param_akt, TRUE, FALSE),
                    reduce_data = ifelse(param_all, TRUE, FALSE),
                    filterable  = ifelse(param_filter, TRUE, FALSE),
                    hochschulen = ifelse(param_hschl, TRUE, FALSE)
                  )
                })
            }
          }
        },
        ignoreNULL = FALSE
      )



    }
  )
}

draw_info <- function(ergebnis_id){
  div(
    style = "text-align: center; font-size: calc(var(--font-size) + 4px)",
    p("Daten zu:"),
    div(recode_ergebnisid_to_titel(ergebnis_id))
  )
}

get_data <- function(ergebnis_id){

  #daten_id.1

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

draw_reactable <- function(view_daten, only_new = TRUE, reduce_data = TRUE, filterable = FALSE, hochschulen = FALSE){

  if (nrow(view_daten) < 1){
    return(reactable(data.frame(x = "keine Daten")))
  }

  if (reduce_data & nrow(view_daten) > VIEW_DATEN_LIMIT_TO_REDUCE){
    min_tiefe  <- aggregate(tiefe ~ geschwister_id, view_daten, min)
    view_daten <- view_daten[min_tiefe$tiefe[match(view_daten$geschwister_id, min_tiefe$geschwister_id)] >= view_daten$tiefe,]
  }

  if (only_new) {
    view_daten <- view_daten[!duplicated(view_daten$gruppen_id),]
  }

  view_daten <- view_daten[order(view_daten$tiefe, view_daten$geschwister_id, view_daten$gruppen_id, 1:nrow(view_daten)),]
  if ("hochschule" %in% names(view_daten)){
    if (hochschulen){
      view_daten <- view_daten[!is.na(view_daten$hochschule),]
    } else {
      view_daten <- view_daten[is.na(view_daten$hochschule),]
    }
  }

  reichweite_typ <-
    get_query(
      "SELECT reichweite_typ.beschr, reichweite_klasse.beschr as klasse
       FROM reichweite_typ
       LEFT JOIN reichweite_klasse ON reichweite_typ.reichweite_klasse_id = reichweite_klasse.id"
    )

  reichweite_typ$beschr_preprocess <- gsub(" ", "__", preprocess_str(reichweite_typ$beschr))
  reichweite_typ$gebiet <- reichweite_typ$klasse %in% "Räumliche Gebiete" | reichweite_typ$beschr %in% "Hochschule"

  reichweite <- view_daten[,names(view_daten)[names(view_daten) %in% reichweite_typ$beschr_preprocess[!reichweite_typ$gebiet]]]
  if (is.data.frame(reichweite)){
    view_daten$reichweite <- apply(reichweite, 1, \(.) ifelse(all(is.na(.)), "", paste(.[!is.na(.)], collapse = ", ")))
  } else {
    view_daten$reichweite <- reichweite
  }

  gebiet <- view_daten[,names(view_daten)[names(view_daten) %in% reichweite_typ$beschr_preprocess[reichweite_typ$gebiet]]]
  if (is.data.frame(gebiet)){
    view_daten$gebiet <- apply(gebiet, 1, \(.) ifelse(all(is.na(.)), "", paste(.[!is.na(.)], collapse = ", ")))
  } else {
    view_daten$gebiet <- gebiet
  }

  view_daten$wert <- recode_wert(view_daten$wert)

  react_daten <- view_daten[,c("variable", "gebiet", "zeit", "reichweite", "wert", "wert_einheit")]
  name_gebiet <- ifelse(sum(reichweite_typ$gebiet) < 2, reichweite_typ$beschr[reichweite_typ$gebiet], "Region")
  if ("hochschule" %in% names(view_daten) & hochschulen){
    name_gebiet <- "Hochschule"
  }

  column_width <-
    list(
      variable     = max(nchar(view_daten$variable)),
      gebiet       = max(nchar(view_daten$gebiet)),
      zeit         = max(nchar(view_daten$zeit)),
      reichweite   = max(nchar(view_daten$reichweite)),
      wert         = max(nchar(view_daten$wert)),
      wert_einheit = max(nchar(view_daten$variable))
    )
  column_width <- lapply(column_width, \(.) min(c(30, max(c(10, .)))))
  column_width <- lapply(column_width, \(.) round(. / sum(unlist(column_width)) * 1200))



  reactable(
    react_daten,
    highlight  = TRUE,
    filterable = filterable,
    borderless = TRUE,
    fullWidth = TRUE,
    defaultPageSize = 25,
    theme =
      reactableTheme(
        headerStyle =
          list(
            "&" = list(
              "background-color" = "var(--blue)",
              "border" = "none",
              "color" = "white",
              "font-family" = "var(--font-family-bold)",
              "font-weight" = "400"
            ),
            "&:hover" =
              list(
                "background-color" = "var(--primary)"
              )
          ),
        paginationStyle =
          list(
            "&" =
              list(
                "font-size" = "var(--font-size-small);",
                "border-top" = "1.5px solid var(--blue);",
                "border-bottom" = "1.5px solid var(--blue);",
                "padding" = "6px;",
                "margin-top" = "8px;"
              )
          )

      ),
    columns =
      list(
        variable = colDef(
          name     = "Indikator",
          width    = column_width$variable
        ),
        gebiet = colDef(
          name  = name_gebiet,
          width = column_width$gebiet
        ),
        zeit = colDef(
          name  = "Zeit",
          width = column_width$zeit
        ),
        reichweite = colDef(
          name     = "Gruppen",
          width    = column_width$reichweite
        ),
        wert = colDef(
          name  = "Wert",
          width = column_width$wert,
          align = "right"
        ),
        wert_einheit = colDef(
          name  = "Einheit",
          width = column_width$wert_einheit
        )
      ),
    language =
      reactableLang(
        pageInfo          = "{rowStart} bis {rowEnd} von {rows} Einträgen",
        pagePreviousLabel = "Vorherige Seite",
        pageNextLabel     = "Nächste Seite",
        pageNext          = "weiter",
        pagePrevious      = "zurück"
    ),
    rowClass = "small-font",
    details = function(index){
      datum <- view_daten[index,]
      div(
        draw_under_construction()
        # class = "reactable-details",
        # h3("Details"),
        # p("Under Construction"),
        # p("WERT"),
        # p("WERTEINHEIT"),
        # p("Erklärungen"),
        # p("Gilt für"),
        # p(paste("Quelle:", datum$quelle[1]))

      )
    }
  )

}
#
# get_query("SELECT vdl.*, e.beschr FROM view_daten_link vdl LEFT JOIN erklaerung_link el ON vdl.tabelle_id = el.tabelle_id AND vdl.reihe_id = el.reihe_id LEFT JOIN erklaerung e ON el.erklaerung_id = e.id")

write_details <- function(index) {
 div(
   h3(react_daten$tag[index])
 )
}

recode_parameter_boolean <- function(x, vec = FALSE, default = 1){ #TODO in utils packen
  if (is.null(x)) return(default)
  if (length(x) != 1) return(default)
  x <- suppressWarnings(as.integer(x))
  if (any(is.na(x))) return(default)
  if (!(x %in% 0:1)) return(default)
  return(x)
}

recode_ergebnisid_to_titel <- function(ergebnis_id){
  lapply(unlist(strsplit(ergebnis_id, "_")), \(.) .) |>
    lapply(\(.) unlist(strsplit(., "-"))) |>
    lapply(
      \(.){
        . <- unlist(.)
        get_query(
          sprintf(
            "SELECT beschr FROM %s WHERE id = %s",
            get_query(
              sprintf(
                "SELECT bez FROM tabelle WHERE id = %s",
                .[1]
              )
            )[1,1],
            .[2]
          )
        )[1,1]
      }
    ) |>
    {
      \(.){
        . <- unlist(.)
        . <- paste0("<h3 style = 'display: inline;'>", ., "</h3>")
        if (length(.) == 1){
          return(.)
        } else if (length(.) == 2){
          return(paste(., collapse = " und "))
        } else {
          return(paste(c(paste(.[1:(length(.) - 1)], collapse = ", "), .[length(.)]), collapse = " und "))
        }
      }
    }() |>
    HTML()
}

recode_wert <- function(wert){
  ifelse(
    is.na(suppressWarnings(as.numeric(wert))),
    wert,
    format_number(wert)
  )
}

format_number <- function(wert){
  # INEFFIZIENTER WEG
  as.numeric(wert) |>
    lapply(
      \(.){
        if (. > 1000){
          format(roundFive(., 1), big.mark = ".", decimal.mark = ",", nsmall = 0)
        } else if (. > 10) {
          format(roundFive(., 2), big.mark = ".", decimal.mark = ",", nsmall = 0)
        } else {
          format(roundFive(., 3), big.mark = ".", decimal.mark = ",", nsmall = 0)
        }
      }
    ) |>
    unlist() |>
    trimws()
}

