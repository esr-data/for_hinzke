#' Necessary Packages/Functions

box::use(
  ../../R/utils/database[get_query, search_database, get_cache_labels, get_search_data],
  ../../R/utils/ui[draw_search],
  ../../R/utils/string[preprocess_str],
  ../../R/pkgs/svNum/numeric[formatNumeric],
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, fluidRow, tagList,
    uiOutput,
    h2, h3, div, icon, p, a,
    renderUI,
    reactiveValues,
    HTML
  ],
  shinyWidgets[updateSearchInput, searchInput, switchInput],
  shinycssloaders[withSpinner],
  reactable[reactable, reactableLang, colDef],
  shiny.router[get_page, get_query_param, change_page],
  utils[URLencode, URLdecode],
  stats[aggregate],
  htmltools[tags],
  rlist[list.rbind],
  tibble[tibble],
  urltools[param_get, param_set]
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
          style = "width: 100%; display: flex; flex-direction: column",
          h2(
            style = "text-align: center",
            "Suche"
          ),
          div(
            style = "max-width: 100%",
            div(
              style = "max-width: 700px; padding: 10px; display: flex; margin: 0 auto;",
              draw_search(
                inputId     = ns("suchen"),
                placeholder = "Begriffe in der Datenbank suchen ...",
                btnSearch   = icon("search"),
                btnReset    = icon("remove"),
                width       = "100%",
                label       = NULL
              )
            ),
          ),
          div(
            style = "width: fit-content; margin: 0 auto;",
            HTML("<p><b>Tipp!</b> Benutze das <b>&</b>-Zeichen zur Trennung verschiedener Suchbegriffe, die kombiniert werden sollen!</p>")
          )
        )
      ),
      div(
        style = "padding: 10px; margin-top: 20px; margin-bottom: 20px;",
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
          if (get_page() %in% "suchen"){
            current_url <- session$clientData$url_hash
            new_url     <-
              param_set(
                urls = current_url,
                key = "term",
                value = URLencode(input$suchen, reserved = TRUE)
              )
            if (new_url != current_url){
              change_page(new_url)
            }
          }
        }
      )

      observeEvent(
        current$suche, {

          output$ergebnisse <-
            renderUI({

              if (current$suche != ""){
                ergebnisse <- search_and_prepare(current$suche)
                div(
                  div(
                    style = "margin-bottom: 28px;",
                    HTML(sprintf("<h3 style = 'display: flex'><div style = 'padding: 8px'>Suchergebnisse für:</div> <div class = 'suchergebnisse-fuer'><sub><i class='fa-solid fa-quote-right'></i></sub>%s<sup><i class='fa-solid fa-quote-left'></i></sup></div></h3>", current$suche))
                  ),
                  div(
                    class = "suchergebnisse",
                    draw_reactable(ergebnisse)
                  )
                )

              } else {
                HTML("")
              }

            })




        }
      )

    }
  )
}

search_and_prepare <- function(suchwort){

  suche <-
    search_database(suchwort) |>
    process_search() |>
    prepare_simple_result()

  variablen <- c("treffer_id", "treffer_div", "link", "ranking", "einfach_treffer")

  if (nrow(rlist::list.rbind(suche)) > 0){

    if (length(suche) > 1){
      mehrfach_suche <- extract_mehrfach_treffer(suche)
      suche <-
        suche |>
        lapply(\(.) .[,variablen]) |>
        rlist::list.rbind()
      if (nrow(mehrfach_suche) > 0){
        suche <- rbind(suche, mehrfach_suche[,variablen])
      }

    } else {
      suche <- suche[[1]]
      suche <- suche[,variablen]
    }

    suche <- suche[order(suche$ranking, decreasing = TRUE),]

    suche$treffer_div <-
      paste0(
        "<div class = 'suche-tabelle-treffer-container'>",
        suche$treffer_div,
        "</div>"
      )
    suche$link <- paste0("<div>", suche$link, "</div>")

  } else {
    suche     <- data.frame(suche)
    suche[1,] <- NA
    for (i in variablen){
      suche[,i] <- NA
    }
    suche <- suche[0,]
    suche <- tibble(suche)
  }

  # RETURN:
  return(suche)
}

draw_reactable <- function(suche){

  if (nrow(suche) < 1){
    return(
      reactable(
        data.frame(x = 0, y = 0)[0,],
        columns =
          list(
            x = colDef(name = ""),
            y = colDef(name = "")
          ),
        borderless = TRUE,
        highlight  = TRUE,
        language   =
          reactableLang(
            noData = "Keine Einträge in der Datenbank gefunden."
          )
      )
    )
  }

  suche$treffer_div <- paste("<div style = 'display: flex; margin: 12px 0;'>", suche$treffer_div, "<div style = 'width: 12px'></div>", suche$link, "</div>")
  suche$nr <- paste0("<div class = 'suche-tabelle-range'>", 1:nrow(suche), "</div>")
  reactable(
    suche[,c("nr", "treffer_div")],
    columns =
      list(
        nr = colDef(
          maxWidth = 100,
          html = TRUE
        ),
        treffer_div = colDef(
          name = "Treffer",
          html = TRUE,
          minWidth = 500
        )
      ),
    borderless = TRUE,
    highlight = TRUE,
    language =
      reactableLang(
        pageNext = "Vor",
        pagePrevious = "Zurück",
        pageInfo = "{rowStart}\u2013{rowEnd} von {rows} Treffern",
        noData = "Keine Einträge in der Datenbank gefunden."
      )
  )
}

process_search <- function(suche){

  if (is.data.frame(suche)) suche <- list(suche)
  if (all(unlist(lapply(suche, nrow)) < 1)) return(suche)

  tabelle <-
    lapply(suche, \(.) .$tabelle) |>
    unlist() |>
    unique() |>
    sprintf(fmt = "'%s'") |>
    paste(collapse = ",") |>
    sprintf(fmt = "SELECT * FROM tabelle WHERE bez IN (%s)") |>
    get_query()

  search_data        <- get_search_data()
  search_data_beschr <- search_data[search_data$column == "beschr", 1:5]

  suche <-
    suche[unlist(lapply(suche, \(.) nrow(.) > 0))] |>
    lapply(
      \(.){
        .$beschr <-
          search_data_beschr$str[
            match(
              paste(.$tabelle, .$reihe_id),
              paste(search_data_beschr$table, search_data_beschr$id_in_table)
            )
          ]
        .$tabelle_id <- tabelle$id[match(.$tabelle, tabelle$bez)]
        return(.[,c("reihe_id", "tabelle_id", "tabelle", "beschr", "ranking")])
      }
    )

  return(suche)
}

prepare_simple_result <- function(suche){

  if (all(unlist(lapply(suche, nrow)) < 1)) return(suche)

  reichweite <-
    lapply(suche, \(.) .$reihe_id[.$tabelle %in% "reichweite"]) |>
    unlist() |>
    c(1) |>
    unique() |>
    paste(collapse = ",") |>
    sprintf(
      fmt =
      "SELECT r.id as reichweite_id, rt.beschr as reichweite_typ
       FROM reichweite r
       LEFT JOIN reichweite_typ rt ON r.reichweite_typ_id = rt.id
       WHERE r.id IN (%s)"
    ) |>
    get_query()

  suche <-
    lapply(
      suche,
      \(.){
        .$tabelle_label <- .$tabelle
        substr(.$tabelle_label, 1, 1) <-
          .$tabelle_label |>
          substr(1, 1) |>
          toupper()
        .$treffer_id <- paste(.$tabelle_id, .$reihe_id, sep = "-")
        .$reichweite_typ <- ifelse(.$tabelle %in% "reichweite", reichweite$reichweite_typ[match(.$reihe_id, reichweite$reichweite_id)], NA)
        .$treffer_div <-
          ifelse(
            !is.na(.$reichweite_typ),
            paste0("<div class = 'suche-tabelle-treffer treffer-", preprocess_str(.$tabelle), "'><div class = 'suche-tabelle-beschr'>", .$beschr, "</div><div class = 'suche-tabelle-typ treffer-", preprocess_str(.$tabelle), "'>", .$reichweite_typ, "</div>", "</div>"),
            paste0("<div class = 'suche-tabelle-treffer treffer-", preprocess_str(.$tabelle), "'><div class = 'suche-tabelle-beschr'>", .$beschr, "</div><div class = 'suche-tabelle-typ treffer-", preprocess_str(.$tabelle), "'>", .$tabelle_label,  "</div>", "</div>")
          )
        .$link <- recode_treffer_id_in_link(.$treffer_id)
        .$einfach_treffer <- TRUE
        return(.)
      }
    )
  return(suche)
}

extract_mehrfach_treffer <- function(suche){

  suche_mehrfach <-
    suche |>
    lapply(\(.) paste(.$tabelle_id, .$reihe_id, sep = "-")) |>
    expand.grid()

  suche_mehrfach_vorhanden <-
    suche |>
    lapply(\(.) paste(.$tabelle_id, .$reihe_id, sep = "-")) |>
    unlist() |>
    unique() |>
    sprintf(fmt = "'%s'") |>
    paste(collapse = ",") |>
    sprintf(fmt = "treffer_id IN (%s)") |>
    sprintf(
      fmt =
        "WITH treffer AS (
           SELECT dl.daten_id, dl.tabelle_id || '-' || dl.reihe_id as treffer_id
           FROM view_daten_link dl
           WHERE %s
           ORDER BY daten_id, treffer_id
         )
        SELECT *
        FROM treffer
        WHERE daten_id IN (SELECT daten_id FROM (SELECT daten_id, COUNT(*) as n FROM treffer GROUP BY daten_id) WHERE n > 1)"
    ) |>
    get_query() |>
    aggregate(treffer_id ~ daten_id, data = _, \(.) paste(sort(.), collapse = "_")) |>
    {\(.) .$treffer_id}() |>
    unique() |>
    lapply(\(.) strsplit(., split = "_")[[1]])

  suche_mehrfach <-
    suche_mehrfach[
      apply(
        suche_mehrfach,
        1,
        \(.){
          lapply(suche_mehrfach_vorhanden, \(x) all(. %in% x)) |>
            unlist() |>
            any()
        }
      ),
    ] |>
    tibble()

  treffer_id      <- apply(suche_mehrfach, 1, \(.) paste(sort(.), collapse = "_"))
  treffer_id_list <- apply(suche_mehrfach, 1, list)
  suche_mehrfach$treffer_id      <- treffer_id
  suche_mehrfach$treffer_id_list <- lapply(treffer_id_list, \(.) .[[1]])
  suche_mehrfach <- suche_mehrfach[,c("treffer_id", "treffer_id_list")]

  suche <- rlist::list.rbind(suche)

  suche_mehrfach$treffer_div <-
    lapply(
      suche_mehrfach$treffer_id_list,
      \(.){
        suche$treffer_div[suche$treffer_id %in% .] |>
          unique() |>
          paste(collapse = "<i class='fa-solid fa-link fa-rotate-by treffer-mehrfachtreffer-symbol'></i>")
      }
    ) |>
    unlist()

  suche_mehrfach$ranking <-
    suche_mehrfach$treffer_id_list |>
    lapply(\(.) {
      . <- suche[suche$treffer_id %in% .,]
      . <- aggregate(ranking ~ treffer_id, ., max)
      sum(.$ranking)
    }) |>
    unlist()

  suche_mehrfach$einfach_treffer <- FALSE

  suche_mehrfach$link <- recode_treffer_id_in_link(suche_mehrfach$treffer_id)
  return(suche_mehrfach)
}

recode_treffer_id_in_link <- function(treffer_id){
  sprintf(
    "<a class = 'treffer-link' href = '#!/suchergebnisse?input=%s'><i class='fa-solid fa-arrow-up-right-from-square'></i><div>Ergebnisse anzeigen</div></a>",
    treffer_id
  )
}
