#' Missing description
#' @noRd

module_explorer_ui <- function(id = "explorer", label = "m_explorer", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      div(
        class = "panel-content",
        h2("Explorer"),
        fluidRow(
          style = "padding: 10px; display: flex; margin: 0;",
          div(
            class = "content-box",
            pickerInput(
              inputId = ns("select_variable"),
              label = "Liste der Variablen",
              choices = c(""),
              options = pickerOptions(
                actionsBox = FALSE,
                liveSearch = TRUE,
                maxOptions = 1
              ),
              multiple = TRUE
            )
          ),
          div(
            class = "content-box",
            h3("test"),
            uiOutput(ns("select_tags"))
          ),
          div(class = "content-box", "3")
        ),
        reactableOutput(ns("table"))
      )
    )
  )
}

#' Missing description
#' @noRd

module_explorer_server <- function(id = "explorer", type = "all", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      updatePickerInput(
        session,
        "select_variable",
        choices = sort(dbGetQuery(con, "SELECT beschr FROM variable")$beschr)
      )

      output$select_tags <- renderUI({draw_tags()})

      # output$table <- renderReactable({
      #   x <- dbGetQuery(con, "SELECT * FROM tag")
      #   reactable(x)
      # })

      observeEvent(input$select_variable, {
        variable <-
          dbGetQuery(
            con,
            sprintf(
              "SELECT id FROM variable WHERE beschr = '%s'",
              isolate(input$select_variable)
            )
          )[1,1]
        output$table <- renderReactable({load_table_by_variable(variable)})
      })

    }
  )
}

load_table_by_variable <- function(variable){
  daten <-
    dbGetQuery(
      con,
      sprintf(
        "SELECT daten.id, variable.beschr as variable, jahr, wert, wert_einheit.beschr as einheit
       FROM daten
       LEFT JOIN wert_einheit ON daten.wert_einheit_id = wert_einheit.id
       LEFT JOIN variable     ON daten.variable_id = variable.id
       WHERE variable_id = %s",
        variable
      )
    )
  reichweite <-
    "SELECT reichweite.id as id, reichweite.beschr as reichweite, rtyp.beschr as reichweite_typ, rklasse.beschr as reichweite_klasse
   FROM reichweite
   LEFT JOIN reichweite_typ rtyp ON reichweite.reichweite_typ_id = rtyp.id
   LEFT JOIN reichweite_klasse rklasse ON rtyp.reichweite_klasse_id = rklasse.id
   WHERE reichweite.id IN (SELECT reichweite_id FROM daten_reichweite WHERE daten_id IN (SELECT id FROM daten WHERE variable_id = %s))" |>
    sprintf(variable) |>
    dbGetQuery(conn = con)
  daten_reichweite <-
    dbGetQuery(con, sprintf("SELECT * FROM daten_reichweite
                          WHERE daten_id IN (SELECT id FROM daten WHERE variable_id = %s)", variable))

  reichweite$gruppe <- ifelse(reichweite$reichweite_klasse == "Räumliche Gebiete", reichweite$reichweite_klasse, reichweite$reichweite_typ)

  for (i in unique(reichweite$gruppe)){
    daten[,i] <- NA
  }

  for (i in 1:nrow(daten)){
    x <- daten_reichweite[daten_reichweite$daten_id == daten$id[i],]
    for (j in 1:nrow(x)){
      k <- reichweite$id == x$reichweite_id[j]
      l <- c(daten[i, reichweite$gruppe[k]], reichweite$reichweite[k])
      l <- l[!is.na(l)]
      daten[i, reichweite$gruppe[k]] <- paste(l, collapse = ", ")
    }
  }

  for (i in unique(reichweite$gruppe)){
    daten[,i] <- ifelse(is.na(daten[,i]), "Insgesamt", daten[,i])
  }

  return(
    reactable(
      daten[,c("jahr", "wert", "einheit", unique(reichweite$gruppe))],
      showSortable = TRUE,
      searchable = TRUE,
      filterable = TRUE,
      highlight = TRUE,
      defaultColDef = colDef(minWidth = 200, align = "center"),
      columns = list(
        jahr     = colDef(name = "Jahr", minWidth = 75),
        wert     = colDef(
          name  = "Wert",
          width = 100,
          align = "right",
          cell  = function(value) ifelse(!is.na(as.numeric(value)), format(as.numeric(value), big.mark = ".", decimal.mark = ","), value)
        ),
        einheit  = colDef(name = "Einheit",  minWidth = 100, align = "left")
      ),
      columnGroups =
        list(
          colGroup(name = "Reichweite", columns = unique(reichweite$gruppe))
        )
    )
  )
}

draw_tags <- function(){

  tags <- dbGetQuery(con, "SELECT beschr, bez FROM tag WHERE id IN (SELECT tag_id FROM tag_link WHERE tabelle_id = (SELECT id FROM tabelle WHERE bez = 'variable'))")[,1]
  return(
    tagList(
      lapply(
        tags,
        \(x){
          bsButton("sbd_stories_1", label = "stories_1", class = "tag_select")
        }
      )
    )
  )
}
