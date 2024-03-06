#' Necessary Packages/Functions

box::use(
  ../../R/utils/ui[draw_under_construction],
  ../../R/utils/database[get_query, get_sql],
  yaml[read_yaml],
  shiny[
    NS, moduleServer,
    fluidPage, h2,
    icon, div, p,
    tagList, actionButton
  ],
  shinyWidgets[checkboxGroupButtons]
)

#' Missing description
#' @export

module_datensaetze_ui <- function(id = "datensaetze", label = "m_datensaetze") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      h2("Datensätze"),
      draw_under_construction(),
      div(
        div(
          class = "library",
          style = "max-width: 300px;",
          tagList(
            apply(
              get_available_datensaetze(), 1,
              \(x) actionButton(x["id"], x["label"], class = "book", icon = icon("play-circle"))
            )
          )
        ),
        div(
          "INFOFELD"
        )
      ),
      div(
        "ERGEBNISSE"
      )
    )
  )
}

get_available_datensaetze <- function(){
  meta_daten <- read_yaml("yml/explorer/datensatz.yml")
  output <-
    data.frame(
      id           = names(meta_daten),
      label        = unlist(lapply(meta_daten, \(x) x$label)),
      beschreibung = unlist(lapply(meta_daten, \(x) x$beschreibung)),
      sql          = unlist(lapply(meta_daten, \(x) x$sql))
    )
  rownames(output) <- 1:nrow(output)
  return(output)
}

test <- function(){


 apex(
    data = data.frame(Anzahl = nrow(daten)),
    mapping = aes(Anzahl)
  )


  x <- get_available_datensaetze()
  daten <- get_sql(x$sql[2], TRUE)

  apexcharter

  daten$id <- NULL

  reactable::reactable(
    daten
  )


}
