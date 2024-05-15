#' Necessary Packages/Functions

box::use(
  ../../R/utils/team[create_team_member],
  ../../R/utils/earthworm[read_markdown],
  ../../R/utils/earthworm[read_markdown_cache],
  shiny[
    NS, moduleServer,
    fluidPage, fluidRow, column, uiOutput,
    tagList, h1, h4, div, br, img, HTML,
    downloadButton, renderUI, downloadHandler
  ]
)

#' Missing description
#' @export

module_fdz_ui <- function(id = "fdz", label = "m_fdz") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      div(
        class = "subpage-title",
        h1(class = "subpage-title-headline", HTML("Forschungsdatenzentrum<br>Wissenschaftsstatistik (FDZ)")),
        div(class = "header-title-clipgraph")
      ),
      br(),
      fluidRow(
        column(
          width = 8,
          uiOutput(ns("fdz_beschreibung")),
          create_team_member(
            "Lena Finger",
            "lena.finger@stifterverband.de",
            "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Unknown_person.jpg/925px-Unknown_person.jpg",
            "Leitung FDZ",
            "https://www.stifterverband.org/ueber_uns/mitarbeiter/finger_Lena"
          )
        ),
        column(
          width = 4,
          class = "column-content-center-align",
          h4("Flyer FDZ"),
          br(),
          img(
            src = "img/FlyerFDZ.jpg",
            height = "230px",
            alt = "Cover-FDZ-Flyer"
          ),
          br(),
          br(),
          downloadButton(ns("fdz_flyer"))
        )
      )
    )
  )
}

#' Missing description
#' @export

module_fdz_server <- function(id = "fdz") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$fdz_beschreibung <- renderUI({read_markdown("forschungsdatenzentrum-wissenschaftsstatistik")})

      output$fdz_flyer <- downloadHandler(
        "Flyer_FDZ_Stifterverband.pdf",
        content = function(file) {
          file.copy("downloads/FlyerFDZ.pdf", file)
        })

    }
  )
}
