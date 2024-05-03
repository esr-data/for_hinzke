#' Necessary Packages/Functions

box::use(
  ../../R/utils/team[create_team_member],
  ../../R/utils/earthworm[read_markdown],
  ../../R/utils/earthworm[read_markdown_cache],
  shiny[
    NS, moduleServer,
    fluidPage, fluidRow, column, uiOutput,
    tagList, h2, h4, div, br, img,
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
      h2(class = "moodie_title", "Forschungsdatenzentrum Wissenschaftsstatistik (FDZ)"),
      fluidRow(
        column(
          width = 8,
          uiOutput(ns("fdz_beschreibung")),
          create_team_member(
            "Lena Finger",
            "lena.finger@stifterverband.de",
            "https://profile-images.xing.com/images/7ca995fcfbab6e44c8ae7c4610ea59af-3/lena-finger.256x256.jpg",
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
