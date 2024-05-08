#' Necessary Packages/Functions

box::use(
  ../../R/utils/team[create_team_member],
  ../../R/utils/earthworm[read_markdown],
  ../../R/utils/earthworm[read_markdown_cache],
  ../../R/utils/ui[draw_under_construction],
  shiny[
    NS, moduleServer,
    fluidPage, fluidRow, column, uiOutput,
    tagList, h2, h4, div, br, img,
    renderUI
  ]
)

#' Missing description
#' @export

module_team_ui <- function(id = "team", label = "m_team") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      h2(class = "moodie_title", "SV Data"),
      fluidRow(
        column(
          width = 8,
          uiOutput(ns("team_beschreibung")),
        ),
        column(
          width = 4,
          class = "column-content-center-align",
          img(
            src = "img/sv_data.svg",
            height = "230px",
            alt = "Logo Abteilung SV Data"
          )
        )
      ),
      fluidRow(
        width = 12,
        create_team_member(
          "Katharina Brunner",
          "katharina.brunner@stifterverband.de",
          "https://i.mint-vernetzt.de/ug7WRqmUk9DkbBYWsCmynfjw-lkGSJejtDPJXg6XkE8/rs:fill:144:144:1/g:ce/dpr:2/bl:0/aHR0cHM6Ly94ZW1xZXFiZWV2aGxpdnpibWZiby5zdXBhYmFzZS5jby9zdG9yYWdlL3YxL29iamVjdC9wdWJsaWMvaW1hZ2VzL2NiLzcwZDZiMmZmOTA1NTI5OWU5ZDI1ZDg5NzA4YjE1Yi9hdmF0YXIuanBn",
          "Expertin Data Wrangling",
          "https://www.stifterverband.org/ueber_uns/mitarbeiter/brunner_katharina"
        ),
        create_team_member(
          "Marian Burk",
          "marian.burk@stifterverband.de",
          "https://www.stifterverband.org/sites/default/files/styles/max_1300x1300/public/burk_marian.jpg?itok=d2CGjmxI",
          "Experten-Experte",
          "https://www.stifterverband.org/ueber_uns/mitarbeiter/burk_marian"
        ),
        create_team_member(
          "Svenja Elsner",
          "svenja.elsner@stifterverband.de",
          "https://www.stifterverband.org/sites/default/files/styles/max_1300x1300/public/elsner_svenja.jpg?itok=tJRKKp2y",
          "Visualisierungs-Expertin",
          "https://www.stifterverband.org/ueber_uns/mitarbeiter/elsner_svenja"
        ),
        create_team_member(
          "Jessica Ernst",
          "jessica.ernst@stifterverband.de",
          "https://www.stifterverband.org/sites/default/files/styles/max_1300x1300/public/ernst_jessica.jpg?itok=1mmq3ZwI",
          "SV Data Projektmanagement",
          "https://www.stifterverband.org/ueber_uns/mitarbeiter/ernst_jessica"
        ),
        create_team_member(
          "Dr. Barbara Grave",
          "barbara.grave@stifterverband.de",
          "https://www.stifterverband.org/sites/default/files/styles/max_1300x1300/public/grave_1000x562.jpg?itok=iw4yoofe",
          "IT-Expertin",
          "https://www.stifterverband.org/ueber_uns/mitarbeiter/grave_barbara"
        ),
        create_team_member(
          "Dr. Malte Hückstädt",
          "malte.hueckstaet@stifterverband.de",
          "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Unknown_person.jpg/925px-Unknown_person.jpg",
          "Visualisierungs-Experte",
          "https://www.stifterverband.org/ueber_uns/mitarbeiter/hueckstaedt_malte"
        ),
        create_team_member(
          "Kim Elena Micke",
          "kimelena.micke@stifterverband.de",
          "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Unknown_person.jpg/925px-Unknown_person.jpg",
          "Datenbank-Expertin",
          "https://www.stifterverband.org/ueber_uns/mitarbeiter/micke_kim_elena"
        ),
        create_team_member(
          "Dr. Johannes Schmitt",
          "johannes.schmitt@stifterverband.de",
          "https://www.stifterverband.org/sites/default/files/styles/max_1300x1300/public/schmitt_johannes.jpg?itok=Hjqct1-Z",
          "Co-Leitung SV Data",
          "https://www.stifterverband.org/ueber_uns/mitarbeiter/schmitt_johannes"
        ),
        create_team_member(
          "Eike Schröder",
          "eike.schroeder@stifterverband.de",
          "https://www.stifterverband.org/sites/default/files/styles/max_1300x1300/public/schroeder_eike.jpg?itok=EdvddyOg",
          "Co-Leitung SV Data",
          "https://www.stifterverband.org/ueber_uns/mitarbeiter/schroeder_eike"
        ),
        create_team_member(
          "Lukas ?",
          "lukas.?@stifterverband.de",
          "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Unknown_person.jpg/925px-Unknown_person.jpg",
          "Wird sich zeigen",
          "https://www.stifterverband.org/ueber_uns/mitarbeiter/schroeder_eike"
        )
      )
    )
  )
}

#' Missing description
#' @export

module_team_server <- function(id = "team") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$team_beschreibung <- renderUI({read_markdown("svData")})

    }
  )
}
