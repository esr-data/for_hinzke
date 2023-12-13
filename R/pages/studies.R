#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, tagList,
    markdown,
    h2, a, p, div, img
  ],
  bsplus[bs_embed_tooltip]
)

#' Missing description
#' @export

module_studies_ui <- function(id = "studies", label = "m_studies", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      div(
        class = "panel-content",

        # Variabler Titel
        h2(
          style = "text-align: center;",
          ifelse(
            type == "all",
            "Alle Studien",
            ifelse(
              type == "handlung1",
              "Studien zu Bildung & Kompetenz",
              ifelse(
                type == "handlung2",
                "Studien zu Forschung & Innovation",
                "NA"
              )
            )
          )
        ),

        # Informationen

        div(
          style = "background-color: #EAEDEF; padding: 20px; margin: 20px;",
          div(
            style = "color: #195365; margin: 0;",
            markdown(readLines("md/information_studien.md"))
          )
        ),

        # Box

        div(
          style = "padding: 20px; margin: 20px; display: flex; flex-wrap: wrap;",
          create_all_boxes_studies(type)
        )
      )
    )
  )
}

#' Missing description
#' @export

module_studies_server <- function(id = "studies", con, type = "all") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns


    }
  )
}

#' Missing description
#' @noRd

create_box_studies <-
  function(title, infos, img_front, img_back, tooltip = NULL, path, typ){
    element <-
      a(
        href =
          sprintf(
            "https://stifterverband.shinyapps.io/%s/", path
          ),
        div(
          class = sprintf("studies_box %s", tolower(typ)),
          div(
            class = "studies_header",
            title
          ),
          img(
            class = "studies_img",
            src = sprintf("img/%s", img_front),
            onmouseover = sprintf("this.src = 'img/%s'", img_back),
            onmouseout  = sprintf("this.src = 'img/%s'", img_front)
          ),
          p(
            class = "studies_text",
            infos
          )
        )
      )


    if (!is.null(tooltip)){
      return(bs_embed_tooltip(element, tooltip, placement = "bottom"))
    }

    return(element)
  }

#' Missing description
#' @noRd

create_all_boxes_studies <- function(type){

  box_daten <-
    data.frame(
      rbind(
        c(
          "Innovation",
          "FuE-Erhebung",
          "Zahlen rund um die Erhebung zu Forschung und Entwicklung in der Wirtschaft.",
          "studie_fue.jpg",
          "studie_fue_alt.svg",
          "Die Erhebung zur Forschung und Entwicklung (FuE) im Wirtschaftssektor wird jährlich vom Stifterverband im Auftrag des BMBF durchgeführt. Unter anderem werden hierbei die Aufwendungen und das Personal erfasst, welches Unternehmen in Deutschland in die Forschung und Entwicklung investieren. Details zu den aktuellen Ergebnisse finden sich auf dem Dateportal der FuE-Erhebung.",
          "FuE_Daten"
        ),
        c(
          "Innovation",
          "Gründungsradar",
          "Der Gründungsradar untersucht die Gründungsförderung an deutschen Hochschulen.",
          "TODO",
          "TODO",
          "Der Gründungsradar untersucht zum sechsten Mal die Güte der Gründungsförderung an deutschen Hochschulen und stellt diese vergleichend dar. Er unterstreicht dabei die Bedeutung der Hochschulen für das Innovationsgeschehen und zeigt deren Anstrengungen für eine wirkungsvolle Gründungskultur. Die vorliegende Ausgabe nutzt die gleiche Methodik und das gleiche Indikatorenset mit 46 Indikatoren wie die vorhergehende Befragung 2020. Somit sind Zeitvergleiche zwischen den Erhebungen möglich. An der Befragung nahmen 196 Hochschulen teil , für die das Thema Gründungsförderung eine Rolle spielt (184 im Jahr 2020). Der Gründungsradar bietet damit eine valide Datengrundlage für das Gründungsgeschehen und die Gründungsförderung an deutschen Hochschulen.",
          "Gruendungsradar"
        ),
        c(
          "Innovation",
          "Drittmittel an Hochschulen",
          "TODO",
          "TODO",
          "TODO",
          "TODO",
          "DrittmittelAnHochschulen/"
        ),
        c(
          "Sonstige",
          "Engagement-Barometer",
          "TODO",
          "TODO",
          "TODO",
          "TODO",
          "Engagement-Barometer"
        ),
        c(
          "Bildung",
          "Hochschul-Barometer",
          "TODO",
          "TODO",
          "TODO",
          "TODO",
          "Hochschul-Barometer"
        ),
        c(
          "Bildung",
          "Hochschul-Bildungs-Report",
          "TODO",
          "TODO",
          "TODO",
          "TODO",
          "Hochschul-Bildungs-Report"
        ),
        c(
          "Bildung",
          "Länderchecks",
          "TODO",
          "TODO",
          "TODO",
          "TODO",
          "Laenderchecks"
        ),
        c(
          "Bildung",
          "Primus Preis",
          "Fakten und Analysen zu den Preisträgerinitiativen",
          "studie_primus.jpg",
          "studie_primus_alt.svg",
          "Der Primus-Preis zeichnet zivilgesellschaftliche Initiativen mit Vorbildcharakter aus. Die Datenportalseite umfasst sowohl Daten und Informationen zum Preis, z.B. zur Fördersumme und zur Anzahl der Projekte, als auch die Evaluation.", # , welche von Dezember 2019 bis Januar 2020 als eine Online-Befragung der gewählten Primus-Preisträgerinitiativen durchgeführt wurde
          "PrimusPreis"
        ),
        c(
          "Bildung",
          "Private Hochschulen",
          "TODO",
          "TODO",
          "TODO",
          "TODO",
          "PrivateHochschulen"
        ),
        c(
          "Bildung",
          "Stiftungsprofessuren",
          "In diesem Datenportal stellt der Stifterverband Daten zu den über ihn eingerichteten Stiftungsprofessuren zur Verfügung (Stand 11.10.2023).",
          "TODO",
          "TODO",
          "Der Stifterverband fördert seit Mitte der 1980er-Jahre Stiftungsprofessuren an deutschen Hochschulen. Die Anzahl und die Bedeutung von Stiftungsprofessuren haben über die Jahre zugenommen, denn Wirtschaft und Wissenschaft profitieren gleichermaßen von ihnen.",
          "Stiftungsprofessuren"
        )
      )
    )
  names(box_daten) <- c("typ", "titel", "text", "bild_front", "bild_hinten", "tooltip", "path")

  if (type == "handlung1"){
    box_daten <- box_daten[box_daten$typ == "Bildung",]
  } else if (type == "handlung2"){
    box_daten <- box_daten[box_daten$typ == "Innovation",]
  }

  tagList(
    apply(box_daten, 1, \(x) create_box_studies(x["titel"], x["text"], x["bild_front"], x["bild_hinten"], x["tooltip"], x["path"], x["typ"]))
  )
}

