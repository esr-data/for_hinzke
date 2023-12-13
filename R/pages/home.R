#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer, observeEvent, observe,
    reactiveValues, reactiveValuesToList,
    fluidPage, tagList, h2, div, markdown,
    uiOutput, renderUI, HTML,
    actionButton
  ],
  DBI[dbGetQuery],
  visNetwork[
    visNetwork, visNetworkProxy,
    renderVisNetwork, visNetworkOutput,
    visEvents, visFocus, visInteraction,
    visEdges, visNodes, visLayout,
  ]
)

#' Missing description
#' @export

module_home_ui <- function(id = "home", label = "m_home") {
  ns <- NS(id)
  tagList(
    fluidPage(
      div(
        class = "panel-content",
        markdown(readLines("md/willkommen.md")),
        div(
          style = "display: flex; flex-direction: row;",
          div(
            style = "width: 100px;",
            actionButton(ns("datensatz"), label = "Datensätze")
          ),
          div(
            class = "network-panel",
            style = "height: 500px; width: 800px; background-color: var(--very-light-grey); margin: 20px; padding: 2px; box-shadow: 0px 0px 5px 0px var(--grey);",
            visNetworkOutput(ns("network"), width = "100%", height = "100%")
          ),
          div(
            style = "min-width: 200px;",
            uiOutput(ns("network_selected"))
          )
        )
      )
    )
  )
}

#' Missing description
#' @export

module_home_server <- function(id = "home", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      network <- reactiveValues(data = get_network_data(con))

      observeEvent(
        network, {
          if (!is.null(network)){
            output$network <-
              renderVisNetwork({
                draw_network(reactiveValuesToList(network)$data, ns("current_node_id"), 1)
              })
          }


        }
      )

      observeEvent(
        input$datensatz, {
          print(2)
          visNetworkProxy(ns("network"))|>
            visFocus(id = 1, scale = .5)
        }
      )

      observeEvent(
        input$current_node_id, {
          if (!is.null(input$current_node_id$nodes)){
            if(length(input$current_node_id$nodes) > 0){
              print(input$current_node_id$nodes[[1]])
              output$network_selected <- renderUI({HTML(input$current_node_id$nodes[[1]])})
            }
          }
        }
      )

    }
  )
}

#' Missing description
#' @export
get_network_data <- function(con){

  tag <- dbGetQuery(con, "SELECT * FROM tag")

  #TODO Daten ergänzen; später auslagern!!

  tag <-
    rbind(
      tag,
      data.frame(
        id          = max(tag$id) + 1,
        eltern_id   = NA,
        bez         = "wwie",
        bez_lang    = "wissen",
        beschr      = "Wissen",
        beschr_lang = NA,
        descr       = NA
      )
    )

  tag <-
    rbind(
      tag,
      data.frame(
        id          = max(tag$id) + 1,
        eltern_id   = NA,
        bez         = "inno",
        bez_lang    = "innovation",
        beschr      = "Innovation",
        beschr_lang = NA,
        descr       = NA
      )
    )

  tag <-
    rbind(
      tag,
      data.frame(
        id          = max(tag$id) + 1,
        eltern_id   = NA,
        bez         = "portal",
        bez_lang    = "sv_datenportal",
        beschr      = "SV Datenportal",
        beschr_lang = "Das Datenportal des Stifterverbandes",
        descr       = NA
      )
    )

  tag <-
    rbind(
      tag,
      data.frame(
        id          = max(tag$id) + 1,
        eltern_id   = NA,
        bez         = "wis",
        bez_lang    = "wissenschaft",
        beschr      = "Wissenschaft",
        beschr_lang = NA,
        descr       = "science"
      )
    )


  tag$eltern_id[tag$bez_lang %in% c("bildung", "innovation", "wissenstransfer", "wissenschaft")] <- tag$id[tag$bez == "wwie"]
  tag$eltern_id[tag$bez_lang %in% c("wissenschaftler")] <- tag$id[tag$bez_lang == "wissenschaft"]
  tag$eltern_id[tag$bez_lang %in% c("fue")] <- tag$id[tag$bez_lang == "innovation"]
  tag$eltern_id[tag$bez_lang %in% c("informatik")] <- tag$id[tag$bez_lang == "mint"]
  tag$eltern_id[tag$bez_lang %in% c("hochschulbildungsreport")] <- tag$id[tag$bez == "mintdl_daten"]
  tag$eltern_id[tag$bez %in% c("wirt", "mint", "inter", "dtnstz", "wwie")] <- tag$id[tag$bez_lang == "sv_datenportal"]

  # ------

  # Nodes:

  nodes <- tag[,c("id", "beschr")]
  names(nodes)[2] <- "label"

  nodes[,"color.background"]           <- "#c5cdd2"
  nodes[,"color.border"]               <- "#195365"
  nodes[,"font.size"]                  <- 12
  nodes[,"color.highlight.border"]     <- "#195365"
  nodes[,"color.highlight.background"] <- "#195365"

  nodes$shape <- "dot"
  nodes$shape[nodes$label == "Datensatz"]      <- "database"
  nodes$shape[nodes$label == "SV Datenportal"] <- "box"
  nodes$shape[nodes$id %in% tag$id[tag$eltern_id == tag$id[tag$bez_lang == "datensatz"]]] <- "triangle"
  i <- c("Innovation", "Bildung", "Hochschule", "Wissen", "Forschung", "Forschung und Entwicklung", "Studium", "Wissenschaft")
  nodes$shape[nodes$label %in% i] <- "hexagon"
  rm(i)

  nodes$value <- 1
  nodes$value[nodes$shape %in% c("hexagon")] <- 4
  nodes$value[nodes$shape %in% c("box")] <- 6

  nodes$group <- NA

  i <- tag$id[tag$beschr == "Datensatz" | tag$beschr == "MINT-DataLab (Datensatz)"]
  i <- nodes$label %in% tag$beschr[tag$id %in% i | tag$eltern_id %in% i]
  nodes$group[i] <- "daten"

  i <- tag$id[tag$beschr %in% c("Innovation", "Forschung und Entwicklung", "Forschung", "FuE-Aufwendungen", "Wissenstransfer")]
  i <- nodes$label %in% tag$beschr[tag$id %in% i | tag$eltern_id %in% i]
  nodes$group[i] <- "innovation"

  i <- tag$id[tag$beschr %in% c("Hochschule", "Bildung", "Studium", "Studierende", "Lehre", "Lehrveranstaltungen")]
  i <- nodes$label %in% tag$beschr[tag$id %in% i | tag$eltern_id %in% i]
  nodes$group[i] <- "bildung"

  i <- tag$id[tag$beschr %in% c("SV Datenportal", "Wirtschaft", "International", "MINT", "Wissen", "Informatik", "Wissenschaft", "Wissenschaftler")]
  i <- nodes$label %in% tag$beschr[tag$id %in% i]
  nodes$group[i] <- "allgemein"


  i <- nodes$group == "daten"
  nodes$color.background[i] <- "#D3E5D9"
  nodes$color.border[i] <- "#3E694D"
  nodes$color.highlight.border[i]     <- "#91BEA0"
  nodes$color.highlight.background[i] <- nodes$color.highlight.border[i]
  rm(i)

  i <- nodes$group == "innovation"
  nodes$color.background[i] <- "#D5B9F6"
  nodes$color.border[i] <- "#48108D"
  nodes$color.highlight.border[i]     <- "#9650EB"
  nodes$color.highlight.background[i] <- nodes$color.highlight.border[i]
  rm(i)

  i <- nodes$group == "bildung"
  nodes$color.background[i] <- "#E9FE89"
  nodes$color.border[i] <- "#83A100"
  nodes$color.highlight.border[i]     <- "#AFD700"
  nodes$color.highlight.background[i] <- nodes$color.highlight.border[i]
  rm(i)

  i <- nodes$group == "allgemein"
  nodes$color.background[i] <- "#DFDBD1"
  nodes$color.border[i] <- "#5C5640"
  nodes$color.highlight.border[i]     <- "#AEA68B"
  nodes$color.highlight.background[i] <- nodes$color.highlight.border[i]
  rm(i)


  #nodes[,"font.size"] <- 12
  nodes$font.face <- "calibri"
  #nodes$value <- 1

  i <- c("SV Datenportal")
  nodes$font.size[nodes$label %in% i] <- 32
  nodes$value[nodes$label %in% i]     <- 40
  rm(i)

  i <- c("Bildung", "Innovation", "Wissen")
  nodes$font.size[nodes$label %in% i] <- 28
  nodes$value[nodes$label %in% i]     <- 34
  rm(i)

  i <- c("Hochschule", "Forschung", "Forschung und Entwicklung", "Studium", "Wissenschaft")
  nodes$font.size[nodes$label %in% i] <- 20
  nodes$value[nodes$label %in% i]     <- 22
  rm(i)


  # Edges
  edges <- data.frame(from = 1, to = 1)[0,]
  for (i in 1:nrow(tag)){
    if (!is.na(tag$eltern_id[i])){
      edges <- rbind(edges, data.frame(from = tag$id[i], to = tag$eltern_id[i]))
    }
  }
  rm(i)

  get_tag_by_bez <- function(x){
    (tag$id[tag$bez == x])[1]
  }

  i <- nrow(edges)
  edges <-
    rbind(
      edges,
      data.frame(
        from = c(get_tag_by_bez("lehra"), get_tag_by_bez("wimi"), get_tag_by_bez("prof"), get_tag_by_bez("bausl")),
        to   = c(get_tag_by_bez("schl"),  get_tag_by_bez("wiss"), get_tag_by_bez("wiss"), get_tag_by_bez("inter"))
      )
    )
  edges <-
    rbind(
      edges,
      data.frame(
        from = c(get_tag_by_bez("lehr"),  get_tag_by_bez("fue"),       get_tag_by_bez("fue_ex"), get_tag_by_bez("infor"),   get_tag_by_bez("wt")),
        to   = c(get_tag_by_bez("studm"), get_tag_by_bez("fue_erheb"), get_tag_by_bez("dritt"),  get_tag_by_bez("inf_std"), get_tag_by_bez("forsch"))
      )
    )
  edges <-
    rbind(
      edges,
      data.frame(
        from = c(get_tag_by_bez("lehr"),  get_tag_by_bez("wt"),   get_tag_by_bez("fue_erheb")),
        to   = c(get_tag_by_bez("lehrv"), get_tag_by_bez("inno"), get_tag_by_bez("wirt"))
      )
    )

  edges <-
    rbind(
      edges,
      data.frame(
        from = c(get_tag_by_bez("wis"), get_tag_by_bez("wis"),   get_tag_by_bez("wis")),
        to   = c(get_tag_by_bez("hs"),  get_tag_by_bez("forsch"), get_tag_by_bez("wt"))
      )
    )

  edges[,"value"] <- 2
  edges[1:3,"value"] <- 1.5
  edges[(i + 1):nrow(edges),"value"] <- 1
  edges[,"dashes"] <- FALSE
  edges[(i + 1):nrow(edges), "dashes"] <- TRUE
  edges$color <- "#b5bfc5"

  nodes$label <- paste0("<b>", nodes$label, "</b>") # style = 'padding: 20px;'

  return(
    list(
      nodes = nodes,
      edges = edges
    )
  )
}

draw_network <- function(daten, event_id = "none", randomSeed = 1){
  visNetwork(
    nodes  = daten$nodes,
    edges  = daten$edges,
    height = "500px",
    width  = "1000px"
  ) |>
    visEvents(select = sprintf("function(nodes) {Shiny.onInputChange('%s', nodes);;}", event_id)) |>
    visNodes(font = list(multi = "html"))  |>
    visEdges(scaling = list(min = 3.5, max = 3.5)) |>
    visLayout(randomSeed = randomSeed)
}
