box::use(
  ../../R/utils/database[get_query],
  visNetwork[visNetwork, visEvents, visNodes, visEdges, visLayout, visInteraction, visPhysics, visOptions]
)

#' Missing description
#' @export
get_network_data <- function(){

  tag <- get_query("SELECT * FROM tag WHERE NOT bez IN ('covid19') AND NOT id IN (SELECT id FROM view_tag_baum WHERE beschr_pfad LIKE '%Zukunftsmission%' OR beschr_pfad LIKE '%Datensatz%')")

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
        bez         = "exp",
        bez_lang    = "explorer",
        beschr      = "Explorer",
        beschr_lang = "Daten-Explorer Magpie",
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
  # tag$eltern_id[tag$bez_lang %in% c("hochschulbildungsreport")] <- tag$id[tag$bez == "mintdl_daten"]
  tag$eltern_id[tag$bez %in% c("wirt", "mint", "inter", "dtnstz", "wwie")] <- tag$id[tag$bez_lang == "explorer"]

  # ------

  # Nodes:

  nodes <- tag[,c("id", "beschr")]
  names(nodes)[2] <- "label"

  nodes[,"color.background"]           <- "#c5cdd2"
  nodes[,"color.border"]               <- "#195365"
  nodes[,"font.size"]                  <- 20
  nodes[,"color.highlight.border"]     <- "#195365"
  nodes[,"color.highlight.background"] <- "#195365"

  nodes$shape <- "dot"
  nodes$shape[nodes$label %in% "Datensatz"]      <- "database"
  nodes$shape[nodes$label %in% "Explorer"] <- "box"
  nodes$shape[nodes$id %in% tag$id[tag$eltern_id %in% tag$id[tag$bez_lang %in% "datensatz"]]] <- "triangle"
  i <- c("Innovation", "Bildung", "Hochschule", "Wissen", "Forschung", "Forschung und Entwicklung", "Studium", "Wissenschaft")
  nodes$shape[nodes$label %in% i] <- "hexagon"
  rm(i)

  nodes$value <- 1
  nodes$value[nodes$shape %in% c("hexagon")] <- 4
  nodes$value[nodes$shape %in% c("box")] <- 6

  nodes$group <- NA

  i <- tag$id[tag$beschr %in% "Datensatz" | tag$beschr %in% "MINT-DataLab (Datensatz)"]
  i <- nodes$label %in% tag$beschr[tag$id %in% i | tag$eltern_id %in% i]
  nodes$group[i] <- "daten"

  i <- tag$id[tag$beschr %in% c("Innovation", "Forschung und Entwicklung", "Forschung", "FuE-Aufwendungen", "Wissenstransfer", "Finanzierung von FuE")]
  i <- nodes$label %in% tag$beschr[tag$id %in% i | tag$eltern_id %in% i]
  nodes$group[i] <- "Forschung & Innovation"

  i <- tag$id[tag$beschr %in% c("Hochschule", "Bildung", "Studium", "Studierende", "Lehre", "Lehrveranstaltungen", "Schule", "Lehrkräfte")]
  i <- nodes$label %in% tag$beschr[tag$id %in% i | tag$eltern_id %in% i]
  nodes$group[i] <- "Bildung & Kompetenzen"

  ebene_1 <- tag$id[tag$eltern_id %in% tag$id[tag$bez %in% "zmb"]]
  ebene_2 <- tag$id[tag$eltern_id %in% ebene_1]
  ebene_3 <- tag$id[tag$eltern_id %in% ebene_2]
  i <- nodes$label %in% tag$beschr[tag$id %in% c(ebene_1, ebene_2, ebene_3)]
  nodes$group[i] <- "Bildung & Kompetenzen"
  rm(ebene_1, ebene_2, ebene_3)

  i <- tag$id[tag$beschr %in% c("Explorer", "Wirtschaft", "International", "MINT", "Wissen", "Informatik", "Wissenschaft", "Wissenschaftler")]
  i <- nodes$label %in% tag$beschr[tag$id %in% i]
  nodes$group[i] <- "Allgemein"


  i <- nodes$group %in% "daten"
  nodes$color.background[i] <- "#D3E5D9"
  nodes$color.border[i] <- "#3E694D"
  nodes$color.highlight.border[i]     <- "#91BEA0"
  nodes$color.highlight.background[i] <- nodes$color.highlight.border[i]
  rm(i)

  i <- nodes$group %in% "Forschung & Innovation"
  nodes$color.background[i] <- "#D5B9F6"
  nodes$color.border[i] <- "#48108D"
  nodes$color.highlight.border[i]     <- "#9650EB"
  nodes$color.highlight.background[i] <- nodes$color.highlight.border[i]
  rm(i)

  i <- nodes$group %in% "Bildung & Kompetenzen"
  nodes$color.background[i] <- "#E9FE89"
  nodes$color.border[i] <- "#83A100"
  nodes$color.highlight.border[i]     <- "#AFD700"
  nodes$color.highlight.background[i] <- nodes$color.highlight.border[i]
  rm(i)

  i <- nodes$group %in% "Allgemein"
  nodes$color.background[i] <- "#d0d4da"
  nodes$color.border[i] <- "#195365"
  nodes$color.highlight.border[i]     <- "#8d9ca9"
  nodes$color.highlight.background[i] <- nodes$color.highlight.border[i]
  rm(i)


  #nodes[,"font.size"] <- 12
  nodes$font.face <- "calibri"
  #nodes$value <- 1

  i <- c("Explorer")
  nodes$font.size[nodes$label %in% i] <- 28
  nodes$value[nodes$label %in% i]     <- 40
  rm(i)

  i <- c("Bildung", "Innovation", "Wissen")
  nodes$font.size[nodes$label %in% i] <- 28
  nodes$value[nodes$label %in% i]     <- 34
  rm(i)

  i <- c("Hochschule", "Forschung", "Forschung und Entwicklung", "Studium", "Wissenschaft")
  nodes$font.size[nodes$label %in% i] <- 24
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

  position <- rlist::list.rbind(yaml::read_yaml("yml/explorer/network_positions.yml"))
  position[,1] <- unlist(position[,1])
  position[,2] <- unlist(position[,2])
  nodes$x <- position[match(nodes$label, rownames(position)),1]
  nodes$y <- position[match(nodes$label, rownames(position)),2]
  nodes$x[is.na(nodes$x)] <- 0
  nodes$y[is.na(nodes$y)] <- 0

  return(
    list(
      nodes = nodes,
      edges = edges
    )
  )
}

#' Missing description
#' @export

draw_network <- function(daten, event_id = "none", randomSeed = 38){
  visNetwork(
    nodes  = daten$nodes,
    edges  = daten$edges,
    height = "400px",
    width  = "1000px"
  ) |>
    visEvents(select = sprintf("function(nodes) {Shiny.onInputChange('%s', nodes);;}", event_id)) |>
    visNodes(font = list(multi = "html"), x = daten$nodes$x, y = daten$nodes$y)  |>
    visEdges(scaling = list(min = 3.5, max = 3.5)) |>
    visLayout(randomSeed = randomSeed) |>
    visInteraction(dragNodes = TRUE, navigationButtons = TRUE, dragView = FALSE, keyboard = TRUE) |>
    visPhysics(
      enabled = FALSE,
      stabilization = TRUE,
      barnesHut = list(
        "springLength"   = 90,
        "springConstant" = 0.25,
        "avoidOverlap"   = 0.15
      )
    ) |>
    visOptions(selectedBy = "group",
               highlightNearest = TRUE,
               nodesIdSelection =
                 list(
                   enabled = TRUE,
                   useLabels = "asdsad"
                 ))

}
