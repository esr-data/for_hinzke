box::use(
  ../../R/utils/database[get_query],
  visNetwork[visNetwork, visEvents, visNodes, visEdges, visLayout]
)

#' Missing description
#' @export
get_network_data <- function(){

  tag <- get_query("SELECT * FROM tag WHERE NOT bez IN ('covid19')")

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
  nodes$shape[nodes$label %in% "Datensatz"]      <- "database"
  nodes$shape[nodes$label %in% "SV Datenportal"] <- "box"
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

  i <- tag$id[tag$beschr %in% c("Innovation", "Forschung und Entwicklung", "Forschung", "FuE-Aufwendungen", "Wissenstransfer")]
  i <- nodes$label %in% tag$beschr[tag$id %in% i | tag$eltern_id %in% i]
  nodes$group[i] <- "innovation"

  i <- tag$id[tag$beschr %in% c("Hochschule", "Bildung", "Studium", "Studierende", "Lehre", "Lehrveranstaltungen")]
  i <- nodes$label %in% tag$beschr[tag$id %in% i | tag$eltern_id %in% i]
  nodes$group[i] <- "bildung"

  ebene_1 <- tag$id[tag$eltern_id %in% tag$id[tag$bez %in% "zmb"]]
  ebene_2 <- tag$id[tag$eltern_id %in% ebene_1]
  ebene_3 <- tag$id[tag$eltern_id %in% ebene_2]
  i <- nodes$label %in% tag$beschr[tag$id %in% c(ebene_1, ebene_2, ebene_3)]
  nodes$group[i] <- "bildung"
  rm(ebene_1, ebene_2, ebene_3)

  i <- tag$id[tag$beschr %in% c("SV Datenportal", "Wirtschaft", "International", "MINT", "Wissen", "Informatik", "Wissenschaft", "Wissenschaftler")]
  i <- nodes$label %in% tag$beschr[tag$id %in% i]
  nodes$group[i] <- "allgemein"


  i <- nodes$group %in% "daten"
  nodes$color.background[i] <- "#D3E5D9"
  nodes$color.border[i] <- "#3E694D"
  nodes$color.highlight.border[i]     <- "#91BEA0"
  nodes$color.highlight.background[i] <- nodes$color.highlight.border[i]
  rm(i)

  i <- nodes$group %in% "innovation"
  nodes$color.background[i] <- "#D5B9F6"
  nodes$color.border[i] <- "#48108D"
  nodes$color.highlight.border[i]     <- "#9650EB"
  nodes$color.highlight.background[i] <- nodes$color.highlight.border[i]
  rm(i)

  i <- nodes$group %in% "bildung"
  nodes$color.background[i] <- "#E9FE89"
  nodes$color.border[i] <- "#83A100"
  nodes$color.highlight.border[i]     <- "#AFD700"
  nodes$color.highlight.background[i] <- nodes$color.highlight.border[i]
  rm(i)

  i <- nodes$group %in% "allgemein"
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

#' Missing description
#' @export

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
