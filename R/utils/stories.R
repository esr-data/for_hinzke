box::use(
  jsonlite[read_json],
  shiny[div, p, img, h5, tagList, a]
)

get_stories <- function(){
  json_data <- read_json("www/stories/search.json")
  output <-
    data.frame(
      href = NA,
      title = NA
    )[0,]
  for (i in 1:length(json_data)){
    output <-
      rbind(
        output,
        data.frame(
          href  = json_data[[i]]$href,
          title = json_data[[i]]$title
        )
      )
  }
  rm(json_data, i)
  output <- output[output$title != "Start",]
  output$handlungsfeld <-
    ifelse(
      grepl("/hf1_", output$href),
      1,
      ifelse(
        grepl("/hf2_", output$href),
        2,
        0
      )
    )
  output$folder <- unlist(lapply(strsplit(output$href, "/", fixed = TRUE), \(x) x[2]))
  output$author <- NA
  for (i in 1:nrow(output)){
    output$author[i] <- paste(get_authors(output$href[i]), collapse = ";")
  }
  output$id <- 1:nrow(output)
  return(output)
}

draw_card <- function(folder, title, authors, id, hf = 0){
  hf_marker <- div()
  hf_class  <- ""
  if (hf == 1) {
    #hf_marker <- div(class = "card-marker-hf1", "Bildung & Kompetenzen")
    hf_class  <- "hf1-card"
  } else if (hf == 2){
    #hf_marker <- div(class = "card-marker-hf2", "Forschung & Innovation")
    hf_class  <- "hf2-card"
  }
  authors <- paste(strsplit(authors, ";")[[1]], collapse = ", ")

  a(
    class = "card-link",
    href  = paste0("/#!/stories_inhalt?st=", id, "&hf=", hf),
    div(
      class = paste("quarto-grid-item card h-100 card-left", hf_class),
      p(
        class = "card-img-top",
        img(
          src   = sprintf("stories/posts/%s/preview.png", folder),
          class = "thumbnail-image card-img"
        )
      ),
      div(
        class = "card-body post-contents",
        h5(
          class = "no-anchor card-title listing-title",
          title
        ),
        hf_marker,
        div(
          class = "card-attribution card-text-small start",
          div(
            class = "listing-author-head",
            "Autor:innen"
          ),
          div(
            class = "listing-author",
            authors
          )
        )
      )
    )
  )
}

draw_cards <- function(hf = 0){
  if (hf > 0) stories <- stories[stories$handlungsfeld == hf,]
  tagList(
    apply(
      stories[,c("folder", "title", "author", "id", "handlungsfeld")],
      1,
      \(x) draw_card(x["folder"], x["title"], x["author"], x["id"], x["handlungsfeld"])
    )
  )
}

get_authors <- function(href){
  x <- readLines(paste0("www/stories/", href), n = 170)
  min_1 <- grep("quarto-title-meta-contents", x)
  min_2 <- grep("</div>", x, fixed = TRUE)
  min_2 <- (min_2[min_2 > min_1])[1]
  x <- x[(min_1 + 1):(min_2 - 1)]
  x <- gsub("<p>", "", x, fixed = TRUE)
  x <- gsub("</p>", "", x, fixed = TRUE)
  x <- trimws(x)
  return(x)
}

get_story_by_id <- function(id){
  if (is.null(id)) return(stories$href[1])
  if (length(id) != 1) return(stories$href[1])
  if (is.na(id)) return(stories$href[1])

  if (id %in% stories$id){
    return(stories$href[match(id, stories$id)])
  }
  return(stories$href[1])
}

stories <- get_stories()
