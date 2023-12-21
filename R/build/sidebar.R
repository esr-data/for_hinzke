
#' Necessary Packages/Functions

box::use(
  shiny[tagList, div, icon, actionButton],
  ../../R/pages/explorer[report_explorer_subpages]
)

explorer_subpages <- report_explorer_subpages()

#' Missing description
#' @export

draw_sidebar_home <- function(){
  tagList(
    draw_help(),
    draw_to_start()
  )
}

#' Missing description
#' @export

draw_sidebar_stories <- function(){
  tagList(
    draw_help(),
    draw_to_start()
  )
}

#' Missing description
#' @export

draw_sidebar_monitor <- function(){
  tagList(
    div(
      style = "background-color: white;",
      draw_help(),
      draw_to_start()
    )
  )
}

#' Missing description
#' @export

draw_sidebar_explorer <- function(url = NULL){
  tagList(
    apply(
      explorer_subpages, 1,
      \(x){
        actionButton(
          inputId = x["id"],
          label   = x["label"],
          class   =
            paste0(
              "sidebar_dynamic_button",
              ifelse(x["url"] %in% url, " btn_selected", "")
            )
        )
      }
    ),
    draw_help(),
    draw_to_start()
  )
}

#' Missing description
#' @export

draw_sidebar_studies <- function(url = NULL){
  tagList(
    draw_help(),
    draw_to_start()
  )
}

#' Missing description
#' @export

draw_to_start <- function(url = NULL){
  actionButton("sb_home", label = "Zurück zum Start", class = "sidebar_dynamic_button", icon("rotate-left"))
}

#' Missing description
#' @export

draw_help <- function(url = NULL){
  actionButton("sb_help", label = "Hilfe", class = "sidebar_dynamic_button", icon("circle-question"))
}

