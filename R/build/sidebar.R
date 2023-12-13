
#' Necessary Packages/Functions

box::use(
  shiny[tagList, div, icon, actionButton]
)

#' Missing description
#' @export

draw_sidebar_home <- function(){
  tagList(
    div(
      style = "background-color: white;",
      # actionButton("sbd_home_1", label = "home_1", class = "sidebar_dynamic_button", icon = icon("question")),
      # actionButton("sbd_home_2", label = "home_2", class = "sidebar_dynamic_button", icon = icon("question")),
      # actionButton("sbd_home_3", label = "home_3", class = "sidebar_dynamic_button", icon = icon("question")),
      draw_help(),
      draw_to_start()
    )
  )
}

#' Missing description
#' @export

draw_sidebar_stories <- function(){
  tagList(
    div(
      style = "background-color: white;",
      # actionButton("sbd_stories_1", label = "stories_1", class = "sidebar_dynamic_button", icon = icon("question")),
      # actionButton("sbd_stories_2", label = "stories_2", class = "sidebar_dynamic_button", icon = icon("question")),
      # actionButton("sbd_stories_3", label = "stories_3", class = "sidebar_dynamic_button", icon = icon("question")),
      draw_help(),
      draw_to_start()
    )
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

draw_sidebar_explorer <- function(){
  tagList(
    div(
      style = "background-color: white;",
      actionButton("sbd_explorer_suche",     label = "Suche",       class = "sidebar_dynamic_button", icon = icon("magnifying-glass")),
      actionButton("sbd_explorer_indikator", label = "Indikator",   class = "sidebar_dynamic_button", icon = icon("chart-pie")),
      actionButton("sbd_explorer_vergleich", label = "Vergleichen", class = "sidebar_dynamic_button", icon = icon("greater-than-equal")),
      actionButton("sbd_explorer_datensatz", label = "Datensätze",  class = "sidebar_dynamic_button", icon = icon("database")),
      draw_help(),
      draw_to_start()
    )
  )
}

#' Missing description
#' @export

draw_sidebar_studies <- function(){
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

draw_to_start <- function(){
  actionButton("sb_home", label = "Zurück zum Start", class = "sidebar_dynamic_button", icon("rotate-left"))
}

#' Missing description
#' @export

draw_help <- function(){
  actionButton("sb_help", label = "Hilfe", class = "sidebar_dynamic_button", icon("circle-question"))
}

