
draw_sidebar_home <- function(){
  tagList(
    div(
      style = "background-color: white;",
      bsButton("sbd_home_1", label = "home_1", class = "sidebar_dynamic_button", icon = icon("question")),
      bsButton("sbd_home_2", label = "home_2", class = "sidebar_dynamic_button", icon = icon("question")),
      bsButton("sbd_home_3", label = "home_3", class = "sidebar_dynamic_button", icon = icon("question")),
      bsButton("sb_home", label = "Zurück zum Start", class = "sidebar_dynamic_button", icon("rotate-left"))
    )
  )
}

draw_sidebar_stories <- function(){
  tagList(
    div(
      style = "background-color: white;",
      bsButton("sbd_stories_1", label = "stories_1", class = "sidebar_dynamic_button", icon = icon("question")),
      bsButton("sbd_stories_2", label = "stories_2", class = "sidebar_dynamic_button", icon = icon("question")),
      bsButton("sbd_stories_3", label = "stories_3", class = "sidebar_dynamic_button", icon = icon("question")),
      bsButton("sb_home", label = "Zurück zum Start", class = "sidebar_dynamic_button", icon("rotate-left"))
    )
  )
}

draw_sidebar_monitor <- function(){
  tagList(
    div(
      style = "background-color: white;",
      bsButton("sbd_monitor_1", label = "monitor_1", class = "sidebar_dynamic_button", icon = icon("question")),
      bsButton("sbd_monitor_2", label = "monitor_2", class = "sidebar_dynamic_button", icon = icon("question")),
      bsButton("sbd_monitor_3", label = "monitor_3", class = "sidebar_dynamic_button", icon = icon("question")),
      bsButton("sb_home", label = "Zurück zum Start", class = "sidebar_dynamic_button", icon("rotate-left"))
    )
  )
}

draw_sidebar_explorer <- function(){
  tagList(
    div(
      style = "background-color: white;",
      bsButton("sbd_explorer_1", label = "explorer_1", class = "sidebar_dynamic_button", icon = icon("question")),
      bsButton("sbd_explorer_2", label = "explorer_2", class = "sidebar_dynamic_button", icon = icon("question")),
      bsButton("sbd_explorer_3", label = "explorer_3", class = "sidebar_dynamic_button", icon = icon("question")),
      bsButton("sb_home", label = "Zurück zum Start", class = "sidebar_dynamic_button", icon("rotate-left"))
    )
  )
}

draw_sidebar_studies <- function(){
  tagList(
    div(
      style = "background-color: white;",
      bsButton("sbd_studies_1", label = "studies_1", class = "sidebar_dynamic_button", icon = icon("question")),
      bsButton("sbd_studies_2", label = "studies_2", class = "sidebar_dynamic_button", icon = icon("question")),
      bsButton("sbd_studies_3", label = "studies_3", class = "sidebar_dynamic_button", icon = icon("question")),
      bsButton("sb_home", label = "Zurück zum Start", class = "sidebar_dynamic_button", icon("rotate-left"))
    )
  )
}
