
box::use(
  shiny[div, icon, p, restoreInput, HTML, getCurrentTheme, actionButton],
  htmltools[tags, validateCssUnit, singleton],
  shinyWidgets,
  bsplus[
    bs_embed_tooltip
  ]
)

#' Missing description
#' @export

draw_zurueck_button <- function(){
  actionButton(
    "zuruck",
    label = "zurück",
    class = c("zuruck_button", "button_klein"),
    onclick = "goBackPage()"
  )
}

#' Missing description
#' @export

draw_under_construction <- function(){
  div(
    style = "display: flex; color: var(--red); font-size: 30px; margin-top: 30px;",
    style = "display: flex; color: var(--red); font-size: 30px; margin-top: 30px;",
    icon(
      style = "margin-right: 10px;",
      "screwdriver-wrench"
    ),
    p("Under construction ...")
  )
}

#' Missing description
#' @export

draw_search <- function(
    inputId,
    label = NULL,
    value = "",
    placeholder = NULL,
    btnSearch = NULL,
    btnReset = NULL,
    btnClass = "btn-default btn-outline-secondary",
    resetValue = "",
    width = NULL
)
{
  value <- restoreInput(id = inputId, default = value)
  btnClass <- rep_len(btnClass, length.out = 2)

  tagSearch <-
    tags$button(
      class = "btn btn-addon action-button",
      class = btnClass[1],
      id = paste0(inputId, "_search"),
      type = "button",
      btnSearch,
      style = if (is.null(btnSearch)) css(display = "none")
    )

  tagReset <-
    tags$button(
      class = "btn btn-addon action-button",
      class = btnClass[2],
      id = paste0(inputId, "_reset"),
      type = "button",
      btnReset,
      style = if (is.null(btnReset)) css(display = "none")
    )

  tags$div(
    class = "form-group shiny-input-container",
    style = paste0("width: ", validateCssUnit(width)),
    shinyWidgets:::label_input(inputId, label),
    tags$div(
      id = inputId,
      `data-reset` = !is.null(resetValue),
      `data-reset-value` = resetValue,
      class = "input-group search-text",
      tags$input(
        id = paste0(inputId, "_text"),
        style = "border-radius: 0px;",
        type = "text",
        class = "form-control",
        value = value,
        placeholder = placeholder
      ),
      shinyWidgets:::markup_search_input_group_button(
        tagReset,
        tagSearch,
        btnSearch,
        btnReset,
        theme_func = getCurrentTheme
      )
    ),
    shinyWidgets:::html_dependency_input_icons()
  )
}

#' Missing description
#' @export

create_callout_info <- function(text, titel = "Information"){
  div(
    class = "callout-info",
    div(
      class = "callout-info-head",
      shiny::icon("circle-info"),
      div(
        class = "callout-info-h",
        titel
      )
    ),
    div(
      class = "callout-info-body",
      text
    )
  )
}

#' Missing description
#' @export
#'
draw_save_and_share_buttons <- function(ns){
  div(
    tags$label(
      class = "control-label",
      "Speichern & Teilen",
      div(
        actionButton(
          ns("speichern"),
          icon  = icon("floppy-disk", class = "fa-solid"),
          label = "",
          class = "button_icon"
        ),
        actionButton(
          ns("teilen"),
          icon  = icon("share-nodes"),
          label = "",
          class = "button_icon"
        )
      )
    )
  )
}


#' Missing description
#' @export
#'
add_info <- function(content, id = "tip", placement = "top", style = NULL){
  bs_embed_tooltip(
    actionButton(
      id,
      label = "",
      icon  = icon("circle-info", class = "fa-solid"),
      class = "button_icon",
      style = style
    ),
    title =
      HTML(
        sprintf(
          "<h4>Hilfe</h4>%s",
          content
        )
      ),
    placement = placement
  )
}

#' Missing description
#' @export
#'
add_tooltip <- function(tag, content, placement = "top"){
  bs_embed_tooltip(tag, title = HTML(content), placement = placement, delay = list(show = 650, hide = 100))
}

#' @export
get_picker_options <- function(...){
  list(
    `none-selected-text` = "nichts ausgewählt",
    `select-all-text`    = "alle auswählen",
    `deselect-all-text`  = "nichts auswählen",
    `live-search`        = TRUE,
    ...
  )
}
