box::use(
  . / manage_custome_theme[manage_custome_theme],
  . / transform_title_letter_case_by_theme[transform_title_letter_case_by_theme],
  . / make_num_pretty_ger[make_num_pretty_ger],
  . / make_comma_format_on_axis[make_comma_format_on_axis],
  
  magrittr[`%>%`],
  ggplot2[geom_col, geom_text, ggplot, labs, coord_flip, aes, scale_y_continuous,
          element_text, theme, element_blank, element_line, scale_fill_manual],
  stringr[str_c, str_remove],
  forcats[fct_reorder, fct_inorder],
  stringi[stri_wrap],
  rlang[enquo],
  stats[reorder]
)


create_bar_grouped <- function(df, x_var, y_var, group_var,
                       plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
                       theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL,
                       xlabel_text = "", ylabel_text = "", legend_title = "",
                       flipped = FALSE, mode = "stack",
                       remove_y_axis_text = FALSE, remove_x_axis_text = FALSE,
                       interactive = FALSE) {

  # browser()

  if (interactive) {
    return(
      create_bar_grouped_interactive(
        df, {{ x_var }}, {{ y_var }}, {{ group_var }},
        plot_title, plot_subtitle, source, custom_caption,
        theme_and_color_set, custom_theme_and_color_set,
        xlabel_text, ylabel_text, legend_title,
        flipped, mode,
        remove_y_axis_text, remove_x_axis_text
      )
    )
  } else {
    return(
      create_bar_grouped_static(
        df, {{ x_var }}, {{ y_var }}, {{ group_var }},
        plot_title, plot_subtitle, source, custom_caption,
        theme_and_color_set, custom_theme_and_color_set,
        xlabel_text, ylabel_text, legend_title,
        flipped, mode,
        remove_y_axis_text, remove_x_axis_text
      )
    )
  }
}

#' Used as internal function.
#' @noRd

create_bar_grouped_static <- function(
    df, x_var, y_var, group_var,
    plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
    theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL,
    xlabel_text = "", ylabel_text = "", legend_title = "",
    flipped = FALSE, mode = "stack",
    remove_y_axis_text = FALSE, remove_x_axis_text = FALSE){

  # browser()

  y_var_enquoded <- enquo(y_var)
  y_var_name <- str_remove(deparse(substitute(y_var_enquoded)), "~")

  selected_theme <- manage_custome_theme(theme_and_color_set, custom_theme_and_color_set)

  p <- ggplot(df, aes(x = if(flipped) reorder( {{ x_var }} , {{ y_var }} ) else reorder( {{ x_var }} , -{{ y_var }} ), y = {{ y_var }} )) +
    geom_col(aes(fill = fct_inorder( {{ group_var }} )), position = mode) +
    labs(
      x = xlabel_text,
      y = ylabel_text,
      title = transform_title_letter_case_by_theme(plot_title, selected_theme),
      subtitle = str_c(stri_wrap(plot_subtitle, width = 160), collapse = "\n"),
      caption = ifelse(
        is.na(custom_caption),
        str_c(stri_wrap(str_c("Quelle: ", source), width = 160), collapse = "\n"),
        custom_caption
      )
    ) +
    scale_y_continuous(labels = make_comma_format_on_axis()) +
    scale_fill_manual(values = selected_theme[["color"]], name = legend_title) +
    selected_theme[["theme"]] +
    theme(
      panel.grid = element_blank(),
      axis.title.y = element_text(angle = 90),
      axis.text.y = if(remove_y_axis_text) element_blank() else element_text(),
      axis.text.x = if(remove_x_axis_text) element_blank() else element_text(),

    )

  if(flipped) {
    p <- p + coord_flip()
  }

  return(p)

}

#' Used as internal function.
#' @noRd

create_bar_grouped_interactive <- function(
    df, x_var, y_var, group_var,
    plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
    theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL,
    xlabel_text = "", ylabel_text = "", legend_title = "",
    flipped = FALSE, mode = "stack",
    print_data_labels = TRUE,
    remove_y_axis_text = FALSE, remove_x_axis_text = FALSE) {

  # browser()

  selected_theme <- manage_custome_theme(theme_and_color_set, custom_theme_and_color_set)

  x_var_enquoded <- enquo(x_var)
  y_var_enquoded <- enquo(y_var)
  x_var_name <- str_remove(deparse(substitute(x_var_enquoded)), "~")
  y_var_name <- str_remove(deparse(substitute(y_var_enquoded)), "~")
  group_var_enquoded <- enquo(group_var)
  group_var_name <- str_remove(deparse(substitute(group_var_enquoded)), "~")


  df$y_data_label <- ifelse(
    df[[y_var_name]] > 0 & abs(df[[y_var_name]]) > .05 * max(df[[y_var_name]]) | df[[y_var_name]] < 0 & abs(df[[y_var_name]]) < .05 * max(df[[y_var_name]]),
    df[[y_var_name]] - .035 * max(df[[y_var_name]]),
    df[[y_var_name]] + .035 * max(df[[y_var_name]])
  )

  df$hovertext <- paste0("Gruppe: ", df[[x_var_name]], "<br>",
                         "Kategorie: ", df[[group_var_name]], "<br>",
                         "Wert: ", make_num_pretty_ger(round(df[[y_var_name]], 1))
                  )

  if (!flipped) {
    plot <- df %>%
      plot_ly(
        x = ~fct_reorder(.data[[x_var_name]], -.data[[y_var_name]]),
        y = ~ .data[[y_var_name]],
        color = ~ .data[[group_var_name]],
        colors = selected_theme[["color"]],
        type = 'bar',
        orientation = 'v',
        hoverinfo = "text",
        hovertext = ~ hovertext
      )
  } else {
    plot <- df %>%
      plot_ly(
        x = ~.data[[y_var_name]],
        y = ~ fct_reorder(.data[[x_var_name]], .data[[y_var_name]]),
        color = ~ .data[[group_var_name]],
        colors = selected_theme[["color"]],
        type = 'bar', orientation = 'v',
        hoverinfo = "text",
        hovertext = ~ hovertext
      )
  }

  plot <- plot %>%
    layout(
      title = list(
        text = paste0(
          "<b>",
          transform_title_letter_case_by_theme(plot_title, selected_theme),
          "</b> <br> <sub> <i>",
          plot_subtitle,
          "</i> </sub>"
        ),
        xref = "paper",
        x = 0,
        font = list(
          size = 16,
          color = selected_theme[["theme_info"]][["bold_color"]],
          family = selected_theme[["theme_info"]][["text_family"]]
        )
      ),
      barmode = mode,
      margin = list(t = 80, b = 90), # more space for title/subtitle and caption,
      hoverlabel = list(
        font = list(
          color = "white",
          family = selected_theme[["theme_info"]][["text_family"]]
        )
      ),
      xaxis = list(
        title = xlabel_text,
        showticklabels = !remove_x_axis_text,
        zeroline = FALSE,
        titlefont = list(
          color = selected_theme[["theme_info"]][["bold_color"]],
          family = selected_theme[["theme_info"]][["text_family"]]
        ),
        tickfont = list(
          color = selected_theme[["theme_info"]][["bold_color"]],
          family = selected_theme[["theme_info"]][["text_family"]]
        )
      ),
      yaxis = list(
        title = ylabel_text,
        showticklabels = !remove_y_axis_text,
        zeroline = FALSE,
        titlefont = list(
          color = selected_theme[["theme_info"]][["bold_color"]],
          family = selected_theme[["theme_info"]][["text_family"]]
        ),
        tickfont = list(
          color = selected_theme[["theme_info"]][["bold_color"]],
          family = selected_theme[["theme_info"]][["text_family"]]
        )
      ),
      hovermode = 'closest',
      legend = list(
        x = 0.5,
        y = -0.25,
        xanchor = "center",
        yanchor = "bottom",
        orientation = "h"
      ),
      annotations = list(
        list(
          text = legend_title,
          xref = "paper",
          yref = "paper",
          x = 0.5,
          y = -0.175,
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(
            size = 11,
            color = selected_theme[["theme_info"]][["bold_color"]],
            family = selected_theme[["theme_info"]][["text_family"]]
            )
          ),
        list(
          text = ifelse(
            is.na(custom_caption),
            paste0(
              "N = ",
              ifelse(
                is.numeric(df[[y_var_name]]),
                make_num_pretty_ger(sum(df[[y_var_name]])),
                make_num_pretty_ger(nrow(df))
              ),
              ". Quelle: ", source, "."),
            custom_caption
          ),
          xref = "paper",
          yref = "paper",
          x = 1,
          y = -0.2,
          xanchor = "right",
          yanchor = "top",
          showarrow = FALSE,
          font = list(
            size = 9,
            color = selected_theme[["theme_info"]][["bold_color"]],
            family = selected_theme[["theme_info"]][["text_family"]]
          )
        )
      )
    ) %>%
    layout(colorway = selected_theme[["color"]]) %>%

    # config

    config(
      modeBarButtonsToRemove = c("lasso2d", "zoomIn2d", "zoomOut2d"),
      locale = "de",
      displaylogo = FALSE
    )

  return(plot)
}
