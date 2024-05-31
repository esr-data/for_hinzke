
box::use(
  . / manage_custome_theme[manage_custome_theme],
  . / transform_title_letter_case_by_theme[transform_title_letter_case_by_theme],
  . / make_num_pretty_ger[make_num_pretty_ger],
  . / make_comma_format_on_axis[make_comma_format_on_axis],
  
  magrittr[`%>%`],
  ggplot2[geom_col, geom_text, ggplot, labs, coord_flip, aes, scale_y_continuous,
          element_text, theme, element_blank, element_line, scale_fill_manual],
  forcats[fct_reorder],
  stringi[stri_wrap],
  stringr[str_c, str_remove],
  plotly[layout, config, plot_ly],
  dplyr[summarise],
  rlang[enquo],
  stats[reorder]
)

create_bar <- function(df, x_var, y_var,
                           plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
                           theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL,
                           xlabel_text = "", ylabel_text = "",
                           flipped = FALSE,
                           print_data_labels = TRUE,
                           remove_y_axis_text = FALSE, remove_x_axis_text = FALSE,
                           interactive = FALSE) {

  # browser()

  if (interactive) {
    return(
      create_bar_interactive(
        df, {{ x_var }}, {{ y_var }},
        plot_title, plot_subtitle, source, custom_caption,
        theme_and_color_set, custom_theme_and_color_set,
        xlabel_text, ylabel_text,
        flipped,
        print_data_labels,
        remove_y_axis_text, remove_x_axis_text
      )
    )
  } else {
    return(
      create_bar_static(
        df, {{ x_var }}, {{ y_var }},
        plot_title, plot_subtitle, source, custom_caption,
        theme_and_color_set, custom_theme_and_color_set,
        xlabel_text, ylabel_text,
        flipped,
        print_data_labels,
        remove_y_axis_text, remove_x_axis_text
      )
    )
  }
}

#' Used as internal function.
#' @noRd

create_bar_static <- function(
    df, x_var, y_var,
    plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
    theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL,
    xlabel_text = "", ylabel_text = "",
    flipped = FALSE,
    print_data_labels = TRUE,
    remove_y_axis_text = FALSE, remove_x_axis_text = FALSE){

  # browser()

  y_var_enquoded <- enquo(y_var)
  y_var_name <- str_remove(deparse(substitute(y_var_enquoded)), "~")

  selected_theme <- manage_custome_theme(theme_and_color_set, custom_theme_and_color_set)

  p <- ggplot(df, aes(x = if(flipped) reorder( {{ x_var }} , {{ y_var }} ) else reorder( {{ x_var }} , -{{ y_var }} ), y = {{ y_var }} )) +
    geom_col(fill = selected_theme[["theme_info"]][["bold_color"]], col = "white") +
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
    selected_theme[["theme"]] +
    theme(
      panel.grid = element_blank(),
      axis.title.y = element_text(angle = 90),
      axis.text.y = if(remove_y_axis_text) element_blank() else element_text(),
      axis.text.x = if(remove_x_axis_text) element_blank() else element_text(),

    ) +
    if(print_data_labels) {
      geom_text(
        aes(
          x = {{ x_var }},
          y = ifelse(
            {{ y_var }} > 0 & abs( {{y_var}} ) > .05 * max( {{ y_var }} ) | {{ y_var }} < 0 & abs( {{ y_var }} ) < .05 * max( {{ y_var }} ),
            {{ y_var }} - .035 * max( {{ y_var }} ),
            {{ y_var }} + .035 * max( {{ y_var }} )),
          label = format(round( {{ y_var }}, 1), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
        ),
        colour = ifelse(
          df[[y_var_name]] > 0 & abs(df[[y_var_name]]) > .05 * max(df[[y_var_name]]) | df[[y_var_name]] < 0 & abs(df[[y_var_name]]) > .05 * max(df[[y_var_name]]),
          "white",
          selected_theme[["theme_info"]][["bold_color"]]
        ),
        family = selected_theme[["theme_info"]][["text_family"]]
      )
    }

  if(flipped) {
    p <- p + coord_flip()
  }

  return(p)

}

#' Used as internal function.
#' @noRd

create_bar_interactive <- function(
    df, x_var, y_var,
    plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
    theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL,
    xlabel_text = "", ylabel_text = "",
    flipped = FALSE,
    print_data_labels = TRUE,
    remove_y_axis_text = FALSE, remove_x_axis_text = FALSE) {

  # browser()

  selected_theme <- manage_custome_theme(theme_and_color_set, custom_theme_and_color_set)

  x_var_enquoded <- enquo(x_var)
  y_var_enquoded <- enquo(y_var)
  x_var_name <- str_remove(deparse(substitute(x_var_enquoded)), "~")
  y_var_name <- str_remove(deparse(substitute(y_var_enquoded)), "~")

  df$y_data_label <- ifelse(
    df[[y_var_name]] > 0 & abs(df[[y_var_name]]) > .05 * max(df[[y_var_name]]) | df[[y_var_name]] < 0 & abs(df[[y_var_name]]) < .05 * max(df[[y_var_name]]),
    df[[y_var_name]] - .035 * max(df[[y_var_name]]),
    df[[y_var_name]] + .035 * max(df[[y_var_name]])
  )

  if (!flipped) {
    plot <- df %>%
      plot_ly(x = ~fct_reorder(.data[[x_var_name]], -.data[[y_var_name]]), y = ~ .data[[y_var_name]], type = 'bar', orientation = 'v')
  } else {
    plot <- df %>%
      plot_ly(x = ~.data[[y_var_name]], y = ~ fct_reorder(.data[[x_var_name]], .data[[y_var_name]]), type = 'bar', orientation = 'v')
  }

  # if (print_data_labels) {  # try to integrate print_data_labels, but failed for this package version
  #   plot <- plot %>% add_annotations(
  #     text = ~format(
  #       round(y_var, 1),
  #       big.mark = ".",
  #       decimal.mark = ",",
  #       scientific = FALSE,
  #       trim = TRUE),
  #     x = ~ {{ x_var }},
  #     y = ~ .data[["y_data_label"]],
  #     showarrow = FALSE
  #     # font = list(color = ifelse(
  #     #   .data[y_var] > 0 & abs( {{y_var }}) > .05 * max(.data[y_var]) | .data[y_var] < 0 & abs(.data[y_var]) < .05 * max(.data[y_var]),
  #     #   "black",
  #     #   "red"
  #     # ))
  #   )
  # }

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
      margin = list(t = 80, b = 70), # more space for title/subtitle and caption,
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
      legend = list(
        x = 0.5,
        y = -0.1,
        xanchor = "center",
        yanchor = "bottom",
        orientation = "h"
      ),
      annotations = list(
        list(
          text = ifelse(
            is.na(custom_caption),
            paste0(
              "N = ",
              ifelse(
                is.numeric(df[[x_var_name]]),
                make_num_pretty_ger(sum(df[[x_var_name]])),
                make_num_pretty_ger(nrow(df))
              ),
              ". Quelle: ", source, "."),
            custom_caption
          ),
          xref = "paper",
          yref = "paper",
          x = 1,
          y = -0.1,
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
