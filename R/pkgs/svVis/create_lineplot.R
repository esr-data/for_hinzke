
box::use(
  . / manage_custome_theme[manage_custome_theme],
  . / transform_title_letter_case_by_theme[transform_title_letter_case_by_theme],
  . / make_comma_format_on_axis[make_comma_format_on_axis],
  
  ggplot2[ggplot, aes, geom_line, geom_point, labs, xlim, ylim, scale_color_manual, 
          scale_linetype_discrete, scale_y_continuous, scale_x_continuous, 
          scale_shape_discrete, geom_hline, geom_vline, theme, element_text,
          geom_col],
  stringi[stri_wrap],
  stringr[str_c, str_remove] ,
  magrittr[`%>%`] ,
  dplyr[select, pull, summarise] ,
  glue[glue],
  rlang[enquo, quo_get_expr, as_name]

)

create_lineplot <- function(
    df, x_var, y_var,
    plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
    theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL,
    group_color = NULL, group_linetyp = NULL, group_shape = NULL,
    highlight_x_axis = FALSE, highlight_y_axis = FALSE,
    xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL,
    xlabel_text = "", ylabel_text = "",
    legend_title = "",
    interactive = FALSE){

  # browser()

  if (interactive) {
    return(
      create_lineplot_interactive(
        df, {{ x_var }}, {{ y_var }},
        plot_title, plot_subtitle, source, custom_caption,
        theme_and_color_set, custom_theme_and_color_set,
        {{ group_color }}, {{ group_linetyp }}, {{ group_shape }},
        highlight_x_axis, highlight_y_axis,
        xmin, xmax, ymin, ymax,
        xlabel_text, ylabel_text,
        legend_title
      )
    )
  } else {
    return(
      create_lineplot_static(
        df, {{ x_var }}, {{ y_var }},
        plot_title, plot_subtitle, source, custom_caption,
        theme_and_color_set, custom_theme_and_color_set,
        {{ group_color }},{{ group_linetyp }}, {{ group_shape }},
        highlight_x_axis, highlight_y_axis,
        xmin, xmax, ymin, ymax,
        xlabel_text, ylabel_text,
        legend_title
      )
    )
  }
}

create_lineplot_static <- function(
    df, x_var, y_var,
    plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
    theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL,
    group_color = NULL, group_linetyp = NULL, group_shape = NULL,
    highlight_x_axis = FALSE, highlight_y_axis = FALSE,
    xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL,
    xlabel_text = "", ylabel_text = "",
    legend_title = ""){

  # browser()

  selected_theme <- manage_custome_theme(theme_and_color_set, custom_theme_and_color_set)

  test_group_shape <-  !is.null(quo_get_expr(enquo(group_shape)))

  #Definition of max/min values when no limits are provided.

  x_max_default <- pull(summarise(df, max = max( {{ x_var }} )))
  x_min_default <- pull(summarise(df, min = min( {{ x_var }} )))

  y_max_default <- pull(summarise(df, max = max( {{ y_var }} )))
  y_min_default <- pull(summarise(df, min = min( {{ y_var }} )))

  # base ggplot

  base_line <- ggplot(df, aes(x = {{ x_var }}, y = {{ y_var }}, color = {{ group_color }}, linetype = {{ group_linetyp }} )) +
    geom_line()

  if(test_group_shape)  {
    base_line <- base_line + geom_point(aes(shape = {{ group_shape }}))
  }

  # labelled ggplot

  labelled_line <- base_line +
    labs(
      x = str_c(stri_wrap(xlabel_text, width = 30), collapse = "\n"),
      y = str_c(stri_wrap(ylabel_text, width = 30), collapse = "\n"),
      title = transform_title_letter_case_by_theme(plot_title, selected_theme),
      subtitle = str_c(stri_wrap(plot_subtitle, width = 160), collapse = "\n"),
      caption = str_c(stri_wrap(str_c("Quelle: ", source), width = 160), collapse = "\n")
    ) +
    xlim((if (is.null(xmin)){x_min_default} else {xmin}),(if (is.null(xmax)){x_max_default} else {xmax})) +
    ylim((if (is.null(ymin)){y_min_default} else {ymin}),(if (is.null(ymax)){y_max_default} else {ymax}))

  # styling

  styled_line <- labelled_line +
    scale_x_continuous(labels = make_comma_format_on_axis()) +
    scale_y_continuous(labels = make_comma_format_on_axis()) +
    scale_color_manual(name = str_c(stri_wrap(legend_title, width = 30), collapse = "\n"), values = selected_theme[["color"]]) +
    scale_linetype_discrete(name = str_c(stri_wrap(legend_title, width = 30), collapse = "\n")) +
    scale_shape_discrete(name= str_c(stri_wrap(legend_title, width = 30), collapse = "\n")) +
    selected_theme[["theme"]] +
    theme(
      axis.title.y = element_text(angle = 90)
    )

  if (highlight_x_axis)  {
    styled_line <- styled_line +
      geom_hline(yintercept = 0, linetype = "dashed", selected_theme[["theme_info"]][["bold_color"]])
  }

  if (highlight_y_axis)  {
    styled_line <- styled_line +
      geom_vline(xintercept = 0, linetype = "dashed", selected_theme[["theme_info"]][["bold_color"]])
  }

  styled_line
}

create_lineplot_interactive <- function(
    df, x_var, y_var,
    plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
    theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL,
    group_color = NULL, group_linetype = NULL, group_shape = NULL,
    highlight_x_axis = FALSE, highlight_y_axis = FALSE,
    xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL,
    xlabel_text = "", ylabel_text = "",
    legend_title = ""){

  # browser()

  # maybe ToDo: Not 100 % satisfied with legend when more group_* params are given

  selected_theme <- manage_custome_theme(theme_and_color_set, custom_theme_and_color_set)

  test_group_color <-  !is.null(quo_get_expr( enquo(group_color)))
  test_group_shape <-  !is.null( quo_get_expr( enquo(group_shape)))
  test_group_linetype <-  !is.null( quo_get_expr( enquo(group_linetype)))

  df$group_color_var <- if (test_group_color) df[[ as_name( enquo(group_color))]] else {""} # not so nice workaround, any better ideas? Can not gt an if/else in plot_ly direct
  df$group_shape_var <- if (test_group_shape) df[[ as_name( enquo(group_shape))]] else {""} # not so nice workaround, any better ideas?
  df$group_linetype_var <- if (test_group_linetype) df[[ as_name( enquo(group_linetype))]] else {""}

  x_var_enquoded <- enquo(x_var)
  x_var_name <- str_remove(deparse(substitute(x_var_enquoded)), "~")
  y_var_enquoded <- enquo(y_var)
  y_var_name <- str_remove(deparse(substitute(y_var_enquoded)), "~")

  df$x_var <- df[[x_var_name]]
  df$y_var <- df[[y_var_name]]

  # data wrangling

  p <- df %>%
    mutate(
      hover_text =  case_when(
        xlabel_text != "" & ylabel_text != "" ~ glue("{xlabel_text}: {x_var}\n{ylabel_text}: {y_var}"),
        TRUE ~ glue("{x_var}\n{y_var}")
      )
    ) %>%

    # plotting

    plot_ly(
      x = ~ x_var,
      y = ~ y_var,
      type = 'scatter',
      mode = ifelse(test_group_shape, 'lines+markers', 'lines'),
      color = ~ group_color_var,
      colors = selected_theme[["color"]],
      linetype = ~ group_linetype_var,
      text = ~ hover_text,
      hoverinfo = 'text'
    )

  # layout

  p <- p %>% layout(
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
        family = selected_theme[["theme_info"]][["text_family"]])
    ),
    xaxis = list( # dont know why, but sometimes x an y axis goes wild (see zeroline)
      title = xlabel_text,
      range = c(xmin, xmax),
      titlefont = list(
        color = selected_theme[["theme_info"]][["bold_color"]],
        family = selected_theme[["theme_info"]][["text_family"]]
      ),
      zeroline = ifelse(highlight_y_axis, TRUE, FALSE),
      zerolinecolor = selected_theme[["theme_info"]][["bold_color"]],
      tickfont = list(
        color = selected_theme[["theme_info"]][["bold_color"]],
        family = selected_theme[["theme_info"]][["text_family"]]
      )
    ),
    yaxis = list(
      title = ylabel_text,
      range = c(ymin, ymax),
      titlefont = list(
        color = selected_theme[["theme_info"]][["bold_color"]],
        family = selected_theme[["theme_info"]][["text_family"]]
      ),
      zeroline = ifelse(highlight_x_axis, TRUE, FALSE),
      zerolinecolor = selected_theme[["theme_info"]][["bold_color"]],
      tickfont = list(
        color = selected_theme[["theme_info"]][["bold_color"]],
        family = selected_theme[["theme_info"]][["text_family"]]
      )
    ),
    hovermode = 'closest',
    legend = list(
      x = 0.5,
      y = -0.3,
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
        y = -0.225,
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
          paste0("Quelle: ", source, "."),
          custom_caption
        ),
        xref = "paper",
        yref = "paper",
        x = 1,
        y = -0.3,
        xanchor = "right",
        yanchor = "center",
        showarrow = FALSE,
        font = list(
          size = 9,
          color = selected_theme[["theme_info"]][["bold_color"]],
          family = selected_theme[["theme_info"]][["text_family"]]
        )
      )
    ),
    colorway = selected_theme[["color"]]
  ) %>%

    # config

    config(
      modeBarButtonsToRemove = c("lasso2d", "zoomIn2d", "zoomOut2d", "hoverCompareCartesian"),
      locale = "de",
      displaylogo = FALSE
    )

  return(suppressWarnings(print(p))) # suppresses warnings if a grouping var is not given and the else-defaults are used, see: https://stackoverflow.com/questions/64525470/how-do-i-suppress-warnings-with-plotly
}
