
box::use(
  . / manage_custome_theme[manage_custome_theme],
  . / transform_title_letter_case_by_theme[transform_title_letter_case_by_theme],
  
  magrittr[`%>%`],
  sf[st_as_sf, st_simplify],
  dplyr[left_join, filter, mutate],
  tibble[tibble],
  ggplot2[ggplot, geom_sf, theme, labs, scale_fill_gradient2, scale_fill_manual,
          element_text, element_blank, aes],
  ggpubr[theme_pubr],
  plotly[plot_ly, add_sf, config, layout, colorbar, hide_legend],
  stringr[str_c, str_remove],
  stringi[stri_wrap],
  rlang[enquo, ensym]
)


germany_choropleth_federal_states <- readRDS("~/test_datenportal_2/data/germany_choropleth_federal_states.rds")
middle_points_of_ger_federal_states <- readRDS("~/test_datenportal_2/data/middle_points_of_ger_federal_states.rds")


create_choropleth_map_germany <- function(df, var,
                                          level = "Bundesland",
                                          plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
                                          theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL,
                                          interactive = FALSE) {

  # browser()

  if (interactive) {
    return(
      create_choropleth_map_germany_interactive(
        df, {{ var }},
        level,
        plot_title, plot_subtitle, source, custom_caption,
        theme_and_color_set, custom_theme_and_color_set
      )
    )
  } else {
    return(
      create_choropleth_map_germany_static(
        df, {{ var }},
        level,
        plot_title, plot_subtitle, source, custom_caption,
        theme_and_color_set, custom_theme_and_color_set
      )
    )
  }
}

#' Used as internal function.
#' @noRd

create_choropleth_map_germany_static <- function(df, var,
                                                 level = "Bundesland",
                                                 plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
                                                 theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL) {

  # browser()

  selected_theme <- manage_custome_theme(theme_and_color_set, custom_theme_and_color_set)

  if (level == "Bundesland") {
    germany <-
      st_as_sf(left_join(
        tibble(germany_choropleth_federal_states),
        df,
        by = c("NAME_1" = "Region")
      ))
  } else if (level == "Landkreis") {
    germany <-
      st_as_sf(left_join(
        tibble(germany_choropleth_regional_districts),
        df,
        by = c("NAME_2" = "Region")
      ))
  } else {
    stop("Level must be 'Bundesland' or 'Landkreis'. In Future other Levels will be served")
  }

  var_string <- as.character(ensym(var))
  var_type <- class(germany[[var_string]])[1]

  p <- ggplot(germany) +
    geom_sf(aes(fill = {{ var }} )) +
    theme_pubr() +
    theme(
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      text = element_text(color = selected_theme[["theme_info"]][["bold_color"]], family = selected_theme[["theme_info"]][["text_family"]]),
      legend.position = "right",
      plot.title = element_text(face = "bold"),
      plot.title.position = "plot",
      plot.subtitle = element_text(face = "italic"),
      plot.caption = element_text(vjust = 1),
      plot.caption.position =  "plot"
    ) +
    labs(
      fill = "",
      title = transform_title_letter_case_by_theme(plot_title, selected_theme),
      subtitle = paste(plot_subtitle, "\n\n"),
      caption = ifelse(is.na(custom_caption),
                       paste0("Quelle: ", source, "."),
                       custom_caption)
    )

  if (var_type %in% c("numeric", "integer")) {
    p <-
      p + scale_fill_gradient2(low = selected_theme$color[1], high = selected_theme$color[2])
  } else if (var_type %in% c("factor", "character")) {
    p <- p + scale_fill_manual(values = selected_theme$color)
  }

  return(p)

}

#' Used as internal function.
#' @noRd

create_choropleth_map_germany_interactive <- function(df, var,
                                                      level = "Bundesland",
                                                      plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
                                                      theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL) {
  # browser()

  selected_theme <- manage_custome_theme(theme_and_color_set, custom_theme_and_color_set)

  var_enquoded <- enquo(var)
  var_string <- str_remove(deparse(substitute(var_enquoded)), "~")
  var_type <- class(df[[var_string]])[1]

  if (level == "Bundesland") {
    df <- df %>%
      mutate(
        var = df[[var_string]],
        hovertext = paste("Region: ", df$Region, "<br>",
                          "Wert: ", df[[var_string]]),
        fill = get_map_colors(df[[var_string]], selected_theme)
      )
    germany <-
      st_as_sf(left_join(
        tibble(germany_choropleth_federal_states),
        df,
        by = c("NAME_1" = "Region")
      )) %>%
      st_simplify() %>%
      mutate(loc = NAME_1)

  } else if (level == "Landkreis") {
    df <- df %>%
      mutate(
        var = df[[var_string]],
        hovertext = paste("Region: ", df$Region, "<br>",
                          "Wert: ", df[[var_string]]),
        fill = get_map_colors(df[[var_string]], selected_theme)
      )
    germany <-
      st_as_sf(left_join(
        tibble(germany_choropleth_regional_districts),
        df,
        by = c("NAME_2" = "Region")
      )) %>%
      st_simplify() %>%
      mutate(loc = NAME_2)

  }

  suppressMessages({ # suppresses message that you should choose a diagramm type, but not needed/recommended when using add_sf
    germany %>% # Don't use in plot_ly as direct data-input. It doesn't work, don't know why.
      plot_ly(
        hoverinfo = "text",
        hoveron = "fills",
        text =  ~ hovertext
      ) %>%
      add_sf(
        split =  ~ loc,
        # hover text seems determined by split variable
        color =  ~ var,
        colors = ~ fill,
        alpha = 1,
        stroke = "white"
      ) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        hovermode = "closest",
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
            size = 20,
            color = selected_theme[["theme_info"]][["bold_color"]],
            family = selected_theme[["theme_info"]][["text_family"]]
          )
        ),
        margin = list(t = 80, b = 70), # more space for title/subtitle and caption,
        annotations = list(
          list(
            x = 1,
            y = 0,
            xref = "paper",
            yref = "paper",
            text = ifelse(
              is.na(custom_caption),
              paste0("Quelle: ", source, "."),
              custom_caption
            ),
            showarrow = FALSE,
            font = list(
              size = 9,
              color = selected_theme[["theme_info"]][["bold_color"]],
              family = selected_theme[["theme_info"]][["text_family"]]
            )
          )
        )
      ) %>%
      colorbar(
        title = list(
          text = var_string,
          font = list(
            size = 14,
            color = selected_theme[["theme_info"]][["bold_color"]],
            family = selected_theme[["theme_info"]][["text_family"]]
          )
        ),
        tickfont =  list(
          size = 12,
          color = selected_theme[["theme_info"]][["bold_color"]],
          family = selected_theme[["theme_info"]][["text_family"]]
        ),
        x = 1,
        y = 0.75,
        xref = "paper",
        yref = "paper",
        orientation = "v"
      ) %>%
      hide_legend()
  })
}
