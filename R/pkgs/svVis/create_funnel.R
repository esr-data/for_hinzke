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
  dplyr[select, pull, summarise, mutate, case_when, arrange] ,
  glue[glue],
  rlang[enquo, quo_get_expr, as_name],
  highcharter[hchart, hcaes, hc_add_theme, hc_plotOptions, hc_xAxis, hc_yAxis,
              hc_title, hc_subtitle, hc_credits, hc_colors,
              hc_annotations, hc_legend, hc_tooltip, hc_exporting, hc_chart, hc_caption, hc_add_series],
  plotly[plot_ly, config, layout]

)

create_funnel <- function(df, x_var, y_var,
                          plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
                          theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL,
                          print_data_labels = TRUE,
                          plot_type = "static") {

  if (plot_type == "plotly") {
    return(
      create_funnel_interactive_plotly(
        df, x_var, y_var,
        plot_title, plot_subtitle, source, custom_caption,
        theme_and_color_set, custom_theme_and_color_set,
        print_data_labels
      )
    )
  } else if (plot_type == "highcharter") {
    return(
      create_funnel_interactive_highcharter(
        df, x_var, y_var,
        plot_title, plot_subtitle, source, custom_caption,
        theme_and_color_set, custom_theme_and_color_set,
        print_data_labels
      )
    )
  } else {
    return(
      create_funnel_static(
        df,
        plot_title, plot_subtitle, source, custom_caption,
        theme_and_color_set, custom_theme_and_color_set
      )
    )
  }
}

# Function to create static ggplot
create_funnel_static <- function(df,
                                 plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
                                 theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL) {

  selected_theme <- manage_custome_theme(theme_and_color_set, custom_theme_and_color_set)

  # Prepare Data Function
  prepare_data <- function(df) {
    df <- df %>%
      mutate(x_var = factor(x_var, levels = x_var),
             proportion = y_var / max(y_var)) %>%
      arrange(desc(x_var)) %>%
      mutate(ymin = seq(0, 1 - 1 / nrow(df), length.out = nrow(df)),
             ymax = seq(1 / nrow(df), 1, length.out = nrow(df))) %>%
      arrange(x_var)

    return(df)
  }

  df <- prepare_data(df)

  # Create trapezoids
  create_trapezoid <- function(xmin, xmax, ymin, ymax, next_xmin, next_xmax) {
    data.frame(
      x = c(xmin, xmax, next_xmax, next_xmin),
      y = c(ymin, ymin, ymax, ymax)
    )
  }

  trapezoids <- list()
  for (i in 1:(nrow(df) - 1)) {
    trapezoids[[i]] <- create_trapezoid(
      -df$proportion[i + 1] / 2, df$proportion[i + 1] / 2,
      df$ymin[i], df$ymax[i],
      -df$proportion[i] / 2, df$proportion[i] / 2
    )
  }

  trapezoids[[nrow(df)]] <- create_trapezoid(
    0, 0,
    df$ymin[nrow(df)], df$ymax[nrow(df)],
    -df$proportion[nrow(df)] / 2, df$proportion[nrow(df)] / 2
  )

  plot_data <- do.call(rbind, trapezoids)
  plot_data$x_var <- rep(df$x_var, each = 4)

  plot_data$index <- as.numeric(as.factor(plot_data$x_var))
  plot_data$order <- rep(1:4, times = nrow(df))

  # Amend second last category
  categories <- unique(plot_data$x_var)
  target_data <- plot_data[plot_data$x_var == categories[length(categories) - 1], ]
  sorted_data <- target_data[order(target_data$y), ]

  new_y_low <- mean(sorted_data[c(1, 3), 'y'])
  new_x_low <- mean(sorted_data[c(1, 4), 'x'])
  new_x_high <- mean(sorted_data[c(2, 3), 'x'])

  sorted_data[c(1, 2), c('x', 'y')] <- rbind(c(new_x_low, new_y_low), c(new_x_high, new_y_low))

  plot_data[plot_data$x_var == categories[length(categories) - 1], ] <- sorted_data

  triangle_data <- data.frame(
    x = c(0, sorted_data[1, 'x'], sorted_data[2, 'x']),
    y = c(min(target_data$y), sorted_data[1, 'y'], sorted_data[2, 'y']),
    x_var = rep(categories[length(categories) - 1], 3)
  )

  num_categories <- length(unique(df$x_var)) - 1
  triangle_data$index <- num_categories

  triangle_data$order <- c(1, 0, 2)

  plot_data <- plot_data[!(plot_data$x_var == categories[length(categories) - 1] & plot_data$order %in% c(1, 2)), ]
  plot_data <- rbind(plot_data, triangle_data)
  plot_data <- plot_data %>% arrange(index, order)
  max_index_value <- max(plot_data$index)
  plot_data <- plot_data[plot_data$index < max_index_value, ]

  last_x_var <- df$x_var[nrow(df)]

  # ggplot
  p <- ggplot() +
    geom_polygon(data = plot_data, aes(x = x, y = y, fill = x_var)) +
    geom_text(data = subset(df, x_var != last_x_var), aes(x = 0, y = ymax, label = y_var), size = 3) +
    geom_text(data = subset(df, x_var == last_x_var), aes(x = 0, y = (ymin + ymax) / 2, label = y_var), size = 3) +
    geom_text(data = subset(df, x_var != last_x_var), aes(x = -1, y = ymax, label = x_var), size = 3, hjust = 0) +
    geom_text(data = subset(df, x_var == last_x_var), aes(x = 0, y = ymin, label = x_var), size = 3) +
    theme_void() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(
      title = transform_title_letter_case_by_theme(plot_title, selected_theme),
      subtitle = str_c(stri_wrap(plot_subtitle, width = 160), collapse = "\n"),
      caption = if (is.na(custom_caption)) str_c(stri_wrap(str_c("Quelle: ", source), collapse = "\n")) else custom_caption
    ) +
    theme(legend.position = "none")

  return(p)
}

create_funnel_interactive_plotly <- function(df, x_var, y_var,
                                             plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
                                             theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL,
                                             print_data_labels = TRUE) {
  selected_theme <- manage_custome_theme(theme_and_color_set, custom_theme_and_color_set)

  # Prepare Data Function
  prepare_data <- function(df) {
    df <- df %>%
      mutate(x_var = factor(x_var, levels = x_var),
             proportion = y_var / max(y_var)) %>%
      arrange(desc(x_var)) %>%
      mutate(ymin = seq(0, 1 - 1 / nrow(df), length.out = nrow(df)),
             ymax = seq(1 / nrow(df), 1, length.out = nrow(df))) %>%
      arrange(x_var)

    return(df)
  }

  df <- prepare_data(df)

  # Create trapezoids
  create_trapezoid <- function(xmin, xmax, ymin, ymax, next_xmin, next_xmax) {
    data.frame(
      x = c(xmin, xmax, next_xmax, next_xmin),
      y = c(ymin, ymin, ymax, ymax)
    )
  }

  trapezoids <- list()
  for (i in 1:(nrow(df) - 1)) {
    trapezoids[[i]] <- create_trapezoid(
      -df$proportion[i + 1] / 2, df$proportion[i + 1] / 2,
      df$ymin[i], df$ymax[i],
      -df$proportion[i] / 2, df$proportion[i] / 2
    )
  }

  trapezoids[[nrow(df)]] <- create_trapezoid(
    0, 0,
    df$ymin[nrow(df)], df$ymax[nrow(df)],
    -df$proportion[nrow(df)] / 2, df$proportion[nrow(df)] / 2
  )

  plot_data <- do.call(rbind, trapezoids)
  plot_data$x_var <- rep(df$x_var, each = 4)

  plot_data$index <- as.numeric(as.factor(plot_data$x_var))
  plot_data$order <- rep(1:4, times = nrow(df))

  # Amend second last category
  categories <- unique(plot_data$x_var)
  target_data <- plot_data[plot_data$x_var == categories[length(categories) - 1], ]
  sorted_data <- target_data[order(target_data$y), ]

  new_y_low <- mean(sorted_data[c(1, 3), 'y'])
  new_x_low <- mean(sorted_data[c(1, 4), 'x'])
  new_x_high <- mean(sorted_data[c(2, 3), 'x'])

  sorted_data[c(1, 2), c('x', 'y')] <- rbind(c(new_x_low, new_y_low), c(new_x_high, new_y_low))

  plot_data[plot_data$x_var == categories[length(categories) - 1], ] <- sorted_data

  triangle_data <- data.frame(
    x = c(0, sorted_data[1, 'x'], sorted_data[2, 'x']),
    y = c(min(target_data$y), sorted_data[1, 'y'], sorted_data[2, 'y']),
    x_var = rep(categories[length(categories) - 1], 3)
  )

  num_categories <- length(unique(df$x_var)) - 1
  triangle_data$index <- num_categories

  triangle_data$order <- c(1, 0, 2)

  plot_data <- plot_data[!(plot_data$x_var == categories[length(categories) - 1] & plot_data$order %in% c(1, 2)), ]
  plot_data <- rbind(plot_data, triangle_data)
  plot_data <- plot_data %>% arrange(index, order)
  max_index_value <- max(plot_data$index)
  plot_data <- plot_data[plot_data$index < max_index_value, ]

  last_x_var <- df$x_var[nrow(df)]

  # Create Plotly plot
  p <- plot_ly() %>%
    add_polygons(data = plot_data, x = ~x, y = ~y, fill = ~x_var, split = ~x_var) %>%
    add_annotations(data = subset(df, x_var != last_x_var), x = 0, y = ~ymax, text = ~y_var, showarrow = FALSE, font = list(size = 12)) %>%
    add_annotations(data = subset(df, x_var == last_x_var), x = 0, y = ~((ymin + ymax) / 2), text = ~y_var, showarrow = FALSE, font = list(size = 12)) %>%
    add_annotations(data = subset(df, x_var != last_x_var), x = -1, y = ~ymax, text = ~x_var, showarrow = FALSE, font = list(size = 12), xanchor = "left") %>%
    add_annotations(data = subset(df, x_var == last_x_var), x = 0, y = ~ymin, text = ~x_var, showarrow = FALSE, font = list(size = 12)) %>%
    layout( title = list(text = paste0("<b>", transform_title_letter_case_by_theme(plot_title, selected_theme), "</b>", "<br>", "<sub><i>", plot_subtitle, "</sub>"),
                         xref = "paper",
                         x = 0,
                         font = list(size = 16, color = selected_theme[["theme_info"]][["bold_color"]], family = selected_theme[["theme_info"]][["text_family"]])),
            xaxis = list(
              showline = FALSE,
              showticklabels = FALSE,
              zeroline= FALSE,
              title = ""
            ),
            yaxis = list(
              showline = FALSE,
              showticklabels = FALSE,
              zeroline= FALSE,
              title = ""
            ),
            margin = list(t = 80, b = 70), # more space for title/subtitle and caption,
            hoverlabel = list(
              font = list(color = "white", family = selected_theme[["theme_info"]][["text_family"]])
            ),
            showlegend = FALSE
    )

  return(p)
}

create_funnel_interactive_highcharter <- function(df, x_var, y_var,
                                                  plot_title = "", plot_subtitle = "", source = "", custom_caption = NA_character_,
                                                  theme_and_color_set = "sv0_allmain", custom_theme_and_color_set = NULL,
                                                  print_data_labels = TRUE) {
  selected_theme <- manage_custome_theme(theme_and_color_set, custom_theme_and_color_set)

  # Prepare Data Function
  prepare_data <- function(df) {
    df <- df %>%
      mutate(x_var = factor(x_var, levels = x_var),
             proportion = y_var / max(y_var)) %>%
      arrange(desc(x_var)) %>%
      mutate(ymin = seq(0, 1 - 1 / nrow(df), length.out = nrow(df)),
             ymax = seq(1 / nrow(df), 1, length.out = nrow(df))) %>%
      arrange(x_var)

    return(df)
  }

  df <- prepare_data(df)

  # Create trapezoids
  create_trapezoid <- function(xmin, xmax, ymin, ymax, next_xmin, next_xmax) {
    data.frame(
      x = c(xmin, xmax, next_xmax, next_xmin),
      y = c(ymin, ymin, ymax, ymax)
    )
  }

  trapezoids <- list()
  for (i in 1:(nrow(df) - 1)) {
    trapezoids[[i]] <- create_trapezoid(
      -df$proportion[i + 1] / 2, df$proportion[i + 1] / 2,
      df$ymin[i], df$ymax[i],
      -df$proportion[i] / 2, df$proportion[i] / 2
    )
  }

  trapezoids[[nrow(df)]] <- create_trapezoid(
    0, 0,
    df$ymin[nrow(df)], df$ymax[nrow(df)],
    -df$proportion[nrow(df)] / 2, df$proportion[nrow(df)] / 2
  )

  plot_data <- do.call(rbind, trapezoids)
  plot_data$x_var <- rep(df$x_var, each = 4)

  plot_data$index <- as.numeric(as.factor(plot_data$x_var))
  plot_data$order <- rep(1:4, times = nrow(df))

  # Amend second last category
  categories <- unique(plot_data$x_var)
  target_data <- plot_data[plot_data$x_var == categories[length(categories) - 1], ]
  sorted_data <- target_data[order(target_data$y), ]

  new_y_low <- mean(sorted_data[c(1, 3), 'y'])
  new_x_low <- mean(sorted_data[c(1, 4), 'x'])
  new_x_high <- mean(sorted_data[c(2, 3), 'x'])

  sorted_data[c(1, 2), c('x', 'y')] <- rbind(c(new_x_low, new_y_low), c(new_x_high, new_y_low))

  plot_data[plot_data$x_var == categories[length(categories) - 1], ] <- sorted_data

  triangle_data <- data.frame(
    x = c(0, sorted_data[1, 'x'], sorted_data[2, 'x']),
    y = c(min(target_data$y), sorted_data[1, 'y'], sorted_data[2, 'y']),
    x_var = rep(categories[length(categories) - 1], 3)
  )

  num_categories <- length(unique(df$x_var)) - 1
  triangle_data$index <- num_categories

  triangle_data$order <- c(1, 0, 2)

  plot_data <- plot_data[!(plot_data$x_var == categories[length(categories) - 1] & plot_data$order %in% c(1, 2)), ]
  plot_data <- rbind(plot_data, triangle_data)
  plot_data <- plot_data %>% arrange(index, order)
  max_index_value <- max(plot_data$index)
  plot_data <- plot_data[plot_data$index < max_index_value, ]

  last_x_var <- df$x_var[nrow(df)]

  # Create Highcharter plot
  hc <- hchart(plot_data, type = "polygon", hcaes(x = x, y = y, group = x_var)) %>%
    hc_add_series(data = subset(df, !(x_var %in% c('Serie 6', 'Serie 7', 'Serie 9'))),
                  type = "scatter", hcaes(x = -1, y = ymax, name = x_var),
                  marker = list(enabled = FALSE)) %>%
    hc_add_series(data = subset(df, x_var == last_x_var),
                  type = "scatter", hcaes(x = 0, y = ymin, name = x_var),
                  marker = list(enabled = FALSE)) %>%
    hc_title(text = plot_title) %>%
    hc_subtitle(text = plot_subtitle) %>%
    hc_caption(text = custom_caption) %>%
    hc_legend(enabled = FALSE) %>%
    hc_chart(zoomType = "xy") %>%
    hc_exporting(enabled = TRUE) %>%
    hc_yAxis(visible = FALSE) %>%
    hc_xAxis(visible = FALSE) %>%
    hc_tooltip(pointFormat = '{point.x_var}: {point.y_var}')

  return(hc)
}
