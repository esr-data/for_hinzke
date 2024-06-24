box::use(
  ../../R/pkgs/svVis/create_bar[create_bar],
  ../../R/pkgs/svVis/create_bar_grouped[create_bar_grouped],
  ../../R/pkgs/svVis/create_lineplot[create_lineplot],
  ../../R/pkgs/svVis/create_choropleth_map_germany[create_choropleth_map_germany],

  stringi[stri_split],
  readxl[read_excel]
)

get_chart_options <-
  function(df, chart_options_rules_dir = "data/chart_options_rules.xlsx"){ #browser()

    chart_options_rules <- read_excel(chart_options_rules_dir)

    stopifnot("Dataframe has only one row. Change data selection to include some sort of variation to receive chart options." = nrow(df) != 1)

    time <- names(df)[grepl("Zeit", names(df))]
    variable <- "Variable/n"
    value <- names(df)[grepl("Wert", names(df))]
    regional <- "Bundesland" # TODO: welche regionalen Einheiten sind auch relevant für maps?
    other <- names(df)[!names(df) %in% c(time, variable, value, regional)]

    variation_check <-
      sapply(df, function(column) length(unique(column)) > 1)

    variation_time <- variation_check[time]
    variation_variable <- variation_check[variable]
    variation_else <- any(variation_check[other])

    all_federal_states <-
      ifelse(
        !is.element(regional, names(df)),
        FALSE,
        ifelse(
          length(unique(df[[regional]])) == 16,
          TRUE,
          FALSE
        )
      )

    result_fct <-
      stri_split(
        chart_options_rules$result_fct[
          chart_options_rules$variation_time == variation_time &
            chart_options_rules$variation_variable == variation_variable &
            chart_options_rules$variation_else == variation_else &
            chart_options_rules$all_federal_states == all_federal_states],
        fixed = ", "
      )[[1]]

    result_message <-
      chart_options_rules$result_text[
        chart_options_rules$variation_time == variation_time &
          chart_options_rules$variation_variable == variation_variable &
          chart_options_rules$variation_else == variation_else &
          chart_options_rules$all_federal_states == all_federal_states]

    if (all(is.na(result_fct))){

      message(
        paste0(
          "Sorry, no chart can be suggested - ", result_message
        )
      )

    } else {

      message(
        paste0(
          "The following charts can be suggested for your data selection: ",
          result_message,
          ". You can use the following svvis-function(s): ",
          paste(result_fct, collapse = ", "),
          "."
        )
      )

    }

    return(
      list(
        result_fct = result_fct,
        time = time,
        variable = variable,
        value = value,
        regional = regional,
        other = other,
        variation_check = variation_check,
        variation_time = variation_time,
        variation_variable = variation_variable,
        variation_else = variation_else,
        all_federal_states = all_federal_states
      )
    )


  }

# --- retrieve parameters from df for a given chart option ---------------------

# for starters covers lineplot + barplots (simple + staged) only
get_plot_params_from_df <-
  function(df,
           plot_fct,
           time,
           variable,
           value,
           regional,
           other,
           variation_check,
           variation_time,
           variation_variable,
           variation_else,
           all_federal_states){ #browser()

    available_plot_fcts <- c("create_lineplot", "create_bar", "create_bar_grouped", "create_choropleth_map_germany")
    stopifnot("The inserted plot function is not (yet) available" = plot_fct %in% available_plot_fcts)

    function_to_use <- list(
      create_lineplot = get_lineplot_params_from_df,
      create_bar = get_barplot_params_from_df,
      create_bar_grouped = get_barplot_params_from_df,
      create_choropleth_map_germany = get_choropleth_params_from_df # TODO: muss vermutlich noch besser benamst werden, wenn weitere Map-Sorten berücksichtigt sind
    )

    result <-
      function_to_use[[plot_fct]]( # noch eher unhübsch TODO: flexibler gestalten!
        df = df,
        plot_fct = plot_fct,
        time = time,
        variable = variable,
        value = value,
        regional = regional,
        other = other,
        variation_check = variation_check,
        variation_time = variation_time,
        variation_variable = variation_variable,
        variation_else = variation_else,
        all_federal_states = all_federal_states
      )

    return(result)

  }

## --- help fct for renaming df cols -------------------------------------------
rename_result_df <- function(result, rename_group_var = FALSE, group_var_name = NULL) { #browser()

  names(result$df)[names(result$df) == result$x_var] <- "x_var"
  names(result$df)[names(result$df) == result$y_var] <- "y_var"
  names(result$df)[names(result$df) == result$var] <- "var"
  names(result$df)[names(result$df) == result$Region] <- "Region"

  if (rename_group_var) {
    names(result$df)[names(result$df) %in% c(result$group_var, result$group_color)] <- group_var_name
  }

  return(result)
}


## --- subfct for lineplots ----------------------------------------------------

get_lineplot_params_from_df <-
  function(df,
           plot_fct, # TODO: wozu der parameter? Raus?!
           time,
           variable,
           value,
           regional,
           other,
           variation_check,
           variation_time,
           variation_variable,
           variation_else,
           all_federal_states){ #browser()


    if (variation_variable == FALSE & variation_else == FALSE & all_federal_states == FALSE){

      result <- list(
        df = df,
        x_var = time,
        y_var = value,
        plot_title = paste0(unique(df[[variable]]), " im Laufe der Zeit")
      )

      result <- rename_result_df(result)

    } else if (variation_variable == TRUE & variation_else == FALSE & all_federal_states == FALSE){

      result <- list(
        df = df,
        x_var = time,
        y_var = value,
        group_color = variable,
        plot_title = paste0(paste(unique(df[[variable]]), collapse = " und "), " im Laufe der Zeit")
      )

      result <- rename_result_df(result, rename_group_var = TRUE, group_var_name = "group_color")

    } else if (variation_variable == FALSE & variation_else == TRUE & all_federal_states == FALSE){

      grouping_variable <- names(variation_check[variation_check])
      grouping_variable <- grouping_variable[!grouping_variable %in% c(time, variable, value, regional)]

      result <- list(
        df = df,
        x_var = time,
        y_var = value,
        group_color = grouping_variable,
        plot_title = paste0(unique(df[[variable]]), " im Laufe der Zeit für ", paste(df[grouping_variable], collapse = " und "))
      )

      result <- rename_result_df(result, rename_group_var = TRUE, group_var_name = "group_color")

    } else if (variation_variable == FALSE & variation_else == FALSE & all_federal_states == TRUE){

      result <- list(
        df = df,
        x_var = time,
        y_var = value,
        group_color = regional,
        plot_title = paste0(unique(df[[variable]]), " im Laufe der Zeit nach ", regional)
      )

      result <- rename_result_df(result, rename_group_var = TRUE, group_var_name = "group_color")

    }

    return(result)

  }


## --- subfct for barplots ----------------------------------------------------

get_barplot_params_from_df <-
  function(df,
           plot_fct, # TODO: parameter kann raus???
           time,
           variable,
           value,
           regional,
           other,
           variation_check,
           variation_time,
           variation_variable,
           variation_else,
           all_federal_states){ #browser()

    if (variation_time == TRUE){

      if (variation_variable == FALSE & variation_else == FALSE & all_federal_states == FALSE){

        result <- list(
          df = df,
          x_var = time,
          y_var = value,
          plot_title = paste0(unique(df[[variable]]), " im Laufe der Zeit")
        )

        result <- rename_result_df(result)

      } else if (variation_variable == TRUE & variation_else == FALSE & all_federal_states == FALSE){

        result <- list(
          df = df,
          x_var = time,
          y_var = value,
          group_var = variable,
          plot_title = paste0(paste(unique(df[[variable]]), collapse = " und "), " im Laufe der Zeit")
        )

        result <- rename_result_df(result, rename_group_var= TRUE, group_var_name = "group_var")


      } else if (variation_variable == FALSE & variation_else == TRUE & all_federal_states == FALSE){

        grouping_variable <- names(variation_check[variation_check])
        grouping_variable <- grouping_variable[!grouping_variable %in% c(time, variable, value, regional)]

        result <- list(
          df = df,
          x_var = time,
          y_var = value,
          group_var = grouping_variable,
          plot_title = paste0(unique(df[[variable]]), " im Laufe der Zeit für ", paste(df[grouping_variable], collapse = " und "))
        )

        result <- rename_result_df(result, rename_group_var= TRUE, group_var_name = "group_var")

      } else if (variation_variable == FALSE & variation_else == FALSE & all_federal_states == TRUE){

        result <- list(
          df = df,
          x_var = time,
          y_var = value,
          group_var = regional,
          plot_title = paste0(unique(df[[variable]]), " im Laufe der Zeit nach ", regional)
        )

        result <- rename_result_df(result, rename_group_var= TRUE, group_var_name = "group_var")

      }

    } else {

      if (variation_variable == TRUE & variation_else == FALSE & all_federal_states == FALSE){

        result <- list(
          df = df,
          x_var = variable,
          y_var = value,
          plot_title = paste0(paste(unique(df[[variable]]), collapse = " und "), " ", unique(df[[time]]))
        )

        result <- rename_result_df(result)


      } else if (variation_variable == FALSE & variation_else == TRUE & all_federal_states == FALSE){

        variation_variable <- names(variation_check[variation_check])
        variation_variable <- variation_variable[!variation_variable %in% c(time, variable, value, regional)]

        result <- list(
          df = df,
          x_var = variation_variable,
          y_var = value,
          plot_title = paste0(unique(df[[variable]]), ": ", paste(df[variation_variable], collapse = " und "))
        )

        result <- rename_result_df(result)


      } else if (variation_variable == TRUE & variation_else == TRUE & all_federal_states == FALSE){

        grouping_variable <- names(variation_check[variation_check])
        grouping_variable <- grouping_variable[!grouping_variable %in% c(time, variable, value, regional)]

        result <- list(
          df = df,
          x_var = variable,
          y_var = value,
          group_var = grouping_variable,
          plot_title = paste0(paste(unique(df[[variable]]), collapse = " und "), " nach ", grouping_variable)
        )

        result <- rename_result_df(result, rename_group_var= TRUE, group_var_name = "group_var")

      } else if (variation_variable == FALSE & variation_else == TRUE & all_federal_states == TRUE){

        grouping_variable <- names(variation_check[variation_check])
        grouping_variable <- grouping_variable[!grouping_variable %in% c(time, variable, value, regional)]

        result <- list(
          df = df,
          x_var = regional,
          y_var = value,
          group_var = grouping_variable,
          plot_title = paste0(unique(df[[variable]]), "nach ", regional, " - differenziert nach: ", paste(df[grouping_variable], collapse = " und "))
        )

        result <- rename_result_df(result, rename_group_var= TRUE, group_var_name = "group_var")

      } else if (variation_variable == TRUE & variation_else == FALSE & all_federal_states == TRUE){

        result <- list(
          df = df,
          x_var = regional,
          y_var = value,
          group_var = variable,
          plot_title = paste0(paste(unique(df[[variable]]), collapse = " und "), "nach ", regional)
        )

        result <- rename_result_df(result, rename_group_var= TRUE, group_var_name = "group_var")

      }

    }

    return(result)

  }


## --- subfct for choropleth map germany ---------------------------------------

get_choropleth_params_from_df <- # TODO: Unterscheidung für metr. vs. kategorische Variablen muss noch ergänzt werden
  function(df,
           plot_fct = plot_fct,
           time = time,
           variable = variable,
           value = value,
           regional = regional,
           other = other,
           variation_check = variation_check,
           variation_time = variation_time,
           variation_variable = variation_variable,
           variation_else = variation_else,
           all_federal_states = all_federal_states){ # TODO: Lösung finden, um unnötige parameter nicht mit reinschreiben zu müssen! S.O.

    #browser()

    result <- list(
      df = df,
      Region = regional,
      var = value,
      plot_title = paste0(unique(df[[variable]]), " nach Bundesland")
    )

    result <- rename_result_df(result)

    return(result)

  }



# --- produce plot -------------------------------------------------------------
# TODO: erweitern zur Ausgabe mehrerer Plots/einer Liste an plots
produce_plot <-
  function(df, chart_options_rules_dir){ #browser()

    # get possible chart options for data input
    results <-
      get_chart_options(
        df,
        chart_options_rules_dir
      )

    result_plots <- list()

    for (i in 1:length(results$result_fct)) {

      # define plot params for determined plot options
      params <-
        get_plot_params_from_df(
          df = df,
          plot_fct = results$result_fct[i], # default: immer das erste in der Wahl - muss noch für mehrere angepasst werden
          time = results$time,
          variable = results$variable,
          value = results$value,
          regional = results$regional,
          other = results$other,
          variation_check = results$variation_check,
          variation_time = results$variation_time,
          variation_variable = results$variation_variable,
          variation_else = results$variation_else,
          all_federal_states = results$all_federal_states
        )

      # produce plot with previously determined input
      if (results$result_fct[i] == "create_lineplot") {

        if (!any(grepl("group_color", names(params$df)))){

          result_plots[[i]] <-
            create_lineplot(
              df = params$df,
              x_var = x_var,
              y_var = y_var,
              xlabel_text = params$x_var,
              ylabel_text = params$y_var,
              plot_title = params$plot_title,
              interactive = FALSE
            )

        } else {

          result_plots[[i]] <-
            create_lineplot(
              df = params$df,
              x_var = x_var,
              y_var = y_var,
              group_color = group_color,
              xlabel_text = params$x_var,
              ylabel_text = params$y_var,
              plot_title = params$plot_title,
              interactive = FALSE
            )

        }

      } else if (results$result_fct[i] == "create_bar"){

        result_plots[[i]] <-
          create_bar(
            df = params$df,
            x_var = x_var,
            y_var = y_var,
            xlabel_text = params$x_var,
            ylabel_text = params$y_var,
            plot_title = params$plot_title
          )

      } else if (results$result_fct[i] == "create_bar_grouped"){

        result_plots[[i]] <-
          create_bar_grouped(
            df = params$df,
            x_var = x_var,
            y_var = y_var,
            group_var = group_var,
            xlabel_text = params$x_var,
            ylabel_text = params$y_var,
            plot_title = params$plot_title
          )

      } else if (results$result_fct[i] == "create_choropleth_map_germany"){

        result_plots[[i]] <-
          create_choropleth_map_germany(
            df = params$df,
            var = var,
            plot_title = params$plot_title

          )

      }

    }

    return(result_plots)

  }

