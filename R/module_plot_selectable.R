
# alternative -------------------------------------------------------------


#------------------------------------------------------------------------------#
# MODULE UI ----
#' UI Module: plotly plot
#'
#' @param id Character string
#'
module_ui_plot_selectable <- function(id) {
    ns <- shiny::NS(id)


    # shiny::fluidRow(column(9,
    # shiny::textOutput(ns('df_descriptor')),
    # shiny::uiOutput(ns('scatterselectControl')),
    plotly::plotlyOutput(ns('scatterselect'))
    # )



    # DT::DTOutput(ns('grouptable')),
    # shiny::textOutput(ns('selected_row'))
    # )

}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

#' Server Module: box for str filter condition
#'
#' Server Module: box for str filter condition
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param df reactive df, with df as element
#' @param group_row numeric, selected group_index
#' @param df reactive df, with df as element
#' @param selector_inputs reactive, output from module_plot_selectorcontrols
#'
#' @details provides UI text box element
#' @details provides UI text box element
module_server_plot_selectable <- function(input, output, session, df, group_row, selector_inputs){
    ns = session$ns

    # print(paste("sellll rooo is:", group_row$group_row))
    print(paste("sellll rooo is:", input[[ns("dtgrouprow-grouptable_rows_selected")]]))


    tmp_data <- df$df$data

    if(length(group_row$group_row)!=0){
        # group_index <- input$grouptable_rows_selected
        group_index <- group_row$group_row
        # group_index <- input[["dtgrouprow-grouptable_rows_selected"]]


    } else {
        group_index <- unique(dplyr::group_indices(tmp_data))
    }

    # adjust colors
    cols <-  data.frame(.index = unique(group_indices(tmp_data)),
                        .color = extend_palette(
                            length(
                                unique(
                                    group_indices(tmp_data)
                                ) # / unique
                            ) # / length
                        ),
                        stringsAsFactors = FALSE) # palette



    coldf <- merge(data.frame(.index = as.factor(dplyr::group_indices(tmp_data))),
                   cols)

    tmp_data <- dplyr::bind_cols(tmp_data, coldf)

    print(head(tmp_data))


    if(!dplyr::is.grouped_df(tmp_data)){

        plot_data <- tmp_data
    } else {



        # gr_rows <- df$df$data %>%
        #     dplyr::group_data() %>%
        #     slice(group_index) %>%
        #     pull(.rows) %>%
        #     unlist()

        # plot_data <- df$df$data[gr_rows, ]
        # plot_data <- tmp_data[dplyr::group_indices(tmp_data) %in% group_index, ]
        plot_data <- tmp_data[tmp_data$.index %in% group_index, ]
    }



    # if(!is.null(df$df$data)){

    #
    #     output$scatterselectControl <- shiny::renderUI({
    #         shiny::fluidRow(
    #             column(4, shiny::varSelectInput(ns('xvar'),
    #                                             label = "X Var",
    #                                             data = plot_data)),
    #             column(4,
    #                    shiny::varSelectInput(ns('yvar'),
    #                                          label = "Y Var",
    #                                          data = plot_data)),
    #             column(1,
    #                    shiny::br(),
    #                    shiny::actionButton(ns('startscatter'),
    #                                        label = "Plot!"))
    #         )
    #
    #     })




    # prepare named value-vector for ggplot scale
    col_value_vector <- extend_palette(
        length(
            unique(
                group_indices(tmp_data)
            ) # / unique
        ) # / length
    )
    names(col_value_vector) <- unique(group_indices(tmp_data))

    # print(col_value_vector)
    # print(dplyr::group_indices(tmp_data))
    # print(dplyr::group_indices(plot_data))
    # print(coldf)
    print(plot_data, n = 150)



    # handle "Plot!" click
    shiny::observeEvent(selector_inputs$abutton, {
        output$scatterselect <- plotly::renderPlotly({


            rlang::eval_tidy(
                rlang::quo_squash(
                    rlang::quo({

                        # plotly::plot_ly(data = plot_data,
                        #                 x = ~ !!selector_inputs$xvar,
                        #                 y = ~ !!selector_inputs$yvar,
                        #                 color = ~as.factor(.index),
                        #                 colors = col_value_vector,
                        #                 type = 'scatter',
                        #                 mode = 'markers',
                        #                 showlegend = TRUE)

                        plotly::plot_ly(data = plot_data) %>%
                            plotly::add_markers(x = ~ !!selector_inputs$xvar,
                                                y = ~ !!selector_inputs$yvar,
                                                color = ~as.factor(.index),
                                                colors = col_value_vector,
                                                type = 'scatter',
                                                showlegend = TRUE) %>%
                            plotly::layout(showlegend = TRUE)


                        # plotly::plot_ly(data = plot_data,
                        #                 x = ~ !!selector_inputs$xvar,
                        #                 y = ~ !!selector_inputs$yvar,
                        #                 color = ~as.factor(.index),
                        #                 colors = col_value_vector,
                        #                 type = 'scatter',
                        #                 mode = 'markers',
                        #                 showlegend = TRUE)



                    })
                )
            )





            # plotly::ggplotly(
            #     ggplot2::ggplot(data = plot_data,
            #                     ggplot2::aes(x = !!selector_inputs$xvar,
            #                                  y = !!selector_inputs$yvar,
            #                                  color = .index)) +
            #         ggplot2::geom_point(show.legend = TRUE) +
            #         ggplot2::geom_point(color = plot_data$.color, show.legend = FALSE) +
            #         # ggplot2::theme(legend.direction="horizontal") +
            #         ggplot2::labs(color = "Groupings") +
            #         ggplot2::scale_color_manual(values = col_value_vector)
            #          # scale
            #
            # )

        })
    })


    # }
}
