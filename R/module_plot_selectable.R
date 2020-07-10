
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
#' @param sel_points numeric, provides .dcrkey of selected points
#'
#' @details provides plot, note, that data set needs a column .dcrkey, added in initial processing step
module_server_plot_selectable <- function(input, output, session, df, group_row, selector_inputs, sel_points){
    ns = session$ns

    # print(paste("sellll rooo is:", group_row$group_row))
    # print(paste("sellll rooo is:", input[[ns("dtgrouprow-grouptable_rows_selected")]]))


    tmp_data <- df$df$data

    if(length(group_row$group_row)!=0){
        # group_index <- input$grouptable_rows_selected
        group_index <- group_row$group_row
        # group_index <- input[["dtgrouprow-grouptable_rows_selected"]]


    } else {
        group_index <- unique(dplyr::group_indices(tmp_data))
    }

    # adjust colors
    cols <-  data.frame(.index = unique(dplyr::group_indices(tmp_data)),
                        .color = extend_palette(
                            length(
                                unique(
                                    dplyr::group_indices(tmp_data)
                                ) # / unique
                            ) # / length
                        ),
                        stringsAsFactors = FALSE) # palette



    coldf <- base::merge(data.frame(.index = as.factor(dplyr::group_indices(tmp_data))),
                   cols)

    tmp_data <- dplyr::bind_cols(tmp_data, coldf)

    # print(head(tmp_data))


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




    opacity <- ifelse(plot_data$.dcrkey %in% sel_points, 0.25, 0.9)
    plot_data$.opacity <- opacity


    print("-------AGAIN-------")
    print(sel_points)


    # adjust selection
    # alpha <- ifelse(plot_data$.dcrkey %in% sel_points, 0.75, 0.35)

    # plot_data$.colrgba <- ifelse(col2plotlyrgba(plot_data$.color, alpha))
    # plot_data$.colrgbstroke <- col2plotlyrgb(plot_data$.color)

    # print("opacity is")
    # print(opacity)


    # add .key ref for plot
    # plot_data$.key <- seq_len(nrow(plot_data))

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
                dplyr::group_indices(tmp_data)
            ) # / unique
        ) # / length
    )
    names(col_value_vector) <- unique(dplyr::group_indices(tmp_data))

    # print(col_value_vector)
    # print(dplyr::group_indices(tmp_data))
    # print(dplyr::group_indices(plot_data))
    # print(coldf)
    # print(plot_data, n = 150)


    # plotted, true or false?



    # handle "Plot!" click
    shiny::observeEvent(selector_inputs$abutton, {
        output$scatterselect <- plotly::renderPlotly({


            # clicked <- shiny::reactiveValues(event_df = NULL)
            #
            # shiny::observe({
            #     clicked$event_df <- plotly::event_data("plotly_click", source = "scatterselect" ,
            #                                   priority = "event")
            #     })
            #
            #
            #




            p <-  rlang::eval_tidy(
                rlang::quo_squash(
                    rlang::quo({


                        print("redrawing")
                        plotly::plot_ly(data = plot_data,
                        # plotly::plot_ly(data = tidyr::drop_na(plot_data),
                        # plotly::plot_ly(data = na.omit(plot_data),
                                        source = "scatterselect"
                                        # marker = list(size = 7,
                                        #               line = list(color = col2plotlyrgba("gray60", 0.9),
                                        #               width = 1)
                                        # )
                        ) %>%
                            plotly::add_markers(x = ~ !!selector_inputs$xvar,
                                                y = ~ !!selector_inputs$yvar,
                                                # color = ~as.factor(.index),
                                                name = ~as.factor(.index),
                                                # colors = col_value_vector,
                                                type = 'scatter',
                                                customdata = ~.dcrkey,
                                                showlegend = TRUE,
                                                marker = list(color = sapply(plot_data$.color,
                                                                             col2plotlyrgba, 0.9,
                                                                             USE.NAMES = FALSE),
                                                              line = list(color = col2plotlyrgba("gray60", 0.9),
                                                                          width = 1)),
                                                unselected = list(marker = list(opacity = 0.9))) %>%
                                                # ,
                                                # unselected = list(marker = list(opacity = 0.9))) %>%
                                                # opacity = ~.opacity) %>%
                            plotly::layout(showlegend = TRUE,
                                           # dragmode =  FALSE
                                           dragmode = "lasso"
                            )  %>%
                            plotly::event_register(event = "plotly_doubleclick") %>%
                            plotly::event_register(event = "plotly_deselect") %>%
                            plotly::event_register(event = "plotly_relayout") %>%
                            plotly::event_register(event = "plotly_click") %>%
                            plotly::event_register(event = "plotly_selected")

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
            ) #\ eval_tidy




            plotly::plotly_build(p)





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


    shiny::observeEvent(sel_points,
                        {
        # if (length(sel_points) > 0) {
            # this is essentially the plotly.js way of doing
            # `p %>% add_lines(x = ~x, y = ~yhat) %>% toWebGL()`
            # without having to redraw the entire plot

            add_points <- plot_data[plot_data$.dcrkey %in% sel_points, ]

            print("adding these points")
            print(add_points)

            plotly::plotlyProxy("scatterselect", session) %>%
                plotly::plotlyProxyInvoke(
                    "addTraces",
                    # list(
                    #     x = add_points[ , selector_inputs$xvar],
                    #     y = add_points[ , selector_inputs$yvar],
                    #     type = "scatter",
                    #     mode = "marker",
                    #     marker = list(line = list(color = sapply(plot_data$.color,
                    #                                  col2plotlyrgba, 0.9,
                    #                                  USE.NAMES = FALSE),
                    #                               width = 2),
                    #                   color = col2plotlyrgba("white", 0.9),
                    #                               width = 1)
                    # )
                    list(
                        x = add_points[ , selector_inputs$xvar],
                        y = add_points[ , selector_inputs$yvar],
                        type = "scatter",
                        mode = "markers",
                        color = I("red"))
                        # line = list(color = sapply(plot_data$.color,
                                                                 # col2plotlyrgba, 0.9,
                                                                 # USE.NAMES = FALSE),
                                    # width = 2),
                        # color = col2plotlyrgba("white", 0.9)
                        # )

                )
        # }
    })



    # }
}
