
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



    if(length(group_row$group_row)!=0){
        # group_index <- input$grouptable_rows_selected
        group_index <- group_row$group_row
        # group_index <- input[["dtgrouprow-grouptable_rows_selected"]]


    } else {
        group_index <- unique(dplyr::group_indices(df$df$data))
    }

    if(!dplyr::is.grouped_df(df$df$data)){

        plot_data <- df$df$data
    } else {



        # gr_rows <- df$df$data %>%
        #     dplyr::group_data() %>%
        #     slice(group_index) %>%
        #     pull(.rows) %>%
        #     unlist()

        # plot_data <- df$df$data[gr_rows, ]
        plot_data <- df$df$data[dplyr::group_indices(df$df$data) %in% group_index, ]
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



    shiny::observeEvent(selector_inputs$abutton, {
        output$scatterselect <- plotly::renderPlotly({

            plotly::ggplotly(
                ggplot2::ggplot(data = plot_data,
                                ggplot2::aes(x = !!selector_inputs$xvar,
                                             y = !!selector_inputs$yvar,
                                             color = as.factor(dplyr::group_indices(plot_data)))) +
                    ggplot2::geom_point() +
                    ggplot2::theme(legend.direction="horizontal") +
                    ggplot2::labs(color = "Grouping index")
            )

        })
    })


    # }
}
