#------------------------------------------------------------------------------#
# MODULE UI ----
#' UI Module: dynamic histogram output for n vars
#'
#' @param id Character string
#'
module_ui_histograms <- function(id) {
    ns <- shiny::NS(id)


    # shiny::tagList(shiny::h4(shiny::tags$strong("Impact on distribution")),
    #     plotly::plotlyOutput(ns("histogram"),
    #                                     height = "600px"))


    shiny::uiOutput(ns("histmodule"))

}


#------------------------------------------------------------------------------#
# MODULE SERVER ----

#' Server Module: dynamic histogram output for n vars
#' str filter condition
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param dframe reactive df
#' @param selector_inputs reactive vals from above-plot controls,
#' @param sel_points reactive, provides .dcrkey of selected points
#'
#' @details provides UI buttons for deleting last / entire outlier selection
#'
#' @return reactive values with input xvar, yvar and actionbutton counter
module_server_histograms  <-
    function(input,
             output,
             session,
             dframe,
             selector_inputs,
             sel_points) {
        ns = session$ns

        # plot generator
        one_plot <- function(var, dfull, dfilt) {
            # cols <- extend_palette(2)

            plotly::plot_ly(legendgroup = I("compare")) %>%
                plotly::add_histogram(
                    data = dfull,
                    x = as.formula(paste0("~", var)),
                    color = I("#31b0d5"),
                    name = I("Original")
                ) %>%
                plotly::add_histogram(
                    data = dfilt,
                    x = as.formula(paste0("~", var)),
                    color = I("#ec971f"),
                    name = I("Filtered")
                ) %>%
                # add_annotations(text = sprintf("<b>%s</b>", var),
                #                 y = 1.2,
                #                 x = 0.5,
                #                 xref = "paper",
                #                 yref = "paper",
                #                 showarrow = FALSE) %>%
                plotly::layout(
                    barmode = "overlay",
                    yaxis = list(title = "Count"),
                    showlegend = TRUE,
                    plot_bgcolor  = "rgba(0, 0, 0, 0)",
                    paper_bgcolor = "rgba(0, 0, 0, 0)"
                )
        }



        # grab all variables (not plotstart button)
        selector_inputs <- shiny::reactiveValuesToList(selector_inputs)

        all_vars <-
            selector_inputs[grepl("var$", names(selector_inputs))]
        all_vars <- vapply(all_vars, as.character, character(1))

        # check which vars are not numeric
        non_numeric_columns <-
            colnames(dframe())[!vapply(dframe(), is.numeric, logical(1))]

        # drop empty var name entries and grab only
        # those representing numeric columns
        vars_to_plot <- setdiff(drop_empty(all_vars),
                                non_numeric_columns)

        output$histogram <- plotly::renderPlotly({
            vars_to_plot %>%
                lapply(one_plot,
                       dfull = dframe(),
                       dfilt = dframe()[dframe()$.dcrkey %nin% sel_points$df$keys,]) %>%
                setNames(rep("histoplot", NROW(vars_to_plot))) %>%
                # arrange
                plotly::subplot(
                    nrows = NROW(vars_to_plot),
                    shareX = FALSE,
                    margin = 0.1,
                    titleX = TRUE,
                    titleY = TRUE,
                    which_layout = "merge"
                ) %>%
                plotly::config(displaylogo = FALSE,
                               modeBarButtonsToRemove = list("hoverCompareCartesian"))

        })

        output$histmodule <- shiny::renderUI({
            shiny::tagList(
                shiny::h4(shiny::tags$strong("Impact on distribution")),
                plotly::plotlyOutput(ns("histogram"),
                                     height = paste0(NROW(
                                         vars_to_plot
                                     ) * 200, "px"))
            )


        })


    }
