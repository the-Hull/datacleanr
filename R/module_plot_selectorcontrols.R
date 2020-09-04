#------------------------------------------------------------------------------#
# MODULE UI ----
#' UI Module: selector controls
#'
#' @param id Character string
#'
module_ui_plot_selectorcontrols <- function(id) {
    ns <- shiny::NS(id)


    shiny::tagList(
        # shiny::textOutput(ns('df_descriptor')),
        shiny::uiOutput(ns('scatterselectControl'))


        # DT::DTOutput(ns('grouptable')),
        # shiny::textOutput(ns('selected_row'))
    )

}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

#' Server Module: box for str filter condition
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param df df (not reactive - prevent re-execution of observer)
#'
#' @details provides UI text box element
#'
#' @return reactive values with input xvar, yvar and actionbutton counter
module_server_plot_selectorcontrols  <- function(input, output, session, df){
    ns = session$ns


    output$scatterselectControl <- shiny::renderUI({
        shiny::fluidRow(
            shiny::column(3, shiny::varSelectInput(ns('xvar'),
                                                   label = "X Var",
                                                   data = df[ , !grepl("[.]dcr", colnames(df))])),
            shiny::column(3,
                          shiny::varSelectInput(ns('yvar'),
                                                label = "Y Var",
                                                data = df[ ,!grepl("[.]dcr", colnames(df))])),
            # column(3,
            #        shiny::varSelectInput(ns('zvar'),
            #                              label = "Z Var",
            #                              data = df[ , colnames(df)!=".dcrkey"])),
            shiny::column(3,
                          shiny::selectInput(ns('zvar'),
                                             label = "Z Var",
                                             choices = c("", colnames(df)[!grepl("[.]dcr", colnames(df))]),
                                             selected = NULL)),
            shiny::column(3,
                          align = "right",
                          style = "margin-top: 25px;",
                          shiny::actionButton(ns('startscatter'),
                                              label = "Plot",
                                              icon = shiny::icon("chart-area"),
                                              class = "btn-info")
            )
        )

    })

    # output$scatterselectControl <- shiny::renderUI({
    #       shiny::splitLayout(shiny::varSelectInput(ns('xvar'),
    #                                         label = "X Var",
    #                                         data = df$df$data),
    #                shiny::varSelectInput(ns('yvar'),
    #                                      label = "Y Var",
    #                                      data = df$df$data),
    #                shiny::actionButton(ns('startscatter'),
    #                                    label = "Plot!",
    #                                    icon = shiny::icon("chart-area")),
    #                style = "display:vertical-align: bottom;"
    #     )
    #
    # })


    # inputs_to_monitor <- shiny::reactiveValues(xvar = NULL,
    #                                            yvar= NULL,
    #                                            abutton= NULL)
    #
    # shiny::observe({
    #     inputs_to_monitor$xvar <- shiny::reactive({input$xvar})
    #     inputs_to_monitor$yvar <- shiny::reactive({input$yvar})
    #     inputs_to_monitor$zvar <- shiny::reactive({input$zvar})
    #     inputs_to_monitor$abutton <- shiny::reactive({input$startscatter})
    # })
    #
    #
    #
    # return(inputs_to_monitor)




    # }
}
