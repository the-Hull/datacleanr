# shiny::actionButton(ns("undoselection"),
#                     label = "Undo last selection",
#                     icon = shiny::icon("undo"),
#                     class = "btn-warning"),
# shiny::actionButton(ns("clearselection"),
#                     label = "Clear all",
#                     icon = shiny::icon("exclamation-triangle"),
#                     class = "btn-danger"),
#


#------------------------------------------------------------------------------#
# MODULE UI ----
#' UI Module: Delete selection buttons
#'
#' @param id Character string
#'
module_ui_deleteselection_btn <- function(id) {
    ns <- shiny::NS(id)


    shiny::tagList(
        shiny::uiOutput(ns('deleteselection'))
    )

}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

#' Server Module: box for str filter condition
#'
#' @param input,output,session standard \code{shiny} boilerplate
#'
#' @details provides UI buttons for deleting last / entire outlier selection
#'
#' @return reactive values with input xvar, yvar and actionbutton counter
module_server_deleteselection_btn  <- function(input, output, session){
    ns = session$ns


    output$scatterselectControl <- shiny::renderUI({
        shiny::fluidRow(style = "margin-bottom: 25px;",
            shiny::column(6,
                          align = "left",
                          shiny::actionButton(ns("undoselection"),
                                label = "Undo last selection",
                                icon = shiny::icon("undo"),
                                class = "btn-warning")),
            shiny::column(6,
                          align = "right",
                          shiny::actionButton(ns("clearselection"),
                                label = "Clear all",
                                icon = shiny::icon("exclamation-triangle"),
                                class = "btn-danger"))



            # column(3, shiny::varSelectInput(ns('xvar'),
            #                                 label = "X Var",
            #                                 data = df[ , !grepl("[.]dcr", colnames(df))])),
            # column(3,
            #        shiny::varSelectInput(ns('yvar'),
            #                              label = "Y Var",
            #                              data = df[ ,!grepl("[.]dcr", colnames(df))])),
            # # column(3,
            # #        shiny::varSelectInput(ns('zvar'),
            # #                              label = "Z Var",
            # #                              data = df[ , colnames(df)!=".dcrkey"])),
            # column(3,
            #        shiny::selectInput(ns('zvar'),
            #                           label = "Z Var",
            #                           choices = c("", colnames(df)[!grepl("[.]dcr", colnames(df))]),
            #                           selected = NULL)),
            # column(1,
            #        style = "margin-top: 25px;",
            #        shiny::actionButton(ns('startscatter'),
            #                            label = "Plot!",
            #                            icon = shiny::icon("chart-area"),
            #                            class = "btn-info")
            # )
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
