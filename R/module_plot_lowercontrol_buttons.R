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
module_ui_lowercontrol_btn <- function(id) {
    ns <- shiny::NS(id)


    shiny::tagList(shiny::uiOutput(ns('lowercontrolbuttons')))

}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

#' Server Module: box for str filter condition
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param selector_inputs reactive vals from above-plot controls, used to determine if plot is a map (lon/lat)
#'
#' @details provides UI buttons for deleting last / entire outlier selection
#'
#' @return reactive values with input xvar, yvar and actionbutton counter
module_server_lowercontrol_btn  <-
    function(input, output, session, selector_inputs) {
        ns = session$ns

        is_spatial_plot <-
            identical(c(
                as.character(shiny::isolate(selector_inputs$xvar)),
                as.character(shiny::isolate(selector_inputs$yvar))
            ),
            c("lon", "lat"))


        output$lowercontrolbuttons <- shiny::renderUI({
            if (!is_spatial_plot) {
                shiny::fluidRow(
                    style = "margin-bottom: 25px;",
                    shiny::column(
                        6,
                        align = "left",
                        shiny::actionButton(
                            ns("undoselection"),
                            label = "Undo last selection",
                            icon = shiny::icon("undo"),
                            class = "btn-warning"
                        )
                    ),



                    shiny::column(
                        6,
                        align = "right",
                        shiny::actionButton(
                            ns("clearselection"),
                            label = "Clear all",
                            icon = shiny::icon("exclamation-triangle"),
                            class = "btn-danger"
                        )
                    )
                )

            } else {
                shiny::fluidRow(
                    style = "margin-bottom: 15px;",
                    shiny::column(
                        4,
                        stlye = "margin-top: 55px;",
                        align = "left",
                        shiny::actionButton(
                            ns("undoselection"),
                            label = "Undo last selection",
                            icon = shiny::icon("undo"),
                            class = "btn-warning"
                        )
                    ),

                    shiny::column(4,
                                  align = "center",
                                  selectInput(
                                     inputId =  ns("mapstyle"),
                                      label = "Select map style",
                                      choices = c(
                                          "open-street-map",
                                          "carto-positron",
                                          "carto-darkmatter",
                                          "stamen-terrain",
                                          "stamen-toner",
                                          "stamen-watercolor"
                                      ),
                                     selected = "open-street-map",
                                  )),
                    shiny::column(
                        4,
                        stlye = "margin-top: 55px;",
                        align = "right",
                        shiny::actionButton(
                            ns("clearselection"),
                            label = "Clear all",
                            icon = shiny::icon("exclamation-triangle"),
                            class = "btn-danger"
                        )
                    )
                )
            }
        })
    }
