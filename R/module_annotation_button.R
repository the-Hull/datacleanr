#------------------------------------------------------------------------------#
# MODULE UI ----
#' UI Module: Selection Annotator
#'
#' @param id Character string
#'
module_ui_text_annotator <- function(id) {
    ns <- shiny::NS(id)


    shiny::tagList(
        # shiny::uiOutput(ns('text_annotator'))
        shiny::br(),
        shiny::fluidRow(
            # shiny::textOutput(ns('df_descriptor')),
            shiny::column(8,
                          shiny::textInput(inputId = ns('textinput'),
                                           value = "outlier",
                                           label = NULL)),
            shiny::column(4,
                          shiny::actionButton(inputId = ns('annotate_button'),
                                              icon = shiny::icon("paragraph"),
                                              class = "btn-info",
                                              label = "Annotate!")))
    )

}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

#' Server Module:  Selection Annotator
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param sel_data reactive df
#'
#' @details provides UI text box element
#'
#' @return reactive values with input xvar, yvar and actionbutton counter
module_server_text_annotator  <- function(input, output, session, sel_data){
    ns = session$ns


    if(nrow(sel_data$df) > 0){

        annotate_ind <- which(sel_data$df$selection_count == max(sel_data$df$selection_count, na.rm = TRUE))

        sel_data$df[annotate_ind, ".annotation"] <- input$textinput




    }



    return(sel_data$df)


}


