
# UI ----------------------------------------------------------------------



#' UI Module: data summary
#'
#' @param id shiny standard
#'
#'
module_ui_checkbox <- function(id){
    ns <- shiny::NS(id)


    shiny::tagList(
        # shiny::uiOutput(ns("summary"))
        shiny::uiOutput(ns("checkbox"))
    )

}



# Server ------------------------------------------------------------------





#' Server Module: checkbox rendering
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param text Character, appears next to checkbox (or coerced)
#' @param conditional_reactive Reactive, either NULL or not
#'
module_server_checkbox <- function(input,
                               output,
                               session,
                               text,
                               conditional_reactive){


    if(!is.character(text)){
        stop("Please supply a text (as.character) for UI element")
    }


    if(!is.null(conditional_reactive)){

        checkbox_ui_element <- renderUI({

            checkboxInput("grouptick", label = text, value = FALSE, width = NULL)

                })

        output$checkbox <- checkbox_ui_element


    } else {

        output$checkbox <- NULL

    }


}

