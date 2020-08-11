
# UI ----------------------------------------------------------------------



#' UI Module: data summary
#'
#' @param id shiny standard
#' @param cond_id character,
#'
#'
module_ui_checkbox <- function(id, cond_id) {
    ns <- shiny::NS(id)



    if(!is.character(cond_id)){
        stop("supply string matching an input/output namespace.")
    }

    space <- "output."

    shiny::tagList(
        # shiny::conditionalPanel(condition = paste0(space, cond_id, " !== null"),
        shiny::conditionalPanel(condition = paste0("input['", cond_id ,"'] != ''"),
                                shiny::htmlOutput(ns("checkbox")))
    )
}



# Server ------------------------------------------------------------------





#' Server Module: checkbox rendering
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param text Character, appears next to checkbox (or coerced)
module_server_checkbox <- function(input, output, session, text) {

    output$checkbox <- shiny::renderUI({

        ns <- session$ns

        shiny::checkboxInput(ns("checkbox"),
                             label = text,
                             value = FALSE,
                             width = NULL)

    })

    check_val <- shiny::reactiveVal()

    shiny::observe({
        if(!is.null(input$checkbox)){
            check_val(input$checkbox)
        } else {
            check_val(FALSE)
        }
    })


    return(check_val)


}

