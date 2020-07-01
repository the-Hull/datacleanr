# UI ----------------------------------------------------------------------
#' UI Module: Apply/Reset Filtering
#'
#' @param df data.frame loaded into gadget; should support df, tibble, data.table
#' @param id Character, identifier for variable selection
#'
#'
module_ui_apply_reset <- function(id){

    ns <- shiny::NS(id)

    shiny::tagList( shiny::actionButton(inputId = ns("applyfilter"),
                                        label = "Apply",
                                        icon = shiny::icon("check-double"),
                                        class = "btn-info"),
                    shiny::actionButton(inputId = ns("resetfilter"),
                                        label = "Reset",
                                        icon = shiny::icon("undo"),
                                        class = "btn-danger"))
}
# Server ------------------------------------------------------------------

#' Server Module: apply / reset filter
#'
#' @param input,output,session standard
#'
#'
module_server_apply_reset <- function(input, output, session, df_filtered, df_original){

    ns <- session$ns



    output <- shiny::reactiveValues(data = NULL)


    shiny::observeEvent(input$applyfilter, {


        output$data <- df_filtered$df
        print(paste("applied filter!!! filter input was:", nrow(df_filtered$df),
                    "filter output is:", nrow(output$data)))
    })

    shiny::observeEvent(input$resetfilter, {


        output$data <- df_original()

    })




    return(output)
}
