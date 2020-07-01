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
                                        icon = shiny::icon("check-double")),
                    shiny::actionButton(inputId = ns("resetfilter"),
                                        label = "Reset",
                                        icon = shiny::icon("undo")))
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
        # print("Applied")
        # print(dplyr::is.grouped_df(output))
        # print(dplyr::group_vars(output$data))
    })

    shiny::observeEvent(input$resetfilter, {


        print("before")
        print(dplyr::group_vars(df_original()))
        output$data <- df_original()

        print(paste("reset filter!!! origina input was:", nrow(df_original()),
                    "reset output is:", nrow(output$data)))

        print("Reset")
        print(dplyr::group_vars(output$data))
    })

    # shiny::observeEvent(input[[ns("gobutton")]], {
    #     output$data <- df_original
    #     print(nrow(output$data))
    #     print("No filtering")
    #     print(output$data)
    #     # print(dplyr::is.grouped_df(output))
    #     print(dplyr::group_vars(output$data))
    # })



    return(output)
}
