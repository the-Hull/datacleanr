

# alternative -------------------------------------------------------------


#------------------------------------------------------------------------------#
# MODULE UI ----
#' UI Module: box for str filter condition
#'
#' @param id Character string
#'
module_ui_group_selector_table <- function(id) {
    ns <- NS(id)


            shiny::tagList(
                DT::DTOutput(ns('grouptable')),
                shiny::textOutput(ns('selected_row'))
            )

}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

#' Server Module: box for str filter condition
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param df data frame (either from overview or filtering tab)
#'
#' @details provides UI text box element
module_server_group_selector_table <- function(input, output, session, df){
    ns = session$ns


    output$grouptable <- DT::renderDT(iris,
                                    selection = 'single')
    # output$grouptable <- DT::renderDT(df,
    #                                   options = list(selection = 'single'))






    output$selected_row <- shiny::renderText({

    dt_selected_row <- input$grouptable_rows_selected
        req(input$gobutton)

        # if(req(dt_selected_row)){
            paste0("Selected Row is: ", dt_selected_row)
        # } else {
#
            # NULL
        # }
        })


}
