
# UI ----------------------------------------------------------------------


module_ui_failed_filters <- function(id){

    ns <- shiny::NS(id)

    shiny::uiOutput(outputId = ns("failedfilter"))




}


# server ------------------------------------------------------------------


module_server_failed_filters <- function(input, output, session, df, statements){

    ns  <-  session$ns

    output$failedfilter <- shiny::renderUI({

        states <- checked_filter(df,statements)$succeeded


        shiny::tagList(shiny::tags$p("Filters "),
                       shiny::tags$b(states),
                       shiny::tags$p(" are invalid."))

    })



}



