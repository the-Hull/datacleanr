
# UI ----------------------------------------------------------------------


module_ui_failed_filters <- function(id){

    ns <- shiny::NS(id)

    shiny::verbatimTextOutput(outputId = ns("failedfilter"))




}


# server ------------------------------------------------------------------


#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param df
#' @param statements
#'
#' @return
#'
module_server_failed_filters <- function(input, output, session, df, statements){

    ns  <-  session$ns




    output$failedfilter <- shiny::renderPrint({



            print(statements)
            states <- try({checked_filter(df,statements)})


            if(!is.null(states)){

                # return(shiny::tagList(shiny::tags$p("Filters "),
                #                shiny::tags$b(which(states == FALSE)),
                #                shiny::tags$p(" are invalid.")))

                return(print(states))
            } else {
                return(print("ABC"))
            }



    })



}



