
# UI ----------------------------------------------------------------------


module_ui_failed_filters <- function(id){

    ns <- shiny::NS(id)

    shiny::uiOutput(outputId = ns("failedfilter"))




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




    output$failedfilter <- shiny::renderUI({



            print(statements)
            states <- try({checked_filter(df,statements)})$succeeded


            if(!is.null(states)){

                return(shiny::tagList(shiny::tags$p("Filter(s) ",
                                                    # str(states),
                                                    shiny::tags$b(
                                                        paste(which(states == FALSE),
                                                              collapse = ", ")),
                                                   " won't be applied, due to invalid statement(s).")))

                # return(print(states))
            } else {
                return(print(NULL))
            }



    })



}



