
# UI ----------------------------------------------------------------------


module_ui_df_filter <- function(id){

    ns <- shiny::NS(id)

    shiny::uiOutput(outputId = ns("filterdf"))




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
module_server_df_filter <- function(input, output, session, df, statements){

    ns  <-  session$ns

    print(statements)
    out <- try({checked_filter(df,statements)})
    states <- out$succeeded


    output$filterdf <- shiny::renderUI({

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


    if(any(states)){
        return(out$filtered_df)
    } else {
        return(df)
    }





}



