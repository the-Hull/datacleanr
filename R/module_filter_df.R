
# UI ----------------------------------------------------------------------


#' UI Module: filter info text output
#'
#' @param id character, shiny namespacing
#'
#' @return UI text element giving number of failed filters and percent of filtered rows
#'
module_ui_df_filter <- function(id){

    ns <- shiny::NS(id)

    shiny::uiOutput(outputId = ns("filterdf"))




}


# server ------------------------------------------------------------------


#' Server Module: filter info text and filtered df output
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param dframe data frame/tibble for filtering
#' @param condition_df data frame/tibble with filtering conditions and grouping scope
#'
#' @return df, either filtered or original, based on validity of \code{statements} in \code{condition_df}
#'
module_server_df_filter <- function(input, output, session, dframe, condition_df){

    ns  <-  session$ns

    states <- sapply(condition_df[ , 1, drop = TRUE],
                     function(x)
                         check_individual_statement(df = dframe,
                                                    statement = x))





    if(any(states) ){
    out <- filter_scoped_iterate(dframe = dframe,
                                 condition_df = condition_df)


        percent_filtered <- round(100 * (1 - ( nrow(out) /
                                                   nrow(dframe))),0)
    } else {

        percent_filtered <-  0
    }

    # print(paste("FILTER %:", percent_filtered))

    output$filterdf <- shiny::renderUI({

        if(!is.null(states) & any(!states)){

            return(shiny::tagList(shiny::tags$p("Filter(s) ",
                                                # str(states),
                                                shiny::tags$b(
                                                    paste(which(states == FALSE),
                                                          collapse = ", ")),
                                                " won't be applied, due to invalid statement(s).",
                                                shiny::tags$br(),
                                                shiny::tags$br(),
                                                "A total of",
                                                shiny::tags$b(paste(percent_filtered), "%"),
                                                "will be filtered.")))

        } else if(!is.null(states) & all(states)){

            return(shiny::tagList(shiny::tags$p(
                # "Filter(s) ",
                # str(states),
                # shiny::tags$b(
                #     paste(which(states == FALSE),
                #           collapse = ", ")),
                # " won't be applied, due to invalid statement(s).",
                # shiny::tags$br(),
                # shiny::tags$br(),
                "A total of",
                shiny::tags$b(paste(percent_filtered), "%"),
                "will be filtered.")))

        } else {
            return(print(NULL))
        }
    })

    if(any(states)){
        # print("this yeah.")
        # print(out$filtered_df)
        return(out)
    } else if(all(!states)){
        # print("heeere")
        # print(dframe)

        return(dframe)
    }
}



