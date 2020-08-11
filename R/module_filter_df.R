
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
#' @param df data frame/tibble for filtering
#' @param statements character, lengths >= 0, with (valid) statements for \code{base::subset}
#'
#' @return df, either filtered or original, based on validity of \code{statements}
#'
module_server_df_filter <- function(input, output, session, df, statements){

    ns  <-  session$ns

    out <- try({checked_filter(df,statements)})
    states <- out$succeeded

    if(utils::hasName(out, "filtered_df")){
        percent_filtered <- round(100 * (1 - ( nrow(out$filtered_df) /
                                                   nrow(df))),0)
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
        return(out$filtered_df)
    } else if(all(!states)){
        # print("heeere")
        # print(df)

        return(df)
    }
}



