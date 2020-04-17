
# UI ----------------------------------------------------------------------


#' UI Module: data summary
#'
#' @param id shiny standard
#'
#'
module_ui_summary <- function(id){
    ns <- shiny::NS(id)


    shiny::tagList(
        # shiny::uiOutput(ns("summary"))
        shiny::htmlOutput(ns("summary"))
    )

}

# Server ------------------------------------------------------------------

#' Server Module: data summary
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param df input data frame, pass into function with \code{df = reactive({data})}
#' @param df_label character, name of initial data set
#'
module_server_summary <- function(input,
                                  output,
                                  session,
                                  df,
                                  df_label){



#


        dfs <- summarytools::dfSummary(df)
        attr(dfs, "data_info")$Data.frame <- df_label


        html_summary <- shiny::renderUI(summarytools::view(
            dfs,
            method = "render"))

        output$summary <- html_summary





}
