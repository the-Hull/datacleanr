
# UI ----------------------------------------------------------------------



#' UI Module: group selection
#'
#' @param df data.frame loaded into gadget; should support df, tibble, data.table
#' @param id Character, identifier for variable selection
#'
#'
module_ui_group_select <- function(df, id){
    ns <- shiny::NS(id)

    vars <- colnames(df)[get_factor_cols_idx(df)]

    shiny::tagList(shiny::selectInput(inputId = ns("groupvar"),
                                      label = "Grouping Variables",
                                      choices = vars,
                                      selected = NULL,
                                      multiple = TRUE,
                                      selectize = TRUE)
    )


}


# module_ui_summarytool

# Server ------------------------------------------------------------------

#' Server Module: group selection
#'
#' @param input,output,session standard
#'
#'
module_server_group_select <- function(input, output, session){

    if(is.null(reactive({input$groupvar}))) {

        return(reactive({NULL}))


    } else {

        return(reactive({input$groupvar}))

    }


}

