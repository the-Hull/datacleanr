
# UI ----------------------------------------------------------------------



#' UI Module: group selection
#'
#' @param df data.frame loaded into gadget; should support df, tibble, data.table
#' @param id Character, identifier for variable selection
#'
#'
module_ui_group_select <- function(id){
    ns <- shiny::NS(id)

    # vars <- colnames(df)[get_factor_cols_idx(df)]
    #
    # shiny::tagList(shiny::selectInput(inputId = ns("groupvar"),
    #                                   label = "Grouping Variables",
    #                                   choices = vars,
    #                                   selected = NULL,
    #                                   multiple = TRUE,
    #                                   selectize = TRUE)
    # )

    shiny::uiOutput(ns("checkbox"))


}


# module_ui_summarytool

# Server ------------------------------------------------------------------

#' Server Module: group selection
#'
#' @param input,output,session standard
#' @param dframe data frame for filtering
#'
#'
module_server_group_select <- function(input, output, session, dframe){

    ns <- session$ns


    output$checkbox <- shiny::renderUI({

        print(head(dframe))

        vars <- colnames(dframe)[get_factor_cols_idx(dframe)]

        shiny::tagList(shiny::selectInput(inputId = ns("groupvar"),
                                          label = "Grouping Variables",
                                          choices = vars,
                                          selected = NULL,
                                          multiple = TRUE,
                                          selectize = TRUE)
        )


    })

    if(is.null(reactive({input$groupvar}))) {
        return(reactive({NULL}))

    } else {
        return(reactive({input$groupvar}))
    }


}

