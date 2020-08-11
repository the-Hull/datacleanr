
# UI ----------------------------------------------------------------------



#' UI Module: group selection
#'
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


        vars <- colnames(dframe)[get_factor_cols_idx(dframe)]

        shiny::tagList(shiny::selectInput(inputId = ns("groupvar"),
                                          label = "Grouping Variables",
                                          choices = vars,
                                          selected = NULL,
                                          multiple = TRUE,
                                          selectize = TRUE)
        )


    })

    if(is.null(shiny::reactive({input$groupvar}))) {
        return(shiny::reactive({NULL}))

    } else {
        return(shiny::reactive({input$groupvar}))
    }


}

