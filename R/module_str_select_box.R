# UI ----------------------------------------------------------------------

#' UI Module: box for str filter condition
#'
#' @param df data.frame loaded into gadget; should support df, tibble, data.table
#' @param id Character, identifier for variable selection
#'
#'
module_ui_box_str_filter <- function(id, actionbtn){
    ns <- shiny::NS(id)


    shiny::tagList(shiny::h4(paste("Filter condition", actionbtn)),
        shiny::textInput(inputId = ns("strfilter"),
                         label = NULL,
                         # label = paste("Filter condition", actionbtn),
                         value = NULL,
                         width = "100%",
                         placeholder = NULL)
    )
}

# Server ------------------------------------------------------------------

#' Server Module: box for str filter condition
#'
#' @param input,output,session standard
#'
#'
module_server_box_str_filter <- function(input,
                                         output,
                                         session,
                                         selector,
                                         actionbtn){

    shiny::insertUI(
        selector = selector,
        where = "beforeEnd",
        ui = shiny::tags$div(id = paste0("div-filter", actionbtn),

                      module_ui_box_str_filter(paste0("filter", actionbtn), actionbtn),
        )
    )


}



# alternative -------------------------------------------------------------


#------------------------------------------------------------------------------#
# MODULE UI ----
#' UI Module: box for str filter condition
#'
#' @param id Character string
#'
module_ui_filter_str <- function(id) {
    ns <- shiny::NS(id)

    shiny::tags$div(
        id=paste0("filt", id),
        shiny::fluidRow(

                shiny::uiOutput(ns('filter'))

        )
    )
}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

#' Server Module: box for str filter condition
#'
#' @param input,output,session standard \code{shiny} boilerplate
#'
#' @details provides UI text box element
module_server_filter_str <- function(input, output, session){
    ns = session$ns

    output$filter <- shiny::renderUI({
        shiny::textInput(
            inputId = ns("filter"),
            label = paste0("Filter ", strsplit(x = ns(""), split = "-")),
            value = NULL,
            width = "100%",
            placeholder = NULL
        )
    })
}
