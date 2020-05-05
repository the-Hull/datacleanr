
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


# module_ui_summarytool

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
        # ui = tagList(
        #     module_ui_box_str_filter(paste0("filter", actionbtn), actionbtn),
        # )
    )


    # cond_string <- paste(list("Species == 'setosa'",
    #                           "Petal.Length > 1.3"), collapse = " & ")
    #
    # dplyr::filter(iris, eval(str2expression(cond_string)))



}



# alternative -------------------------------------------------------------


#------------------------------------------------------------------------------#
# MODULE UI ----
module_ui_filter_str <- function(id) {
    ns <- NS(id)

    tags$div(
        id=paste0("filt", id),
        fluidRow(

                uiOutput(ns('filter'))

        )
    )
}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

module_server_filter_str <- function(input, output, session){
    ns = session$ns

    output$filter <- renderUI({
        shiny::textInput(
            inputId = ns("filter"),
            label = paste0("Filter ", strsplit(x = ns(""), split = "-")),
            value = "",
            width = "100%",
            placeholder = NULL
        )
    })
}
