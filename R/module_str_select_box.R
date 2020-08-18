# UI ----------------------------------------------------------------------

#' UI Module: box for str filter condition
#'
#' @param id Character, identifier for variable selection
#' @param actionbtn reactive, action button counter
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
#' @param selector character, html selector for placement
#' @param actionbtn reactive, action button counter
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
        id=paste0(id, "-filt"),
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
#' @param reactive, data frame passed into dcr app
#'
#' @details provides UI text box element
module_server_filter_str <- function(input, output, session, dframe){
    ns = session$ns



    output$filter <- shiny::renderUI({
        shiny::fluidRow(
            shiny::column(width = 8,
                          shiny::textInput(
                              inputId = ns("filter"),
                              # label = paste0("Filter ", strsplit(x = ns(""), split = "-")),
                              label = paste0("Filter ", substr(x = ns(""),
                                                               regexpr("[0-9]", ns("")),
                                                               regexpr("[0-9]", ns("")))),
                              value = NULL,
                              width = "100%",
                              placeholder = NULL
                          )),


            shiny::column(width = 4,
                          align = "center",
                          # style="margin-top: 20px;",


                          shinyWidgets::pickerInput(
                              inputId = ns("groupdropdown"),
                              label = "Select/deselect Groups",
                              choices = unique(dplyr::group_indices(dframe)),
                              options = list(
                                  `actions-box` = TRUE),
                              multiple = TRUE
                          )


                          )
        )

    })
}
