#------------------------------------------------------------------------------#
# MODULE UI ----
#' UI Module: Grouptable Relayout Buttons
#'
#' @param id Character string
#'
module_ui_group_relayout_buttons <- function(id) {
    ns <- shiny::NS(id)


    shiny::uiOutput(ns('group_relayout'))

}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

#' Server Module:  Selection Annotator
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param startscatter reactive, actionbutton value
#'
#' @details provides UI text box element
#'
#' @return reactive values with input xvar, yvar and actionbutton counter
module_server_group_relayout_buttons  <- function(input, output, session, startscatter){
    ns = session$ns



    relayout_buttons_tl <- shiny::tagList(
        shiny::column(
            4,
            align = "right",
            shiny::actionButton(
                inputId = ns('update'),
                label = "Update Plot Groups",
                icon = shiny::icon("paragraph"),
                class = "btn-info")),
        shiny::column(
            4,
            align = "right",
            shiny::actionButton(
                inputId = ns('clear'),
                icon = shiny::icon("paragraph"),
                class = "btn-danger",
                label = "Clear all Plot Groups")
        )
    )




    output$group_relayout <- shiny::renderUI({


        shiny::validate(
            shiny::need(
                startscatter(),
                message = ""
            )
        )

        relayout_buttons_tl

    })


}


