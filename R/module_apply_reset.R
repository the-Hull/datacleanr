# UI ----------------------------------------------------------------------
#' UI Module: Apply/Reset Filtering
#'
#' @param df data.frame loaded into gadget; should support df, tibble, data.table
#' @param id Character, identifier for variable selection
#'
#'
module_ui_apply_reset <- function(id){

    ns <- shiny::NS(id)

    shiny::tagList( shiny::actionButton(inputId = ns("applyfilter"),
                                        label = "Apply",
                                        icon = shiny::icon("check-double")),
                    shiny::actionButton(inputId = ns("resetfilter"),
                                        label = "Reset",
                                        icon = shiny::icon("undo")))
}
# Server ------------------------------------------------------------------

#' Server Module: apply / reset filter
#'
#' @param input,output,session standard
#'
#'
module_server_apply_reset <- function(input, output, session, df_filtered, df_original){

    output <- shiny::reactiveVal()

    shiny::observeEvent(input[["applyfilter"]], {
        output <- df_filtered
        print(nrow(output))
        print("Applied")
        print(output)
        # print(dplyr::is.grouped_df(output))
        print(dplyr::group_vars(output))
    })

    shiny::observeEvent(input[["resetfilter"]], {
        output <- df_original
        print(nrow(output))
        print("Reset")
        print(dplyr::group_vars(output))
    })
    return(output)
}
