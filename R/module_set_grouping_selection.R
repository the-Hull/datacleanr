
# UI ----------------------------------------------------------------------



#' UI Module: group selection
#'
#' @param id Character, identifier for variable selection
#'
#'
module_ui_group_select <- function(id) {
  ns <- shiny::NS(id)


  shiny::uiOutput(ns("groupvar"))
}


# module_ui_summarytool

# Server ------------------------------------------------------------------

#' Server Module: group selection
#'
#' @param input,output,session standard
#' @param dframe data frame for filtering
#'
#'
module_server_group_select <- function(input, output, session, dframe) {
  ns <- session$ns


  output$groupvar <- shiny::renderUI({
    vars <- colnames(dframe)[get_factor_cols_idx(dframe)]

    shiny::tagList(shiny::selectInput(
      inputId = ns("groupvar"),
      label = "Grouping Variables",
      choices = vars,
      selected = NULL,
      multiple = TRUE,
      selectize = TRUE
    ))
  })

  if (is.null(shiny::reactive({
    input$groupvar
  }))) {
    return(shiny::reactive({
      NULL
    }))
  } else {
    return(shiny::reactive({
      input$groupvar
    }))
  }
}
