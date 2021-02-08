
# UI ----------------------------------------------------------------------


#' UI Module: data summary
#'
#' @param id shiny standard
#'
#'
module_ui_summary <- function(id) {
  ns <- shiny::NS(id)


  shiny::tagList(
    shiny::uiOutput(ns("gosummarybutton")),
    shiny::htmlOutput(ns("summary"))
  )

  # shiny::htmlOutput(ns("summary"))
}

# Server ------------------------------------------------------------------

#' Server Module: data summary
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param dframe reactive, input data frame
#' @param df_label character, name of initial data set
#' @param start_clicked reactive holding start action button
#' @param group_var_check reactive holding group check output
#'
module_server_summary <- function(input,
                                  output,
                                  session,
                                  dframe,
                                  df_label,
                                  start_clicked,
                                  group_var_check) {
  ns <- session$ns

  output$gosummarybutton <- shiny::renderUI({
    shiny::validate(shiny::need(
      start_clicked(),
      'Click "Set and Start" to enable Overview Summary'
    ))


    shiny::actionButton(ns("gosummary"),
      "Generate Overview",
      icon = shiny::icon("rocket"),
      class = "btn-info"
    )
  })

  shiny::observeEvent(input$gosummary, {
    df <- {
      if (!is.null(dframe()) &&
        !group_var_check()) {
        dplyr::ungroup(dframe())
      } else if (!is.null(dframe()) &&
        group_var_check()) {
        dframe()
      }
    }




    # shiny::req(need(df(), message = "Click on Start!"))

    dfs <- summarytools::dfSummary(df[, !grepl("[.]dcr", colnames(df))])

    if (all(class(dfs) == "stby")) {
      invisible(lapply(seq_along(dfs), function(x) {
        attr(dfs[[x]], "data_info")$Data.frame <<- df_label
      }))
    } else if (class(dfs)[1] == "summarytools") {
      attr(dfs, "data_info")$Data.frame <- df_label
    }



    html_summary <- shiny::renderUI(print(
      dfs,
      method = "render",
      bootstrap.css = FALSE
    ))

    output$summary <- html_summary
  })
}
