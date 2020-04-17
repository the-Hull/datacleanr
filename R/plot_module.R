#' Make ui plot
#'
#' @param id Character, label for UI
#'
#' @return UI plotoutput
#'
scatter_plot_ui_module <- function(id){
    ns <- shiny::NS(id)
    plotOutput(ns("plot1"), height = "100%", brush = ns("brush"))


}



#' Make svr-side plot
#'
#' @param input Shiny basics
#' @param output Shiny basics
#' @param session Shiny basics
#' @param data data set passed to gadget app
#' @param vals reactive vals used for subsetting
#'
#' @return selected vals
#'
scatter_plot_svr_module <- function(input, output, session, data, vals, x, y) {

    # vals <- reactiveValues(keep = rep(TRUE, nrow(data)))



    output$plot1 <- renderPlot({
        # Plot the kept and excluded points as two separate data sets
        keep    <- data[ vals$keep, , drop = FALSE]
        exclude <- data[!vals$keep, , drop = FALSE]

        ggplot(keep, aes_(x, y)) +
            geom_point(data = exclude, color = "grey80") +
            geom_point()
    })



    # Update selected points
    selected <- reactive({
        brushedPoints(data, input$brush, allRows = TRUE)$selected_
    })

#
#     observeEvent(input$add,  vals$keep <- vals$keep | selected())
#     observeEvent(input$sub,  vals$keep <- vals$keep & !selected())
#     observeEvent(input$all,  vals$keep <- rep(TRUE, nrow(data)))
#     observeEvent(input$none, vals$keep <- rep(FALSE, nrow(data)))

    return(selected)
}


#' Handle point selection svr side
#'
#' @param input Shiny
#' @param output Shiny
#' @param session Shiny
#'
#' @return selected points
#'
handle_selections <- function(input, output, session){
    # Update selected points
    selected <- reactive({
        brushedPoints(data, input$brush, allRows = TRUE)$selected_
    })
    observeEvent(input$add,  vals$keep <- vals$keep | selected())
    observeEvent(input$sub,  vals$keep <- vals$keep & !selected())
    observeEvent(input$all,  vals$keep <- rep(TRUE, nrow(data)))
    observeEvent(input$none, vals$keep <- rep(FALSE, nrow(data)))

    return(selected)
}

