#' Data cleanr
#'
#' @param data_set dataframe
#'
#' @return idxs of selected data
#' @export
#'
#' @import miniUI
#'  shiny
#'  ggplot2
#'
datacleanr <- function(data){


    # Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
    # want to add an input to the navbar
    navbarPageWithInputs <- function(..., inputs) {
        navbar <- navbarPage(...)
        form <- tags$form(class = "navbar-form", inputs)
        navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
            navbar[[3]][[1]]$children[[1]], form)
        navbar
    }


    # define layout

    ui <-
        navbarPageWithInputs("datacleanr",

                     id = "nav",
                     # TAB GROUPING ------------
                     tabPanel("Grouping",
                              value = "grouping",
                              icon = icon("layer-group")),
                     # TAB FILTERING -----------
                     tabPanel("Filtering",
                              value = "filtering",
                              icon = icon("sliders-h")),
                     # TAB VIS -----------------
                     tabPanel("Visualization",
                              value = "visu",
                              icon = icon("chart-area")),
                     # EXTRACT VIS -------------
                     tabPanel("Extraction",
                              value = "extract",
                              icon = icon("file-export")),


                     inputs = list(miniUI::miniTitleBarButton("done",
                                                      "Done",
                                                      primary = TRUE),

                                   miniUI::miniTitleBarCancelButton(inputId = "cancel",
                                                            label = "Cancel",
                                                            primary = FALSE))



        )



    server <- function(input, output, session){

    }


    # ui <- miniPage(
    #     gadgetTitleBar(paste("Select points")),
    #     miniContentPanel(padding = 0,
    #                      scatter_plot_ui_module("plot1"),
    #                      # plotOutput("plot1", height = "100%", brush = "brush")
    #     ),
    #     miniButtonBlock(
    #         actionButton("add", "", icon = icon("thumbs-up")),
    #         actionButton("sub", "", icon = icon("thumbs-down")),
    #         actionButton("none", "" , icon = icon("ban")),
    #         actionButton("all", "", icon = icon("refresh"))
    #     )
    # )
    #
    # server <- function(input, output) {
    #     # For storing selected points
    #     vals <- reactiveValues(keep = rep(TRUE, nrow(data)))
    #
    #     {selected <- callModule(scatter_plot_svr_module, "plot1", data = data, vals = vals, x = x, y = y)
    #
    #         # output$plot1 <- renderPlot({
    #         #     # Plot the kept and excluded points as two separate data sets
    #         #     keep    <- data[ vals$keep, , drop = FALSE]
    #         #     exclude <- data[!vals$keep, , drop = FALSE]
    #         #
    #         #     ggplot(keep, aes_(x, y)) +
    #         #         geom_point(data = exclude, color = "grey80") +
    #         #         geom_point()
    #         # })
    #         #
    #         # # Update selected points
    #         # selected <- reactive({
    #         #     brushedPoints(data, input$brush, allRows = TRUE)$selected_
    #         # })
    #         observeEvent(input$add,  vals$keep <- vals$keep | selected())
    #         observeEvent(input$sub,  vals$keep <- vals$keep & !selected())
    #         observeEvent(input$all,  vals$keep <- rep(TRUE, nrow(data)))
    #         observeEvent(input$none, vals$keep <- rep(FALSE, nrow(data)))
    #
    #
    #         # callModule(handle_selections, "brush")
    #
    #         observeEvent(input$done, {
    #             stopApp(vals$keep)
    #         })
    #         observeEvent(input$cancel, {
    #             stopApp(NULL)
    #         })
    #
    #     }
    # }







    runGadget(ui,
              server,
              viewer = browserViewer()
              # viewer = dialogViewer(dialogName = "Data Cleaning - A. Hurley",
              #                       width = 1000,
              #                       height = 800)
    )
    # shinyApp(ui, server)
}



# pick_points(mtcars, ~wt, ~mpg)

