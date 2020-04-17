#' Data cleanr
#'
#' @param data_set dataframe
#'
#' @return idxs of selected data
#' @export
#'
#'
datacleanr <- function(dataset){





    # define layout

    ui <-
        navbarPageWithInputs("datacleanr",

                     id = "nav",
                     # TAB GROUPING ------------
                     shiny::tabPanel("Grouping",
                              value = "grouping",
                              icon = shiny::icon("layer-group"),

                              # panel set-up

                              shiny::sidebarLayout(
                                  sidebarPanel = shiny::sidebarPanel("Test",
                                                              module_ui_group_select(df = dataset,
                                                                                     id = "group"),
                                                              width = 3),
                                  mainPanel = shiny::mainPanel()
                              )


                              ),
                     # TAB FILTERING -----------
                     shiny::tabPanel("Filtering",
                              value = "filtering",
                              icon = shiny::icon("sliders-h")),
                     # TAB VIS -----------------
                     shiny::tabPanel("Visualization",
                              value = "visu",
                              icon = shiny::icon("chart-area")),
                     # EXTRACT VIS -------------
                     shiny::tabPanel("Extraction",
                              value = "extract",
                              icon = shiny::icon("file-export")),


                     inputs = list(miniUI::miniTitleBarButton("done",
                                                      "Done",
                                                      primary = TRUE),

                                   miniUI::miniTitleBarCancelButton(inputId = "cancel",
                                                            label = "Cancel",
                                                            primary = FALSE))



        )



    server <- function(input, output, session){




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



    # END ---------------------------
            observeEvent(input$done, {
                stopApp("Done")
            })
            observeEvent(input$cancel, {
                stopApp(NULL)
            })
    #
    #     }
    # }



    }



    shiny::runGadget(ui,
              server,
              # viewer = browserViewer()
              viewer = dialogViewer(dialogName = "Data Cleaning - A. Hurley",
                                    width = 1000,
                                    height = 800)
    )
    # shinyApp(ui, server)
}



# pick_points(mtcars, ~wt, ~mpg)

