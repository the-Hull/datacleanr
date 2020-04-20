#' Data cleanr
#'
#' @param data_set dataframe
#'
#' @return idxs of selected data
#' @export
#'
#'
datacleanr <- function(dataset){

    df_name <- deparse(substitute(dataset))

    # Panel texts:

    text_grouping_side_panel <- shiny::tagList(shiny::h3("Overview"),
                                shiny::br(),
                                shiny::p("Some text for testing the use of
                                external text and wrapping"),
                                shiny::br())



    # define layout

    ui <-
        navbarPageWithInputs("datacleanr",




                             id = "nav",


                             # TAB GROUPING ------------
                             shiny::tabPanel("Overview & Set-up",
                                             value = "grouping",
                                             icon = shiny::icon("layer-group"),

                                             # panel set-up

                                             shiny::sidebarLayout(

                                                 sidebarPanel = shiny::sidebarPanel(

                                                     text_grouping_side_panel,
                                                     module_ui_group_select(df = dataset,
                                                                            id = "group"),

                                                     module_ui_checkbox(id = "grouptick",
                                                                        cond_id = "gvar"),


                                                     shiny::br(),


                                                     shiny::actionButton("gobutton",
                                                                         "Start",
                                                                         icon = shiny::icon("rocket")),

                                                     shiny::textOutput("checkworked"),





                                                     width = 3
                                                 ),

                                                 mainPanel = shiny::mainPanel(


                                                     # Diagnostics################
                                                     textOutput('show_inputs'),###
                                                     #############################

                                                     module_ui_summary(id = "summary")

                                                 )
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
                                                                            primary = FALSE)),




                             tags$style(type = "text/css", "body {padding-top: 70px;}"),
                             position = "fixed-top"


        )



    server <- function(input, output, session){


        # DIAGNOSTICS ----------------------

        AllInputs <- reactive({
            x <- unlist(reactiveValuesToList(input))
            paste(names(x),
                x)


        })

        output$show_inputs <- renderText({
            AllInputs()
        })

        # ---------------------------


        # get grouping
        gvar <- shiny::callModule(module_server_group_select,
                                  id = "group")
        output$gvar <- reactive({gvar()})


        # check-box for grouping
        shiny::callModule(module = module_server_checkbox,
                          "grouptick",
                          text = "Use grouping for summary")

        outputOptions(output, "gvar", suspendWhenHidden = FALSE)





        output$checkworked <- shiny::renderPrint({

            if(shiny::req(input$`grouptick-checkbox`)){

                paste(input$`grouptick-checkbox`, "was set!")

            names(input)
            } else {

                "Nothing yet."
            }

        })

        # handle initialization
        datareactive <- shiny::reactiveVal()

        datareactive <- shiny::eventReactive(input$gobutton, {

            df <- apply_data_set_up(df = dataset, gvar())

            return(df)
        })




        # shiny::observe({print(input$group)})
        shiny::observe({print(gvar())})
        shiny::observe({print(input$`grouptick-checkbox`)})
        # shiny::observe({print(datareactive())})




        data_summary <- shiny::eventReactive(input$gobutton,

                                             {


                                                 # df exists, but not check box
                                                 if(!is.null(datareactive()) &
                                                    !input$`grouptick-checkbox`){

                                                     return(ungroup(datareactive()))

                                                 } else if(!is.null(datareactive()) &
                                                           input$`grouptick-checkbox`){

                                                     return(datareactive())

                                                 }



                                             })





        # Summary -------------------

        # Add grouping selectors
        shiny::observe({

            # shiny::req(datareactive())
            # datanonreactive <- datareactive()



            # account for grouptick in printing

            # data_summary <- shiny::reactive({





            # })



            shiny::callModule(module_server_summary,
                              "summary",


                              df = data_summary(),


                              df_label = df_name)


        })





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



    }



    shiny::runGadget(ui,
              server,
              # viewer = browserViewer()
              viewer = shiny::dialogViewer(dialogName = "Data Cleaning - A. Hurley",
                                    width = 1000,
                                    height = 800)
    )
}



# pick_points(mtcars, ~wt, ~mpg)

