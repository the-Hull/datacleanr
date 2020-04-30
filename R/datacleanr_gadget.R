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
                                shiny::p("Select grouping variables for subsequent cleaning."),
                                shiny::br(),
                                shiny::p("Choose if you want to view an overview with or without grouping."))



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

                                                     # shiny::textOutput("checkworked"),





                                                     width = 3
                                                 ),

                                                 mainPanel = shiny::mainPanel(


                                                     # Diagnostics################
                                                     # textOutput('show_inputs'),###
                                                     #############################

                                                     module_ui_summary(id = "summary")

                                                 )
                                             )


                             ),
                             # TAB FILTERING -----------
                             shiny::tabPanel("Filtering",
                                             value = "filtering",
                                             icon = shiny::icon("sliders-h"),


                                             shiny::sidebarLayout(

                                                 sidebarPanel = shiny::sidebarPanel(

                                                     text_grouping_side_panel,


                                                     shiny::br(),


                                                     shiny::actionButton("addbutton",
                                                                         "Add Filter",
                                                                         icon = shiny::icon("plus-circle")),
                                                     shiny::actionButton("removebutton",
                                                                         "Remove Filter",
                                                                         icon = shiny::icon("trash")),

                                                     # shiny::textOutput("checkworked"),





                                                     width = 3
                                                 ),

                                                 mainPanel = shiny::mainPanel(


                                                     # Diagnostics################
                                                     shiny::textOutput('show_inputs'),###
                                                     #############################

                                                    shiny::h2("Filter me!"),
                                                    shiny::tags$div(id = 'placeholder')


                                                 )
                                             )



                                             ),
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




                             shiny::tags$style(type = "text/css", "body {padding-top: 70px;}"),
                             position = "fixed-top"


        )



    server <- function(input, output, session){


        # DIAGNOSTICS ----------------------

        AllInputs <- shiny::reactive({
            x <- unlist(shiny::reactiveValuesToList(input))
            paste(names(x),
                x)


        })

        output$show_inputs <- shiny::renderText({
            AllInputs()
        })

        # ---------------------------


        # get grouping
        gvar <- shiny::callModule(module_server_group_select,
                                  id = "group")
        output$gvar <- shiny::reactive({gvar()})


        # check-box for grouping
        shiny::callModule(module = module_server_checkbox,
                          "grouptick",
                          text = "Use grouping for summary")

        shiny::outputOptions(output, "gvar", suspendWhenHidden = FALSE)





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




        shiny::observe({print(gvar())})
        shiny::observe({print(input$`grouptick-checkbox`)})



        # Summary -------------------

        # Add grouping selectors
        # shiny::observe({
        shiny::observeEvent(input$gobutton, {

            # shiny::req(datareactive())
            # datanonreactive <- datareactive()



            # account for grouptick in printing

            # data_summary <- shiny::reactive({





            # })



            shiny::callModule(module_server_summary,
                              "summary",


                              # df = data_summary(),
                              df =  {if(!is.null(datareactive()) &&
                                     !input$`grouptick-checkbox`){

                                      dplyr::ungroup(datareactive())

                                  } else if(!is.null(datareactive()) &&
                                            input$`grouptick-checkbox`){

                                      datareactive()

                                  }},


                              df_label = df_name)


        })


        #
         shiny::observeEvent(input$addbutton, {

            all_filters_lgl <- grepl("filter[0-9]+-strfilter", names(input))
            all_filters <- names(input)[all_filters_lgl]

             if(any(all_filters_lgl)){



            # all_filters <- sub("[ ]", replacement = "", all_filters)

                 filter_vals <- sapply(all_filters, function(x) input[[x]], USE.NAMES = FALSE)

                 keep <- sapply(filter_vals, nchar) > 0

                 filter_vals <- filter_vals[keep]

#
#             filter_numbers <- gsub(pattern = "[^0-9]",
#                                   replacement = "",
#                                   x = all_filters)

            # max_filter <- which.max(filter_numbers)
            #
            # last_filter <- all_filters[max_filter]


            print(filter_vals)

             }

        })

        #

        # incrementor for button clicks
        filter_control <- shiny::reactiveValues(value = 1)


        # data frame to collect filter statements
        filter_statements <- reactiveValues()

        filter_statements$df <- data.frame(
            "statement" = character(0),
            stringsAsFactors = FALSE
        )

        shiny::observeEvent(input$addbutton, {

            shiny::callModule(module_server_box_str_filter,
                              "textselect",
                              selector = "#placeholder",
                              # selector = "p",
                              actionbtn = filter_control$value)






#
            filter_control$value <- filter_control$value + 1

        })








        shiny::observeEvent(input[[shiny::NS(paste0("filter",
                                                    filter_control$value-1),
                                             "strfilter")]], {

                                                 # print(input[[shiny::NS(paste0("filter",
                                                 #                               filter_control$value-1),
                                                 #                        "strfilter")]])

                                                 # filter_statements$df[filter_control$value - 1,
                                                 #                      "statements"] <- input[[shiny::NS(paste0("filter",
                                                 #                                                               filter_control$value -1),
                                                 #                                                        "strfilter ")]]
                                             })




        observeEvent(input$removebutton, {

            # print(input$`filter1-strfilter`)
            # print(input$`filter2-strfilter`)

            # active_filters <- check_active_filters(allinputs = AllInputs())

            btn <- filter_control$value
            removeUI(
                selector = paste0("#div-filter",filter_control$value - 1)
            )


            filter_control$value <- filter_control$value - 1

            if(filter_control$value < 1){
                filter_control$value <- 1
            }

        })




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

