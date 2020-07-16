#' Data cleanr
#'
#' @param data_set dataframe
#'
#' @return idxs of selected data
#' @export
#'
#'
datacleanr <- function(dataset){


    # set tz too UTC for plotly

    old_tz <- Sys.getenv("TZ")
    Sys.setenv(TZ = "UTC")


    df_name <- deparse(substitute(dataset))

    # Panel texts ----------------------------

    text_grouping_side_panel <- shiny::tagList(shiny::h4(shiny::tags$strong("Overview")),
                                               shiny::br(),
                                               shiny::p("Select grouping variables for subsequent cleaning."),
                                               shiny::br(),
                                               shiny::p("Choose if you want to view an overview with or without grouping."))

    text_filtering_side_panel <- shiny::tagList(
        # shiny::p(
        # shiny::tags$b("Add/Remove"),
        # "text boxes and add unquoted filter statements."),
        shiny::p("Use", shiny::tags$b("single quotes"), "for values of character/factor variables."),
        shiny::p("Click", shiny::tags$b("'Apply Filter'"), "when you're ready."),
        shiny::tags$p("For example, valid statements for filtering",
                      shiny::tags$b("iris"),
                      "are:"),
        shiny::tags$ol(
            shiny::tags$li(shiny::tags$small("Species == 'setosa'")),
            shiny::tags$li(shiny::tags$small("Species %in% c('setosa','versicolor')")))


    )





    # define layout --------------------------

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
                                                                         icon = shiny::icon("rocket"),
                                                                         class = "btn-info"),

                                                     # shiny::textOutput("checkworked"),





                                                     width = 3
                                                 ),

                                                 mainPanel = shiny::mainPanel(


                                                     # Diagnostics################
                                                     # textOutput('show_inputs'),###

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

                                                     shiny::h4(shiny::tags$strong("Filter Statements")),



                                                     shiny::actionButton("addbutton",
                                                                         "Add",
                                                                         icon = shiny::icon("plus-circle")),
                                                     shiny::actionButton("removebutton",
                                                                         "Remove",
                                                                         icon = shiny::icon("trash")),

                                                     shiny::br(),
                                                     shiny::br(),


                                                     text_filtering_side_panel,


                                                     shiny::br(),


                                                     module_ui_df_filter("check"),

                                                     module_ui_apply_reset("appfilt"),



                                                     width = 3
                                                 ),

                                                 mainPanel = shiny::mainPanel(


                                                     shiny::textOutput('show_inputs'),###
                                                     shiny::verbatimTextOutput("outDF"),

                                                     shiny::h2("Filter me!"),
                                                     # MODULE UI FOR VARIABLE 1
                                                     module_ui_filter_str(1),
                                                     shiny::tags$div(id = 'placeholder')


                                                 )
                                             )



                             ),
                             # TAB VIS -----------------
                             shiny::tabPanel("Visualization",
                                             value = "visu",
                                             icon = shiny::icon("chart-area"),

                                             shiny::sidebarLayout(
                                                 sidebarPanel = shiny::sidebarPanel(width = 4,
                                                                                    module_ui_group_selector_table("dtgrouprow")),
                                                 mainPanel = shiny::mainPanel(width = 8,
                                                                              module_ui_plot_selectorcontrols("selectors"),
                                                                              module_ui_plot_selectable("plot"),
                                                                              module_ui_plot_annotation_table("annotator"))
                                             )

                             )


                             ,
                             # TAB EXTRACT -------------
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


        # suppress plotly warnings, etc.
        # options(warn = -1)


        # DIAGNOSTICS ----------------------

        AllInputs <- shiny::reactive({
            x <- unlist(shiny::reactiveValuesToList(input))
            paste(names(x),
                  x)


        })

        output$show_inputs <- shiny::renderText({
            AllInputs()
        })



        # grouping ---------------------------


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


            # add .key ref for plot
            df$.dcrkey <- seq_len(nrow(df))


            return(df)
        })

        shiny::observe({print(gvar())})
        shiny::observe({print(input$`grouptick-checkbox`)})



        # summary -------------------

        # handle summary operations when go button is hit
        shiny::observeEvent(input$gobutton, {


            shiny::callModule(module_server_summary,
                              "summary",

                              df =  {if(!is.null(datareactive()) &&
                                        !input$`grouptick-checkbox`){

                                  dplyr::ungroup(datareactive())

                              } else if(!is.null(datareactive()) &&
                                        input$`grouptick-checkbox`){

                                  datareactive()
                              }},
                              df_label = df_name)


        })
        # filter ------------------

        # CREATE EMPTY DATAFRAME
        add.filter <- shiny::reactiveValues()

        add.filter$df <- data.frame(
            "filter" = character(0),
            stringsAsFactors = FALSE


        )

        shiny::observe({




            req(input$gobutton)



            shiny::callModule(module_server_filter_str, 1)

            ## SAVE INPUTS FROM 1 INTO DATAFRAME
            shiny::observeEvent(input[[NS(1, "filter")]], {
                add.filter$df[1, 1] <- input[[NS(1, "filter")]]
            })



            btn <- shiny::reactiveValues(value = 1)

            # ADD VARIABLES

            shiny::observeEvent(input$addbutton, {

                # EACH TIME THE USER CLICKS, ADD 1 TO BUTTON VALUE
                btn$value <- btn$value + 1

                ## WHEN WE USE btn$value DIRECTLY WE LOSE REACTIVITY
                ## PASSING IT TO btn.temp AND USING btn.tmp WORKS (SOMEHOW)
                btn.tmp <- btn$value

                # CALL MODULE NUMBER params$btn
                shiny::callModule(module_server_filter_str, btn.tmp)

                # INSERT MODULE UI
                shiny::insertUI(
                    selector = '#placeholder',
                    where = "beforeEnd",
                    ui = module_ui_filter_str(btn.tmp)
                )

                ## SAVE INPUTS FROM NUMBER params$btn INTO DATAFRAME
                shiny::observeEvent(input[[shiny::NS(btn.tmp, "filter")]], {
                    add.filter$df[btn.tmp, 1] <- input[[NS(btn.tmp, "filter")]]



                })

            })

            # REMOVE VARIABLES

            shiny::observeEvent(input$removebutton, {

                # REMOVE LAST LINE FROM DATAFRAME
                add.filter$df <- add.filter$df[-btn$value, , drop = FALSE]
                # print(str(add.filter$df))

                # REMOVE LAST LINE MODULE UI
                shiny::removeUI(
                    ## pass in appropriate div id
                    selector = paste0('#filt', btn$value)
                )

                # SUBTRACT 1 FROM BUTTON VALUE

                if(btn$value > 1){
                    btn$value <- btn$value - 1
                } else {
                    btn$value <- 0
                }

            })


            # OUTPUT DATAFRAME
            output$outDF <- shiny::renderPrint({
                print(add.filter$df)
            })

        })


        filtered_data <- shiny::reactiveValues(df = NULL)
        # apply filtering
        shiny::observe({
            filtered_data$df <- shiny::callModule(module = module_server_df_filter,
                                                  id = "check",
                                                  df = datareactive(),
                                                  statements = add.filter$df$filter)

            print(paste("filter output in app is:", nrow(filtered_data$df)))


        })




        # set up variables for vis panel
        plot_df <- shiny::reactiveValues(df = NULL)
        selected_row <- shiny::reactiveValues(group_row = NULL)
        selector_vals <- NULL


        plot_df$df <- shiny::callModule(module = module_server_apply_reset,
                                        id = "appfilt",
                                        df_filtered = filtered_data,
                                        df_original = datareactive)

        # handle group/ungroup after hitting start button
        # to supply plot_df (e.g. for viz selection table)
        # this is necessary, as changing grouping levels
        # would not affect plot_df, as it is already supplied in event handler
        # below (handle table + variable inputs; observevent gobutton)
        shiny::observeEvent(input$gobutton, {

            plot_df$df$data <- datareactive()

        })


        # handle table + variable inputs
        shiny::observeEvent({
            input$gobutton
            input$`appfilt-applyfilter`
            input$`appfilt-applyreset`
            1},
            {

                # provide data set in case filtering is skipped
                if(is.null(plot_df$df$data)){

                    plot_df$df$data <- datareactive()
                }

                if(!is.null(plot_df$df$data)){

                    shiny::callModule(module_server_group_selector_table,
                                      id = "dtgrouprow",
                                      df = plot_df,
                                      df_label = df_name)

                    selector_vals <<- shiny::callModule(module_server_plot_selectorcontrols,
                                                        "selectors",
                                                        plot_df)
                }
            })

        shiny::observeEvent(
            {input[["selectors-startscatter"]]
                selector_vals
                input[["dtgrouprow-grouptable_rows_selected"]]
                1
            }
            , {

                req(input[["selectors-startscatter"]])

                selected_row$group_row <- input$`dtgrouprow-grouptable_rows_selected`


                if(!is.null(plot_df$df$data)){

                    shiny::callModule(module_server_plot_selectable,
                                      id = "plot",
                                      df = plot_df,
                                      group_row = selected_row,
                                      selector_inputs = selector_vals,
                                      sel_points = shiny::isolate(selected_data))



                }

            })







        selected_data <- shiny::reactiveValues(df = data.frame(keys = character(0), selection_count = integer(0)))

        # handle clicks
        shiny::observeEvent({plotly::event_data("plotly_click", priority = "event", source = "scatterselect")}, {

            shiny::req(input[["selectors-startscatter"]])

            clicked <- plotly::event_data("plotly_click",
                                          source = "scatterselect",
                                          priority = "event")

            if(nrow(selected_data$df) > 0 & nrow(clicked) > 0){
                new <- data.frame(keys = as.character(clicked$customdata),
                                  selection_count = max(selected_data$df$selection_count) + 1)

                if(any(new$keys %in% selected_data$df$keys)){
                    new <- new[!{new$keys %in% selected_data$df$keys}, ]
                }

                selected_data$df <- rbind(selected_data$df, new)

            } else {
                new <- data.frame(keys = as.character(clicked$customdata),
                                  selection_count = 1)
                selected_data$df <- new
            }
            print(selected_data$df)
        })


        # handle selections
        shiny::observeEvent({plotly::event_data("plotly_selected", priority = "event", source = "scatterselect")}, {

            shiny::req(input[["selectors-startscatter"]])


            print("selected!")

            # selected <- shiny::reactiveVal()
            selected <- plotly::event_data("plotly_selected",
                                           source = "scatterselect",
                                           priority = "event")

            if(length(selected)>0){

                if(nrow(selected_data$df) > 0 & nrow(selected) > 0){
                    new <- data.frame(keys = as.character(selected$customdata),
                                      selection_count = max(selected_data$df$selection_count) + 1)

                    if(any(new$keys %in% selected_data$df$keys)){
                        new <- new[!{new$keys %in% selected_data$df$keys}, ]
                    }
                    selected_data$df <- rbind(selected_data$df, new)
                } else {
                    new <- data.frame(keys = as.character(selected$customdata),
                                      selection_count = 1)
                    selected_data$df <- new
                }

                print(selected_data$df)
            }

        })

        # clear on dbl click
        shiny::observeEvent({plotly::event_data("plotly_doubleclick", source = "scatterselect", priority = "event")
            plotly::event_data("plotly_deselect", source = "scatterselect", priority = "event")
            1}, {


                req(nrow(selected_data$df) > 0)
                print("data cleared on dbl click")

                drop_ind <- which(selected_data$df$selection_count == max(selected_data$df$selection_count, na.rm = TRUE))
                selected_data$df <- selected_data$df[ -drop_ind, ]
            })






        # insert editable table for annotations
        shiny::observe({


            # shiny::observeEvent(selected_data, {
            shiny::req(input[["selectors-startscatter"]])

            annotations <- shiny::callModule(module_server_plot_annotation_table,
                                             "annotator",
                                             df = plot_df,
                                             sel_points = selected_data)

        })





        # OUTLIST
        # shiny::observeEvent(input[["selectors-startscatter"]], {
        #     outs <- outputOptions(output)
        #     print(outs)
        #     print("ya")
        #     lapply(names(outs), function(name) {
        #         outputOptions(output, name, suspendWhenHidden = FALSE)
        #     })
        #
        # })





        # END ---------------------------
        observeEvent(input$done, {

            Sys.setenv(TZ = old_tz)
            stopApp("Done")
        })
        observeEvent(input$cancel, {

            stopApp(NULL)
            Sys.setenv(TZ = old_tz)
        })



    }





    shiny::runGadget(ui,
                     server,
                     viewer = shiny::browserViewer()
                     # viewer = shiny::dialogViewer(dialogName = "Data Cleaning - A. Hurley",
                     #                              width = 1200,
                     #                              height = 800)

    )
}



# pick_points(mtcars, ~wt, ~mpg)

