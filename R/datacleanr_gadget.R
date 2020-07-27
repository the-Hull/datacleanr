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




# UI ----------------------------------------------------------------------
# //-------------------------------------------------------------------------




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
                                                     width = 3
                                                 ), #/sidebarPanel

                                                 mainPanel = shiny::mainPanel(
                                                     module_ui_summary(id = "summary")
                                                 ) #/mainPanel
                                             ) #/sidebarLayout


                             ), # /tabPanel

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
                                                     shiny::tagList( shiny::actionButton(inputId = "apply_filter",
                                                                                         label = "Apply",
                                                                                         icon = shiny::icon("check-double"),
                                                                                         class = "btn-info"),
                                                                     shiny::actionButton(inputId = "reset_filter",
                                                                                         label = "Reset",
                                                                                         icon = shiny::icon("undo"),
                                                                                         class = "btn-danger")),


                                                     width = 3
                                                 ), # /sidebarLayout

                                                 mainPanel = shiny::mainPanel(
                                                     shiny::textOutput('show_inputs'),###
                                                     shiny::verbatimTextOutput("outDF"),
                                                     shiny::h2("Filtering statements"),
                                                     module_ui_filter_str(1),
                                                     shiny::tags$div(id = 'placeholder')
                                                 ) #/mainPanel
                                             ) #/sidebarLayout
                             ), #/tabPanel

                             # TAB VIS -----------------
                             shiny::tabPanel("Visualization",
                                             value = "visu",
                                             icon = shiny::icon("chart-area"),
                                             shiny::sidebarLayout(
                                                 sidebarPanel = shiny::sidebarPanel(width = 4,
                                                                                    module_ui_group_selector_table('df'),
                                                                                    shiny::p("Placeholder")),
                                                 mainPanel = shiny::mainPanel(width = 8,
                                                                              module_ui_plot_selectorcontrols("selectors"),
                                                                              module_ui_plot_selectable("plot"),
                                                                              module_ui_plot_annotation_table("annotator"))
                                             ) #/sidebarLayout
                             ) #/tabPanel


                             ,
                             # TAB EXTRACT -------------
                             shiny::tabPanel("Extraction",
                                             value = "extract",
                                             icon = shiny::icon("file-export")), #/tabPanel


                             inputs = list(miniUI::miniTitleBarButton("done",
                                                                      "Done",
                                                                      primary = TRUE),

                                           miniUI::miniTitleBarCancelButton(inputId = "cancel",
                                                                            label = "Cancel",
                                                                            primary = FALSE)), #inputs




                             shiny::tags$style(type = "text/css", "body {padding-top: 70px;}"),
                             position = "fixed-top"


        ) #/navbarPageWithInputs

# // ----------------------------------------------------------------------
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




        # Set-up ------------------------------------------------------------------


        # handle initialization
        dataset$.dcrkey <- seq_len(nrow(dataset))




        # stores data used in app
        datareactive <- shiny::reactiveVal()

        # stores original data after hitting start button
        recover_data <- shiny::reactiveVal()

        # tracks which version of data to pass to datareactive
        action_tracking <- shiny::reactiveValues(plot_start = NULL,
                                                 action_tracking = NULL)
                                                 # applied_filters_button = NULL,
                                                 # reset_filters_button = NULL)

        # reactive inputs for plot variable selection
        # selector_vals <- shiny::reactiveValues()


        # tracker for plot initialization
        start_scatter <- shiny::reactiveVal()





# // ----------------------------------------------------------------------
        # GROUPING ---------------------------

        # get grouping
        gvar <- shiny::callModule(module_server_group_select,
                                  id = "group")
        output$gvar <- shiny::reactive({gvar()})
        # check-box for grouping
        shiny::callModule(module = module_server_checkbox,
                          "grouptick",
                          text = "Use grouping for summary")

        shiny::outputOptions(output, "gvar", suspendWhenHidden = FALSE)

        #  START + SUMMARY -------------------

        # handle summary operations when go button is hit
        shiny::observeEvent(input$gobutton, {

            # handle actions

            df <- apply_data_set_up(df = dataset, gvar())
            df$.dcrindex <- dplyr::group_indices(df)

            datareactive(df)
            recover_data(df)

            print(paste("Is DF Grouped??", dplyr::is.grouped_df(datareactive())))

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


            # clean-up




        })


# // ----------------------------------------------------------------------


        # FILTER STATEMENTS ------------------

        # CREATE EMPTY DATAFRAME
        add.filter <- shiny::reactiveValues()

        add.filter$df <- data.frame(
            "filter" = character(0),
            stringsAsFactors = FALSE)

        btn <- shiny::reactiveValues(value = 1)


        shiny::observe({

                shiny::callModule(module_server_filter_str, id = 1)

                ## SAVE INPUTS FROM 1 INTO DATAFRAME
                shiny::observeEvent(input[[NS(1, "filter")]], {
                    add.filter$df[1, 1] <- input[[NS(1, "filter")]]
                })

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

                    ## SAVE INPUTS FROM NUMBER COUNTER BTN INTO DATAFRAME
                    shiny::observeEvent(input[[shiny::NS(btn.tmp, "filter")]], {
                        add.filter$df[btn.tmp, 1] <- input[[NS(btn.tmp, "filter")]]
                    })

                    print(btn$value)


                })

                # REMOVE VARIABLES

                shiny::observeEvent(input$removebutton, {

                    # REMOVE LAST LINE FROM DATAFRAME
                    add.filter$df <- add.filter$df[-btn$value, , drop = FALSE]
                    # print(str(add.filter$df))

                    # REMOVE LAST LINE MODULE UI
                    shiny::removeUI(
                        ## pass in appropriate div id
                        selector = paste0('#filt', btn$value))

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




# FILTER PREVIEW STRING ---------------------------------------------------


        shiny::observe({

            shiny::validate(shiny::need(add.filter,
                                       label = "add filter"))
            shiny::validate(shiny::need(input$gobutton,
                                       label = "StartButton"))

            shiny::callModule(module = module_server_df_filter,
                                                  id = "check",
                                                  df = shiny::isolate(recover_data()),
                                                  statements = add.filter$df$filter)
        })

        # FILTER Apply/Undo  -------------------------------------------------------

        #apply
        shiny::observeEvent(input$apply_filter, {
            shiny::validate(shiny::need(add.filter,
                                       label = "add filter"))
            shiny::validate(shiny::need(input$gobutton,
                                       label = "StartButton"))

            df <- try({checked_filter(recover_data(),
                                      add.filter$df$filter)})

            if(any(df$succeeded)){
                datareactive(df$filtered_df)
            }
            rm(df)
        })

        # reset filtering
        shiny::observeEvent(input$reset_filter, {
            shiny::validate(shiny::need(add.filter,
                                       label = "add filter"))
            shiny::validate(shiny::need(input$gobutton,
                                       label = "StartButton"))

            datareactive(recover_data())
            print(paste("AFTER-RESET: filter output in app is:", nrow(datareactive())))

            # reset filters

            add.filter$df <- add.filter$df[0,,drop = FALSE]
            # purrr::walk(seq_len(btn$value),
            #        function(i){
            #
            #            id <- paste0(i, "-filter")
            #            print(id)
            #            shiny::updateTextInput(session = session,
            #                                   inputId = id,
            #                                   value = "")
            #            }
            # )

            print("right here")

            sapply(seq_len(btn$value),
                   function(i){
                       shiny::removeUI(
                           ## pass in appropriate div id
                           selector = paste0('#filt', i))
                       }
            )

            btn$value <- 0
        })




# // ----------------------------------------------------------------------


# GROUPTABLE --------------------------------------------------------------

        shiny::observe({
            if(!is.null(datareactive() )){
            shiny::callModule(module_server_group_selector_table,
                              id = "df",
                              df = datareactive,
                              df_label = df_name)
            }

        })





# // ----------------------------------------------------------------------


        # PLOT CONTROLS --------------
        # handle data for plotting after gobutton + filtering
        shiny::observe({

            shiny::validate(shiny::need(datareactive,
                                       label = "datareactive"))
                    # selector_vals <<-
                        shiny::callModule(module_server_plot_selectorcontrols,
                                                        "selectors",
                                                        datareactive)
            })



        selector_vals <- list(xvar = shiny::reactive(input$`selectors-xvar`),
                              yvar = shiny::reactive(input$`selectors-yvar`),
                              zvar = shiny::reactive(input$`selectors-zvar`),
                              startscatter = shiny::reactive(input$`selectors-startscatter`))


        shiny::observeEvent({
            selector_vals[[1]]()
            selector_vals[[2]]()
            selector_vals[[3]]()
            }, {

            shiny::validate(shiny::need(shiny::isolate(selector_vals),
                                        label = "control vals"))
            action_tracking$plot_start <- FALSE
            action_tracking$controls <- TRUE

            print("used controls - disable selecting")
        })


        ## PLOTTING -----------------
        shiny::observeEvent(input[["selectors-startscatter"]],
        # shiny::observe(
            {


                action_tracking$plot_start <- TRUE
                action_tracking$controls <- FALSE
                print("pressed start - enable selecting")


            shiny::validate(shiny::need(datareactive, label = "datareactive"))
            shiny::validate(shiny::need(input[["selectors-startscatter"]], label = "PlotStartbutton"))

                shiny::callModule(module_server_plot_selectable,
                                  id = "plot",
                                  df = datareactive,
                                  selector_inputs = shiny::isolate(selector_vals),
                                  sel_points = shiny::isolate(selected_data))

            }) #/observe


        selected_data <- shiny::reactiveValues(
            df = data.frame(keys = integer(0),
                            selection_count = integer(0),
                            stringsAsFactors = FALSE)
        )


        # PLOT DATA SELECTION ---------------

         # handle clicks
        shiny::observeEvent({plotly::event_data("plotly_click", priority = "event", source = "scatterselect")}, {

            # shiny::req(input[["selectors-startscatter"]])

            # shiny::validate(shiny::need(input[["selectors-startscatter"]],
            #                             label = "PlotStarter"))
            shiny::validate(shiny::need(action_tracking$plot_start,
                                        label = "PlotStarter"))

            clicked <- plotly::event_data("plotly_click",
                                          source = "scatterselect",
                                          priority = "event")

            if(nrow(selected_data$df) > 0 & nrow(clicked) > 0){
                new <- data.frame(keys = as.integer(clicked$customdata),
                                  selection_count = max(selected_data$df$selection_count) + 1,
                                  stringsAsFactors = FALSE)

                if(any(new$keys %in% selected_data$df$keys)){
                    new <- new[!{new$keys %in% selected_data$df$keys}, ]
                }

                selected_data$df <- rbind(selected_data$df, new)

            } else {
                new <- data.frame(keys = as.integer(clicked$customdata),
                                  selection_count = 1,
                                  stringsAsFactors = FALSE)
                selected_data$df <- new
            }
            print(paste("orig selection:"))
            print(selected_data$df)
        })


        # handle selections
        shiny::observeEvent({plotly::event_data("plotly_selected", priority = "event", source = "scatterselect")}, {

            # shiny::validate(shiny::need(input[["selectors-startscatter"]],
            #                             label = "PlotStarter"))
            shiny::validate(shiny::need(action_tracking$plot_start,
                                        label = "PlotStarter"))

            print("selected!")

            # selected <- shiny::reactiveVal()
            selected <- plotly::event_data("plotly_selected",
                                           source = "scatterselect",
                                           priority = "event")

            if(length(selected)>0){

                if(nrow(selected_data$df) > 0 & nrow(selected) > 0){
                    new <- data.frame(keys = as.integer(selected$customdata),
                                      selection_count = max(selected_data$df$selection_count) + 1,
                                      stringsAsFactors = FALSE)

                    if(any(new$keys %in% selected_data$df$keys)){
                        new <- new[!{new$keys %in% selected_data$df$keys}, ]
                    }
                    selected_data$df <- rbind(selected_data$df, new)
                } else {
                    new <- data.frame(keys = as.integer(selected$customdata),
                                      selection_count = 1,
                                      stringsAsFactors = FALSE)
                    selected_data$df <- new
                }

                print(paste("orig selection:"))
                print(selected_data$df)
            }

        })

        # clear on dbl click
        shiny::observeEvent(
        # shiny::observeEvent({
            # plotly::event_data("plotly_doubleclick", source = "scatterselect", priority = "event")
            plotly::event_data("plotly_deselect", source = "scatterselect", priority = "event")
            , {


                shiny::validate(shiny::need(nrow(selected_data$df) > 0,
                                            label = "need selected data"))
                print("data cleared on dbl click")

                drop_ind <- which(selected_data$df$selection_count == max(selected_data$df$selection_count, na.rm = TRUE))
                selected_data$df <- selected_data$df[ -drop_ind, ]
            })




# PLOT ADD TRACES ---------------------------------------------------------




        old_keys <- shiny::reactiveVal()

        max_id_original_traces <- shiny::reactive({dplyr::n_groups(datareactive()) - 1})
        shiny::observeEvent(plotly::event_data("plotly_click",
                                               source = "scatterselect",
                                               priority = "event"),
                            {
                                ok <- handle_add_traces(sp = selected_data,
                                                        dframe = datareactive,
                                                        ok = old_keys,
                                                        selectors = selector_vals,
                                                        source = "plot-scatterselect",
                                                        session = session)
                                old_keys(ok())
                            })


        shiny::observeEvent(plotly::event_data("plotly_selected",
                                               source = "scatterselect",
                                               priority = "event"),
                            {
                                ok <- handle_add_traces(sp = selected_data,
                                                        dframe = datareactive,
                                                        ok = old_keys,
                                                        selectors = selector_vals,
                                                        source = "plot-scatterselect",
                                                        session = session)
                                old_keys(ok())
                            })



        shiny::observeEvent(plotly::event_data(c("plotly_deselect"),
                                               source = "scatterselect",
                                               priority = "event"),
                            {
            shiny::validate(shiny::need(input$`plot-tracemap`,
                                        label = "need tracepam"))

            traces <- matrix(input$`plot-tracemap`, ncol = 2, byrow = TRUE)
            indices <-  as.integer(traces[ as.integer(traces[, 2]) > max_id_original_traces(), 2])

            print(paste("indices are:", indices))

            if(length(indices)>0){
                plotly::plotlyProxy("plot-scatterselect", session) %>%
                    plotly::plotlyProxyInvoke(
                        "deleteTraces",
                        max(indices)
                    )
                print("removed trace!!")
            }
            old_keys(NULL)
            print(traces)
        })


# // ----------------------------------------------------------------------


# ANNOTATION TABLE --------------------------------------------------------




        # insert editable table for annotations
        # shiny::observe({
        shiny::observeEvent(selected_data$df, {


            # shiny::observeEvent(selected_data, {
            shiny::validate(shiny::need(input[["selectors-startscatter"]],
                                        label = "PlotStarter"))

            shiny::validate(shiny::need(selected_data,
                                        label = "selected data"))

            # annotations <-
             shiny::callModule(module_server_plot_annotation_table,
                                             "annotator",
                                             df = datareactive,
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

