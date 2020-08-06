#' datacleanr server function
#' @param dataset data.frame, tibble or data.table that needs cleaning
#' @param df_name character, name of dataset passed into shiny app
#'
#' @param input,output,session standard \code{shiny} boilerplate
datacleanr_server <- function(input, output, session, dataset, df_name){


    ns <- session$ns

    old_tz <- Sys.getenv("TZ")
    Sys.setenv(TZ = "UTC")


    # suppress plotly warnings, etc.
    # options(warn = -1)


    # Help Texts --------------------------------------------------------------

    text_filtering_side_panel <- shiny::tagList(
        # shiny::p(
        # shiny::tags$b("Add/Remove"),
        # "text boxes and add unquoted filter statements."),
        shiny::p(shiny::tags$b("Add/Remove"),
                 "filter statements as necessary. These are passed to",
                 shiny::tags$b("base::subset()"), "."),
        shiny::p("Use", shiny::tags$b("single quotes"), "for values of character/factor variables."),
        shiny::tags$p("For example, valid statements for filtering",
                      shiny::tags$b("iris"),
                      "are:"),
        shiny::tags$ol(
            shiny::tags$li(shiny::tags$small("Species == 'setosa'")),
            shiny::tags$li(shiny::tags$small("Species %in% c('setosa','versicolor')"))),

        shiny::br(),
        shiny::p("Click",
                 shiny::tags$b("'Apply'"),
                 "when you're ready, and",
                 shiny::tags$b("'Reset'"),
                 "to start from scratch."))


    text_annotate_side_panel <- shiny::tagList(
        shiny::p("After selecting points by",
                 shiny::tags$b("clicking/lasso-selecting"),
                 "the",
                 shiny::tags$b("last selection"),
                 "can be annotated with a text label.",
                 shiny::br(),
                 "These labels are collected and provided as an additional column",
                 shiny::tags$b("'.annotation'"),
                 "in the table to the right and outputted via the",
                 shiny::tags$b("Extraction Tab.")),
        shiny::br(),

        shiny::p("The annotation can be updated or removed by deleting all characters in the input box and clicking the button again.",
                 "Note, that the most-recent selection can be deleted with a ",
                 shiny::tags$b("double-click on the plot."),
                 "This also removes the respective annotations."))


    text_plot_main_panel <- shiny::tagList(
        shiny::p("Select at least ",
                 shiny::tags$b("X and Y"),
                 " variables and click",
                 shiny::tags$b("'Plot!'."),
                 "The",
                 shiny::tags$b("Z"),
                 "variable adjusts point size.",
                 "The legend entries correspond with the rownumbers on the table to the left.",
                 "Individual items (i.e. groups) can be",
                 shiny::tags$b("hidden"),
                 "by clicking on the legend.",
                 "Double-clicking one item hides all others (speeding up selections), and a subsequent double-click displays all data."),
        shiny::p("Note, that",
                 shiny::tags$b("'Plot!'"),
                 "must be clicked after any variable input has been",
                 shiny::tags$b("clicked or changed."),
                 shiny::br(),
                 shiny::br(),
                 "To mark and exclude outliers, ",
                 shiny::tags$b("click or lasso/box select"),
                 "individual points.",
                 shiny::tags$b("Double-click"),
                 "on the plot area to remove the last selection.",
                 shiny::br(),
                 "The plot's",
                 shiny::tags$b("control bar"),
                 "allows to",
                 shiny::tags$b("zoom and reset views")),
        shiny::br(),

        shiny::p("Selected points appear in the",
                 shiny::tags$b(" table below"),
                 "and can be annotated with the tool to the left."))

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
                                             controls = NULL)

    # stores recent data selection (used when filtering is reset)
    selected_data_recovery <- shiny::reactiveVal()

    # used for extraction tab
    filter_string <- shiny::reactiveVal()

    # holds plotly selection data
    selected_data <- shiny::reactiveValues(
        df = data.frame(keys = integer(0),
                        selection_count = integer(0),
                        .annotation = character(0),
                        stringsAsFactors = FALSE)
    )




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
        shiny::observeEvent(input[[shiny::NS(1, "filter")]], {
            add.filter$df[1, 1] <- input[[shiny::NS(1, "filter")]]
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
                selector = paste0("#", ns("placeholder")),
                where = "beforeEnd",
                ui = module_ui_filter_str(ns(btn.tmp))
            )


            ## SAVE INPUTS FROM NUMBER COUNTER BTN INTO DATAFRAME
            shiny::observeEvent(input[[shiny::NS(btn.tmp, "filter")]], {
                add.filter$df[btn.tmp, 1] <- input[[shiny::NS(btn.tmp, "filter")]]
            })



        })

        # REMOVE VARIABLES

        shiny::observeEvent(input$removebutton, {

            # REMOVE LAST LINE FROM DATAFRAME
            add.filter$df <- add.filter$df[-btn$value, , drop = FALSE]
            # print(str(add.filter$df))


            print(paste0(ns(btn$value), "-filt"))
            # REMOVE LAST LINE MODULE UI
            shiny::removeUI(
                ## pass in appropriate div id
                selector = paste0("#", ns(btn$value), "-filt"))

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

    # # used for extraction tab
    # filter_string <- shiny::reactiveVal()
    # selected_data_recovery <- shiny::reactiveVal()

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
            filter_string(df$statement_strings)
        }
        rm(df)

        ## Logic to handle removal of selected data in plotly
        # id points in selection now missing in data
        if(nrow(selected_data$df) > 0){

            absent_selection <- selected_data$df$keys %nin% datareactive()$.dcrkey

            if(!is.null(selected_data_recovery())){
                selected_data_recovery(rbind(selected_data$df[absent_selection, ], selected_data_recovery()))
            } else {
                selected_data_recovery(selected_data$df[absent_selection, ])
            }

            # adjust selection
            selected_data$df <- selected_data$df[!absent_selection, ]

        }
        if(!is.null(selector_vals$startscatter)){

            selector_vals$startscatter <- selector_vals$startscatter + 1
        }

    })


    # reset filtering
    shiny::observeEvent(input$reset_filter, {
        shiny::validate(shiny::need(add.filter,
                                    label = "add filter"))
        shiny::validate(shiny::need(input$gobutton,
                                    label = "StartButton"))

        datareactive(recover_data())

        # reset filters
        add.filter$df <- add.filter$df[0,,drop = FALSE]


        sapply(seq_len(btn$value),
               function(i){
                   shiny::removeUI(
                       ## pass in appropriate div id
                       selector = paste0("#", ns(i), "-filt"))
               }
        )

        btn$value <- 0
        filter_string(NULL)

        # reset selected data
        if(!is.null(selected_data_recovery())){
            # adjust selection
            selected_data$df <- rbind(selected_data$df,
                                      selected_data_recovery())
        }

        ## Logic to handle removal of selected data in plotly
        selected_data_recovery(NULL)

        if(!is.null(selector_vals$startscatter)){

            selector_vals$startscatter <- selector_vals$startscatter + 1

        }


    })




    # // ----------------------------------------------------------------------


    # GROUPTABLES --------------------------------------------------------------

    shiny::observe({
        if(!is.null(datareactive() )){
            # on filter tab
            shiny::callModule(module_server_group_selector_table,
                              id = "df-filter-tab",
                              df = datareactive,
                              df_label = df_name)
            # on viz tab
            shiny::callModule(module_server_group_selector_table,
                              id = "df",
                              df = datareactive,
                              df_label = df_name)
        }

    })





    # // ----------------------------------------------------------------------


    selector_vals <- shiny::reactiveValues()

    # PLOT CONTROLS --------------
    # handle data for plotting after gobutton + filtering
    # shiny::observe({
    shiny::observeEvent({
        input$gobutton
        # input$apply_filter
    },
    {

        shiny::validate(shiny::need(datareactive,
                                    label = "datareactive"))

        shiny::callModule(module_server_plot_selectorcontrols,
                          "selectors",
                          shiny::isolate(datareactive()))

    })


    # selector_vals <- list(xvar = shiny::reactive(input$`selectors-xvar`),
    #                       yvar = shiny::reactive(input$`selectors-yvar`),
    #                       zvar = shiny::reactive(input$`selectors-zvar`),
    #                       startscatter = shiny::reactiveVal(input$`selectors-startscatter`))

    shiny::observe({

        selector_vals$xvar <-  input$`selectors-xvar`
        selector_vals$yvar <- input$`selectors-yvar`
        selector_vals$zvar <- input$`selectors-zvar`
        selector_vals$startscatter <- input$`selectors-startscatter`


    })




    shiny::observeEvent({
        selector_vals$xvar
        selector_vals$yvar
        selector_vals$zvar
    }, {
        # shiny::observeEvent({
        #     selector_vals[[1]]()
        #     selector_vals[[2]]()
        #     selector_vals[[3]]()
        # }, {

        shiny::validate(shiny::need(shiny::isolate(selector_vals),
                                    label = "control vals"))
        action_tracking$plot_start <- FALSE
        action_tracking$controls <- TRUE

        print("used controls - disable selecting")
    })


    ## PLOTTING -----------------
    # shiny::observeEvent(input[["selectors-startscatter"]],
    shiny::observeEvent(selector_vals$startscatter,
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


    # selected_data <- shiny::reactiveValues(
    #     df = data.frame(keys = integer(0),
    #                     selection_count = integer(0),
    #                     .annotation = character(0),
    #                     stringsAsFactors = FALSE)
    # )


    # PLOT DATA SELECTION ---------------


    # handle clicks


    # handle selections
    shiny::observeEvent({
        plotly::event_data("plotly_selected", priority = "event", source = "scatterselect")}, {
            print("selected!")

            shiny::validate(shiny::need(action_tracking$plot_start,
                                        label = "PlotStarter"))

            selected <- plotly::event_data("plotly_selected",
                                           source = "scatterselect",
                                           priority = "event")

            selected_data$df <- handle_outlier_selection(sel_data_old = selected_data$df,
                                                         sel_data_new = selected)


        })


    shiny::observeEvent({
        plotly::event_data("plotly_click", priority = "event", source = "scatterselect")}, {
            print("clicked!")

            shiny::validate(shiny::need(action_tracking$plot_start,
                                        label = "PlotStarter"))

            clicked <- plotly::event_data("plotly_click",
                                          source = "scatterselect",
                                          priority = "event")

            selected_data$df <- handle_outlier_selection(sel_data_old = selected_data$df,
                                                         sel_data_new = clicked)


        })



    # # handle clicks
    # shiny::observeEvent({plotly::event_data("plotly_click", priority = "event", source = "scatterselect")}, {
    #
    #     # shiny::req(input[["selectors-startscatter"]])
    #
    #     # shiny::validate(shiny::need(input[["selectors-startscatter"]],
    #     #                             label = "PlotStarter"))
    #     shiny::validate(shiny::need(action_tracking$plot_start,
    #                                 label = "PlotStarter"))
    #
    #     clicked <- plotly::event_data("plotly_click",
    #                                   source = "scatterselect",
    #                                   priority = "event")
    #
    #     if(nrow(selected_data$df) > 0 & nrow(clicked) > 0){
    #         new <- data.frame(keys = as.integer(clicked$customdata),
    #                           selection_count = max(selected_data$df$selection_count) + 1,
    #                           .annotation = "",
    #                           stringsAsFactors = FALSE)
    #
    #         if(any(new$keys %in% selected_data$df$keys)){
    #             new <- new[!{new$keys %in% selected_data$df$keys}, ]
    #         }
    #
    #         selected_data$df <- rbind(selected_data$df, new)
    #
    #     } else {
    #         new <- data.frame(keys = as.integer(clicked$customdata),
    #                           selection_count = 1,
    #                           .annotation = "",
    #                           stringsAsFactors = FALSE)
    #         selected_data$df <- new
    #     }
    #     print(paste("orig selection:"))
    #     print(selected_data$df)
    # })
    #
    #
    # # handle selections
    # shiny::observeEvent({plotly::event_data("plotly_selected", priority = "event", source = "scatterselect")}, {
    #
    #     # shiny::validate(shiny::need(input[["selectors-startscatter"]],
    #     #                             label = "PlotStarter"))
    #     shiny::validate(shiny::need(action_tracking$plot_start,
    #                                 label = "PlotStarter"))
    #
    #     print("selected!")
    #
    #     # selected <- shiny::reactiveVal()
    #     selected <- plotly::event_data("plotly_selected",
    #                                    source = "scatterselect",
    #                                    priority = "event")
    #
    #     if(length(selected)>0){
    #
    #         if(nrow(selected_data$df) > 0 & nrow(selected) > 0){
    #             new <- data.frame(keys = as.integer(selected$customdata),
    #                               selection_count = max(selected_data$df$selection_count) + 1,
    #                               .annotation = "",
    #                               stringsAsFactors = FALSE)
    #
    #             if(any(new$keys %in% selected_data$df$keys)){
    #                 new <- new[!{new$keys %in% selected_data$df$keys}, ]
    #             }
    #             selected_data$df <- rbind(selected_data$df, new)
    #         } else {
    #             new <- data.frame(keys = as.integer(selected$customdata),
    #                               selection_count = 1,
    #                               .annotation = "",
    #                               stringsAsFactors = FALSE)
    #             selected_data$df <- new
    #         }
    #
    #         print(paste("orig selection:"))
    #         print(selected_data$df)
    #     }
    #
    # })
    #
    # clear on dbl click
    shiny::observeEvent(
        # shiny::observeEvent({
        # plotly::event_data("plotly_doubleclick", source = "scatterselect", priority = "event")
      {plotly::event_data("plotly_deselect", source = "scatterselect", priority = "event")
        input[['undo-undoselection']] }
        , {


            shiny::validate(shiny::need(nrow(selected_data$df) > 0,
                                        label = "need selected data"))
            print("data cleared on dbl click")

            drop_ind <- which(selected_data$df$selection_count == max(selected_data$df$selection_count, na.rm = TRUE))

            if(length(drop_ind) == nrow(selected_data$df)){

                selected_data$df <- data.frame(keys = integer(0),
                                               selection_count = integer(0),
                                               .annotation = character(0),
                                               stringsAsFactors = FALSE)

                print("it's this case, really")
            }
            selected_data$df <- selected_data$df[ -drop_ind, ]
        })



    shiny::observeEvent(
            input[['undo-clearselection']]
        , {


            shiny::validate(shiny::need(nrow(selected_data$df) > 0,
                                        label = "need selected data"))

                selected_data$df <- data.frame(keys = integer(0),
                                               selection_count = integer(0),
                                               .annotation = character(0),
                                               stringsAsFactors = FALSE)

                print("DELETED IT ALL!")

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
                                                    source = ns("plot-scatterselect"),
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
                                                    source = ns("plot-scatterselect"),
                                                    session = session)
                            old_keys(ok())
                        })



    # shiny::observeEvent(plotly::event_data(c("plotly_deselect"),
    #                                        source = "scatterselect",
    #                                        priority = "event"),
    #                     {
    #                         shiny::validate(shiny::need(input[[ns("plot-tracemap")]],
    #                                                     label = "need tracepam"))
    #
    #                         traces <- matrix(input[[ns("plot-tracemap")]], ncol = 2, byrow = TRUE)
    #                         indices <-  as.integer(traces[ as.integer(traces[, 2]) > max_id_original_traces(), 2])
    #
    #                         print(paste("indices are:", indices))
    #
    #                         if(length(indices)>0){
    #                             plotly::plotlyProxy(ns("plot-scatterselect"), session) %>%
    #                                 plotly::plotlyProxyInvoke(
    #                                     "deleteTraces",
    #                                     max(indices)
    #                                 )
    #                             print("removed trace!!")
    #                         }
    #                         old_keys(NULL)
    #                         print(traces)
    #                     })

    # PLOT DELETE TRACES ---------------------------------------------------------


    # undo buttons
    shiny::observe({

        shiny::validate(shiny::need(input[["selectors-startscatter"]], label = "PlotStartbutton"))

        shiny::callModule(module_server_deleteselection_btn,
                          id = "undo")

    })


    # undo last selection with button
    shiny::observeEvent({
        input$`undo-undoselection`},
        {

            input$`plot-undoselection`
            shiny::validate(shiny::need(input[["plot-tracemap"]],
                                        label = "need tracepam"))

            traces <- matrix(input[["plot-tracemap"]], ncol = 2, byrow = TRUE)
            indices <-  as.integer(traces[ as.integer(traces[, 2]) > max_id_original_traces(), 2])

            if(length(indices)>0){
                plotly::plotlyProxy("plot-scatterselect", session) %>%
                    plotly::plotlyProxyInvoke(
                        "deleteTraces",
                        max(indices)
                    )
                print("removed trace!!")
            }
            old_keys(NULL)
        })

    # undo last selection with click
    shiny::observeEvent({
        plotly::event_data(c("plotly_deselect"),
                           source = "scatterselect",
                           priority = "event")},
    {

        shiny::validate(shiny::need(input[["plot-tracemap"]],
                                    label = "need tracepam"))

        traces <- matrix(input[["plot-tracemap"]], ncol = 2, byrow = TRUE)
        indices <-  as.integer(traces[ as.integer(traces[, 2]) > max_id_original_traces(), 2])
        print(traces)


        if(length(indices)>0){
            plotly::plotlyProxy("plot-scatterselect", session) %>%
                plotly::plotlyProxyInvoke(
                    "deleteTraces",
                    max(indices)
                )
            print("removed trace!!")
        }
        old_keys(NULL)
    })


    # delete entire selection with button
    shiny::observeEvent({
        input$`undo-clearselection`},
        {

            shiny::validate(shiny::need(input[["plot-tracemap"]],
                                        label = "need tracepam"))

            traces <- matrix(input[["plot-tracemap"]], ncol = 2, byrow = TRUE)
            indices <-  as.integer(traces[ as.integer(traces[, 2]) > max_id_original_traces(), 2])

            if(length(indices)>0){
                plotly::plotlyProxy("plot-scatterselect", session) %>%
                    plotly::plotlyProxyInvoke(
                        "deleteTraces",
                        indices
                    )
                print("removed trace!!")
            }
            old_keys(NULL)
        })

    # // ----------------------------------------------------------------------


    # ANNOTATION ADDING --------------------------------------------------------



    # shiny::observe({
    shiny::observeEvent(input$`annotator-annotate_button`, {

        # if(nrow(shiny::isolate(selected_data$df))>0){


        selected_data$df <- shiny::callModule(module_server_text_annotator,
                                              "annotator",
                                              sel_data = isolate(selected_data))
        # shiny::validate(need(selected_data,
        # label = "make selection first"))


    })


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
                          "dt",
                          df = datareactive,
                          sel_points = selected_data)

    })

    # // ---------------------------------------------------------------------


    # EXTRACTION --------------------------------------------------------------




    code_out <- shiny::reactiveVal()

    shiny::observe({

        req(datareactive())

        code_out(
            shiny::callModule(module_server_extract_code,
                              id = "extract",
                              df_label = df_name,
                              filter_strings = filter_string,
                              sel_points = selected_data,
                              overwrite = input$overwrite)
        )

    })


    observeEvent(input$`extract-codebtn`, {


        context <- rstudioapi::getSourceEditorContext()
        rstudioapi::insertText(text = paste0("\n", code_out(), "\n"),
                               id = context$id)
    })

    observeEvent(input$`extract-copybtn`, {


        clipr::write_clip(content = code_out(),
                          object_type = "character")

    })


    # // ----------------------------------------------------------------------


    # HELP LINKS --------------------------------------------------------------


    ## Filter Tab
    shiny::observeEvent(input$`help-filter`, {

        shiny::showModal(shiny::modalDialog(
            text_filtering_side_panel,
            title = "How to filter data",
            size = "m",
            easyClose = TRUE,
            footer = NULL
        ))

    })

    # VIZ tab - annotator tool
    shiny::observeEvent(input$`help-annotator`, {

        shiny::showModal(shiny::modalDialog(
            text_annotate_side_panel,
            title = "How to annotate outliers",
            size = "m",
            easyClose = TRUE,
            footer = NULL
        ))

    })
    # Viz tab - plot + selector tool
    shiny::observeEvent(input$`help-plot`, {

        shiny::showModal(shiny::modalDialog(
            text_plot_main_panel,
            title = "Plotting data and selecting outliers",
            size = "m",
            easyClose = TRUE,
            footer = NULL
        ))

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

        # handle plotly TZ issue
        Sys.setenv(TZ = old_tz)
        stopApp("Done")
    })
    observeEvent(input$cancel, {

        Sys.setenv(TZ = old_tz)
        stopApp(NULL)

    })

    }


