#' datacleanr server function
#' @param input,output,session standard \code{shiny} boilerplate
#' @param dataset data.frame, tibble or data.table that needs cleaning
#' @param df_name character, name of dataset or file_path passed into shiny app
#' @param is_on_disk logical, whether df was read from file
#'
#' @importFrom rlang .data
#'
datacleanr_server <- function(input, output, session, dataset, df_name, is_on_disk){


    ns <- session$ns



    # old_tz <- Sys.getenv("TZ")
    # Sys.setenv(TZ = "UTC")


    # suppress plotly warnings, etc.
    # options(warn = -1)




    # DIAGNOSTICS ----------------------
#
#     AllInputs <- shiny::reactive({
#       x <- unlist(shiny::reactiveValuesToList(input))
#       paste(names(x),
#             x)
#
#
#     })
#
#     output$show_inputs <- shiny::renderText({
#       AllInputs()
#     })




    # Set-up ------------------------------------------------------------------


    # hide all tabs until go button is clicked
    # shiny::hideTab(inputId = "nav", target = "tabFiltering")
    # shiny::hideTab(inputId = "nav", target = "tabVisualization")
    # shiny::hideTab(inputId = "nav", target = "tabExtraction")
    #
    # shiny::observeEvent(input$gobutton,
    #                     {
    #
    # shiny::showTab(inputId = "nav", target = "tabFiltering")
    # shiny::showTab(inputId = "nav", target = "tabVisualization")
    # shiny::showTab(inputId = "nav", target = "tabExtraction")
    #
    # })

    # handle initialization
    dataset$.dcrkey <- seq_len(nrow(dataset))
    dataset <- dplyr::mutate_if(dataset,
                                .predicate = ~rlang::inherits_any(.x, "POSIXt"),
                                .funs = ~lubridate::force_tz(.x, "UTC"))




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
    # filter_string <- shiny::reactiveVal()
    filter_strings <- shiny::reactiveValues()

    # holds plotly selection data
    selected_data <- shiny::reactiveValues(
        df = data.frame(keys = integer(0),
                        selection_count = integer(0),
                        .annotation = character(0),
                        stringsAsFactors = FALSE)
    )

    # max_id_original_traces <- shiny::reactive({dplyr::n_groups(datareactive()) - 1})
    max_id_original_traces <- shiny::reactive({dplyr::n_groups(datareactive())})




    # // ----------------------------------------------------------------------
    # GROUPING ---------------------------

    # get grouping
    gvar <- shiny::callModule(module_server_group_select,
                              id = "group",
                              dframe = dataset)
    output$gvar <- shiny::reactive({gvar()})
    # check-box for grouping
    grouping_check <- shiny::callModule(module = module_server_checkbox,
                                        "grouptick",
                                        text = "Use grouping for summary")

    shiny::outputOptions(output, "gvar", suspendWhenHidden = FALSE)

    #  START + SUMMARY -------------------

    # handle summary operations when go button is hit
    shiny::observeEvent(input$gobutton, {


        dframe <- apply_data_set_up(df = dplyr::ungroup(dataset), gvar())
        dframe <- dplyr::mutate(dframe,
                                .dcrindex = dplyr::cur_group_id())


        datareactive(dframe)
        recover_data(dframe)

        shiny::callModule(module_server_summary,
                          "summary",

                          df =  {if(!is.null(datareactive()) &&
                                    !grouping_check()){

                              dplyr::ungroup(datareactive())

                          } else if(!is.null(datareactive()) &&
                                    grouping_check()){

                              datareactive()
                          }},
                          df_label = df_name)




    }, priority = 100)


    # // ----------------------------------------------------------------------


    # FILTER STATEMENTS ------------------

    # CREATE EMPTY DATAFRAME
    add.filter <- shiny::reactiveValues()

    add.filter$df <- dplyr::tibble(
        filter = character(0),
        grouping = list())

    btn <- shiny::reactiveValues(value = 0)

    # ADD VARIABLES

    shiny::observeEvent(input$addbutton, {


        shiny::validate(shiny::need(input$gobutton > 0, label = "Press go first!"))


        btn$value <- btn$value + 1

        btn.tmp <- btn$value

        # isolate prevents 'apply filter' from re-firing
        shiny::callModule(module_server_filter_str,
                          btn.tmp,
                          dframe = shiny::isolate(datareactive()))

        shiny::insertUI(
            selector = paste0("#", "placeholder"),
            where = "beforeEnd",
            ui = module_ui_filter_str(btn.tmp)
        )
        # save data into df
        shiny::observeEvent({
            input[[shiny::NS(btn.tmp, "groupdropdown")]]
            input[[shiny::NS(btn.tmp, "filter")]]},
            {
                add.filter$df[btn.tmp, 1] <- input[[shiny::NS(btn.tmp, "filter")]]
                add.filter$df[btn.tmp, "grouping"][[1]] <- list(input[[shiny::NS(btn.tmp, "groupdropdown")]])
            }
        )

        # Remove filters
        shiny::observeEvent(input$removebutton, {

            shiny::validate(shiny::need(input$gobutton > 0, label = "Press go first!"))

            # drop lines and ui
            add.filter$df <- add.filter$df[-btn$value, , drop = FALSE]

            shiny::removeUI(
                selector = paste0("#", btn$value, "-filt"))

            # manage buttons
            if(btn$value > 1){
                btn$value <- btn$value - 1
            } else {
                btn$value <- 0
            }
        })

        # # diagnostic df
        # output$outDF <- shiny::renderPrint({
        #   print(add.filter$df)
        # })


    })




    # FILTER PREVIEW STRING ---------------------------------------------------

    #
    tmp_filter <- shiny::reactiveVal()

    shiny::observe({

        shiny::validate(shiny::need(add.filter,
                                    label = "add filter"))
        # shiny::validate(shiny::need(input$gobutton,
        #                             label = "StartButton"))



        filt_out <- shiny::callModule(module = module_server_df_filter,
                                      id = "check",
                                      dframe = shiny::isolate(recover_data()),
                                      condition_df = add.filter$df)

        shiny::isolate(tmp_filter(filt_out))



    })

    # FILTER Apply/Undo  -------------------------------------------------------


    filter_statements_lgl <- shiny::reactiveVal()
    filter_df <- shiny::reactiveVal(data.frame())

    #apply
    shiny::observeEvent(input$apply_filter, {



        shiny::validate(shiny::need(add.filter,
                                    label = "add filter"))
        shiny::validate(shiny::need(input$gobutton,
                                    label = "StartButton"))

        shiny::isolate(datareactive(shiny::isolate(tmp_filter()$df)))
        filter_statements_lgl(tmp_filter()$statements_lgl)
        filter_df(add.filter$df)


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

        # force replotting of data
        if(!is.null(selector_vals$startscatter)){

            selector_vals$startscatter <- selector_vals$startscatter + 1
        }

    })


    # reset filtering
    shiny::observeEvent(
        {input$reset_filter}, {
            shiny::validate(shiny::need(add.filter,
                                        label = "add filter"))
            shiny::validate(shiny::need(input$gobutton,
                                        label = "StartButton"))

            datareactive(recover_data())
            filter_statements_lgl(NULL)

            # reset filters
            add.filter$df <- add.filter$df[0,,drop = FALSE]


            sapply(seq_len(btn$value),
                   function(i){
                       shiny::removeUI(
                           ## pass in appropriate div id
                           selector = paste0("#", i, "-filt"))
                   }
            )

            btn$value <- 0
            filter_strings <- shiny::reactiveValues()

            # reset selected data
            if(!is.null(selected_data_recovery())){
                # adjust selection
                selected_data$df <- rbind(selected_data$df,
                                          selected_data_recovery())
            }

            ## Logic to handle removal of selected data in plotly
            selected_data_recovery(NULL)

            # force replotting of data
            if(!is.null(selector_vals$startscatter)){

                selector_vals$startscatter <- selector_vals$startscatter + 1

            }


        })



    # FILTER UPDATE GROUP SELECTION -------------------------------------------

    shiny::observeEvent(input$gobutton,
                        {

                            if(btn$value > 0){

                                sapply(seq_len(btn$value),
                                       function(i){


                                           shinyWidgets::updatePickerInput(session = session,
                                                                           inputId = shiny::NS(i, "groupdropdown"),
                                                                           choices = unique(datareactive()$.dcrindex)
                                           )}
                                )

                            }

                        },
                        priority = 0)


    # // ----------------------------------------------------------------------


    # GROUPTABLES --------------------------------------------------------------

    shiny::observe({
        if(!is.null(datareactive() )){
            # on filter tab
            shiny::callModule(module_server_group_selector_table,
                              id = "df-filter-tab",
                              df = datareactive,
                              df_label = df_name,
                              selection = 'none',
                              options = list(scrollX = TRUE))
            # on viz tab
            shiny::callModule(module_server_group_selector_table,
                              id = "df",
                              df = datareactive,
                              df_label = df_name,
                              selection = 'multiple',
                              options = list(scrollX = TRUE))
        }

    })

    # selected_table_rows <- shiny::reactive({!is.null(input$`df-grouptable_rows_selected`)})



    # GROUP TABLE RELAYOUT BTNS---------------------


    shiny::callModule(module_server_group_relayout_buttons,
                      id = "grp_relayout",
                      startscatter = shiny::reactive(selector_vals$startscatter))




    # GROUPTABLE PLOT LIMITS --------------------------------------------------
    selected_table_rows <- shiny::reactiveVal()


    shiny::observeEvent(input[['grp_relayout-update']], {




        shiny::validate(shiny::need(datareactive, label = "datareactive"))
        shiny::validate(shiny::need(input[["selectors-startscatter"]], label = "PlotStartbutton"))
        shiny::validate(shiny::need(action_tracking$plot_start, label = "plot_start actiontracking"))
        shiny::validate(shiny::need(input[["plot-tracemap"]],
                                    label = "need tracepam"))

        selected_table_rows(input$`df-grouptable_rows_selected`)
        trace_map <-  matrix(input[["plot-tracemap"]], ncol = 2, byrow = TRUE)



        handle_restyle_traces(source_id = "plot-scatterselect",
                              session = session,
                              dframe = datareactive(),
                              scaling = 0.1,
                              xvar = as.character(selector_vals$xvar),
                              yvar = as.character(selector_vals$yvar),
                              max_id_group_trace = max_id_original_traces(),
                              trace_map = trace_map,
                              # input_sel_rows = input$`df-grouptable_rows_selected`,
                              input_sel_rows = selected_table_rows(),
                              flush = TRUE)



    })
    shiny::observeEvent(input[['grp_relayout-clear']], {



        shiny::validate(shiny::need(datareactive, label = "datareactive"))
        shiny::validate(shiny::need(input[["selectors-startscatter"]], label = "PlotStartbutton"))
        shiny::validate(shiny::need(action_tracking$plot_start, label = "plot_start actiontracking"))
        shiny::validate(shiny::need(input[["plot-tracemap"]],
                                    label = "need tracepam"))



        dtpr <- DT::dataTableProxy(
            outputId = 'df-grouptable',
            deferUntilFlush = FALSE
        )
        DT::selectRows(proxy = dtpr,
                       selected = NULL)

        selected_table_rows(NULL)
        trace_map <-  matrix(input[["plot-tracemap"]], ncol = 2, byrow = TRUE)


        handle_restyle_traces(source_id = "plot-scatterselect",
                              session = session,
                              dframe = datareactive(),
                              scaling = 0.1,
                              xvar = as.character(selector_vals$xvar),
                              yvar = as.character(selector_vals$yvar),
                              trace_map = trace_map,
                              max_id_group_trace = max_id_original_traces(),
                              # input_sel_rows = input$`df-grouptable_rows_selected`,
                              input_sel_rows = selected_table_rows(),
                              flush = TRUE)

})






    # // ----------------------------------------------------------------------


    selector_vals <- shiny::reactiveValues()

    # PLOT CONTROLS --------------
    # handle data for plotting after gobutton + filtering
    shiny::observeEvent({
        input$gobutton
    },
    {

        shiny::validate(shiny::need(datareactive,
                                    label = "datareactive"))

        shiny::callModule(module_server_plot_selectorcontrols,
                          "selectors",
                          shiny::isolate(datareactive()))

    })


    # set up disabling of selection when using x/y/z vars
    shiny::observe({

        selector_vals$xvar <-  input$`selectors-xvar`
        selector_vals$yvar <- input$`selectors-yvar`
        selector_vals$zvar <- input$`selectors-zvar`
        selector_vals$startscatter <- input$`selectors-startscatter`
    })


    # disable selecting when clicked on x/y/z var for plot (need to replot)
    shiny::observeEvent({
        selector_vals$xvar
        selector_vals$yvar
        selector_vals$zvar
    }, {

        shiny::validate(shiny::need(shiny::isolate(selector_vals),
                                    label = "control vals"))
        action_tracking$plot_start <- FALSE
        action_tracking$controls <- TRUE

        print("used controls - disable selecting")
    })


    # disable selecting when clicked on different map style (need to replot)
    # shiny::observeEvent(input[['lwrcontrol-mapstyle']],{
    #
    #   shiny::validate(shiny::need(shiny::isolate(selector_vals),
    #                               label = "control vals"))
    #
    #   # shiny::validate(shiny::need(input[["selectors-startscatter"]],
    #   #                             label = "PlotStartbutton"))
    #
    #   action_tracking$plot_start <- FALSE
    #   action_tracking$controls <- TRUE
    #
    #   print("used controls - disable selecting")
    # }
    #                     )


    ## PLOTTING -----------------





    shiny::observeEvent(selector_vals$startscatter,
                        {
                            action_tracking$plot_start <- TRUE
                            action_tracking$controls <- FALSE
                            print("pressed start - enable selecting")

                            shiny::validate(shiny::need(datareactive, label = "datareactive"))
                            shiny::validate(shiny::need(input[["selectors-startscatter"]], label = "PlotStartbutton"))
                            shiny::validate(shiny::need(action_tracking$plot_start, label = "plot_start actiontracking"))
#
#                             if(selected_table_rows()){
#
#                                 dtpr <- DT::dataTableProxy(
#                                     outputId = 'df-grouptable',
#                                     deferUntilFlush = TRUE
#                                 )
#
#                                 DT::selectRows(proxy = dtpr,
#                                                selected = NULL)
#                             }

                            shiny::callModule(module_server_plot_selectable,
                                              id = "plot",
                                              df = datareactive,
                                              selector_inputs = shiny::isolate(selector_vals),
                                              sel_points = shiny::isolate(selected_data),
                                              mapstyle = shiny::isolate(input[['lwrcontrol-mapstyle']]))

                            # if(selected_table_rows()){
                            #
                            #   print("GROUP TABLE SELECTED")
                            #
                            #
                            #   handle_restyle_traces(
                            #     source_id = "plot-scatterselect",
                            #     session = session,
                            #     dframe = datareactive(),
                            #     scaling = 0.075,
                            #     xvar = as.character(selector_vals$xvar),
                            #     yvar = as.character(selector_vals$yvar),
                            #     max_id_group_trace = max_id_original_traces(),
                            #     input_sel_rows = input[['df-grouptable_rows_selected']],
                            #     flush = FALSE)
                            #
                            #
                            #
                            #
                            #
                            # }

                        }) #/observe


    # selected_data <- shiny::reactiveValues(
    #     df = data.frame(keys = integer(0),
    #                     selection_count = integer(0),
    #                     .annotation = character(0),
    #                     stringsAsFactors = FALSE)
    # )

    # force replotting in plot observer
    # there must be a better way of achieving this
    # likely with plotly::restyle or relayout
    # shiny::observeEvent(input[['lwrcontrol-mapstyle']],
    #   {
    #     shiny::isolate({selector_vals$startscatter <- selector_vals$startscatter + 1})
    #   }
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


            selected_data$df <- handle_sel_outliers(sel_old_df = selected_data$df,
                                                    sel_new = selected)



        })


    shiny::observeEvent({
        plotly::event_data("plotly_click", priority = "event", source = "scatterselect")}, {
            print("clicked!")

            shiny::validate(shiny::need(action_tracking$plot_start,
                                        label = "PlotStarter"))

            clicked <- plotly::event_data("plotly_click",
                                          source = "scatterselect",
                                          priority = "event")

            selected_data$df <- handle_sel_outliers(sel_old_df = selected_data$df,
                                                    sel_new = clicked)


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
    # undo on button
    shiny::observeEvent(
        input[['lwrcontrol-undoselection']]
        , {


            shiny::validate(shiny::need(nrow(selected_data$df) > 0,
                                        label = "need selected data"))
            print("data cleared on button click")

            drop_ind <- which(selected_data$df$selection_count == max(selected_data$df$selection_count, na.rm = TRUE))

            if(length(drop_ind) == nrow(selected_data$df)){

                selected_data$df <- data.frame(keys = integer(0),
                                               selection_count = integer(0),
                                               .annotation = character(0),
                                               stringsAsFactors = FALSE)

            }
            selected_data$df <- selected_data$df[ -drop_ind, ]
        }, priority = 1000)


    # clear on dbl click
    shiny::observeEvent(
        plotly::event_data("plotly_deselect", source = "scatterselect", priority = "event")
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

            }
            selected_data$df <- selected_data$df[ -drop_ind, ]
        })



    shiny::observeEvent(
        input[['lwrcontrol-clearselection']]
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


    max_trace <- shiny::reactive({
        shiny::validate(shiny::need(input[["plot-tracemap"]],
                                    label = "need tracepam"))

        mt <- max(as.numeric(matrix(input[["plot-tracemap"]], ncol = 2, byrow = TRUE)[,2]))

        return(mt)

    })



    old_keys <- shiny::reactiveVal()

    # max_id_original_traces <- shiny::reactive({dplyr::n_groups(datareactive()) - 1})
    shiny::observeEvent(plotly::event_data("plotly_click",
                                           source = "scatterselect",
                                           priority = "event"),
                        {





                            ok <- handle_add_outlier_trace(sp = selected_data,
                                                           dframe = recover_data,
                                                           ok = old_keys,
                                                           selectors = selector_vals,
                                                           trace_map = shiny::isolate(matrix(input[["plot-tracemap"]], ncol = 2, byrow = TRUE)),
                                                           source = "plot-scatterselect",
                                                           session = session)
                            old_keys(ok())
                        })


    shiny::observeEvent(plotly::event_data("plotly_selected",
                                           source = "scatterselect",
                                           priority = "event"),
                        {


                            ok <- handle_add_outlier_trace(sp = selected_data,
                                                           dframe = recover_data,
                                                           ok = old_keys,
                                                           selectors = selector_vals,
                                                           source = "plot-scatterselect",
                                                           trace_map = shiny::isolate(matrix(input[["plot-tracemap"]], ncol = 2, byrow = TRUE)),
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



    # PLOT DELETE / MAP CHOICE Control ----------------------------------------



    # undo buttons
    # shiny::observe({
    # shiny::observeEvent(input[["selectors-startscatter"]], {
    shiny::observeEvent(ignoreInit = TRUE,
                        ignoreNULL = TRUE,
                        action_tracking$plot_start, {

                            # shiny::validate(shiny::need(input[["selectors-startscatter"]], label = "PlotStartbutton"))

                            # if(shiny::req(shiny::isolate(action_tracking$plot_start))){
                            # if(shiny::req(shiny::isolate(action_tracking$plot_start))){
                            shiny::callModule(module_server_lowercontrol_btn,
                                              id = "lwrcontrol",
                                              selector_inputs = shiny::isolate(selector_vals),
                                              action_track = action_tracking)
                            # }


                        })


    # PLOT DELETE TRACES ---------------------------------------------------------
    # undo last selection with button
    shiny::observeEvent({
        input$`lwrcontrol-undoselection`},
        {





            shiny::validate(shiny::need(input[["plot-tracemap"]],
                                        label = "need tracepam"))


            is_spatial_plot <- identical(c(as.character(selector_vals$xvar),
                                           as.character(selector_vals$yvar)),
                                         c("lon", "lat"))

            traces <- matrix(input[["plot-tracemap"]], ncol = 2, byrow = TRUE)
            # indices <-  as.integer(traces[ as.integer(traces[, 2]) > max_id_original_traces(), 2])


            print(traces)

            indices <- as.integer(traces[traces[, 1]=="O" ,2])




            if(length(indices)>0){

                pproxy <- plotly::plotlyProxy("plot-scatterselect", session)

                    plotly::plotlyProxyInvoke(
                        pproxy,
                        "deleteTraces",
                        max(indices)
                    )


                # z <- zvar_toggle(selector_vals$zvar, df = recover_data()[ selected_data$df$keys, ])


                add_points <- recover_data()[selected_data$df$keys, ]
                # handle plotly - only adds trace for array > 2L
                if(nrow(add_points) == 1){
                    add_points <- rbind(add_points, add_points)
                }




                if(is_spatial_plot){


                    add_list <-

                    list(
                        lon = add_points[ , as.character(selector_vals$xvar), drop = TRUE],
                        lat = add_points[ , as.character(selector_vals$yvar), drop = TRUE],
                        customdata = add_points[ , ".dcrkey", drop = TRUE],
                        text = add_points[ , ".dcrkey", drop = TRUE],
                        type = "scattermapbox",
                        mode = "markers",
                        name = "O",
                        opacity = 1,
                        marker = list(
                            symbol = "hospital",
                            size = 12,
                            allowoverlap = TRUE,
                            color = "black",
                            opacity = 1),
                        selected = list(marker = list(opacity = 1)),
                        unselected = list(marker = list(opacity = 1)),
                        showlegend = list(TRUE))

                } else {

                    add_list <- list(
                        x = add_points[ , as.character(selector_vals$xvar), drop = TRUE],
                        y = add_points[ , as.character(selector_vals$yvar), drop = TRUE],
                        # size = z,
                        # sizes = c(25,100),
                        type = "scattergl",
                        mode = "markers",
                        name = "O",
                        customdata = add_points[ , ".dcrkey" , drop = TRUE],
                        text = add_points[ , ".dcrkey" , drop = TRUE],
                        marker = list(
                            symbol = ifelse(is_spatial_plot,
                                            "hospital",
                                            "x"),
                            size = 12,
                            color = "black",
                            opacity = 1),
                        unselected = list(marker = list(opacity = 1)),
                        selected = list(marker = list(opacity = 1)),
                        showlegend = TRUE
                    )

                }


                    plotly::plotlyProxyInvoke(
                        pproxy,
                        "addTraces",
                        add_list
                        )


            }
            old_keys(NULL)
        },
        priority = 0)

    # undo last selection with click
    # shiny::observeEvent({
    #   plotly::event_data(c("plotly_deselect"),
    #                      source = "scatterselect",
    #                      priority = "event")},
    #   {
    #
    #     shiny::validate(shiny::need(input[["plot-tracemap"]],
    #                                 label = "need tracepam"))
    #
    #     traces <- matrix(input[["plot-tracemap"]], ncol = 2, byrow = TRUE)
    #     indices <-  as.integer(traces[ as.integer(traces[, 2]) > max_id_original_traces(), 2])
    #
    #
    #     if(length(indices)>0){
    #       plotly::plotlyProxy("plot-scatterselect", session) %>%
    #         plotly::plotlyProxyInvoke(
    #           "deleteTraces",
    #           max(indices)
    #         )
    #       print("removed trace!!")
    #     }
    #     old_keys(NULL)
    #   })


    # delete entire selection with button
    shiny::observeEvent({
        input$`lwrcontrol-clearselection`},
        {

            shiny::validate(shiny::need(input[["plot-tracemap"]],
                                        label = "need tracepam"))

            traces <- matrix(input[["plot-tracemap"]], ncol = 2, byrow = TRUE)
            # indices <-  as.integer(traces[ as.integer(traces[, 2]) > max_id_original_traces(), 2])
            indices <- as.integer(traces[traces[, 1]=="O" ,2])


            if(length(indices)>0){
                plotly::plotlyProxy("plot-scatterselect", session) %>%
                    plotly::plotlyProxyInvoke(
                        "deleteTraces",
                        indices
                    )
            }
            old_keys(NULL)
        })

    # // ----------------------------------------------------------------------




    # PLOT HISTOGRAMS ---------------------------------------------------------


    output$histogramupdatebutton <- shiny::renderUI({

        # shiny::observeEvent(selected_data, {
        shiny::validate(shiny::need(input[["selectors-startscatter"]],
                                    label = "",
                                    message = ""))

        shiny::tagList(shiny::actionButton(inputId = "histogramupdate",
                                           label = "Update",
                                           icon = shiny::icon("sync-alt"),
                                           class = "btn-info"))

    })

    shiny::observeEvent(input$histogramupdate, {
        # shiny::observe({

        if(shiny::req(action_tracking$plot_start)){
            shiny::callModule(id = "plotvars",
                              module = module_server_histograms,
                              dframe = shiny::isolate(datareactive()),
                              selector_inputs = shiny::isolate(selector_vals),
                              sel_points = shiny::isolate(selected_data$df))
        }

    },
    priority = 0)


    # // ----------------------------------------------------------------------



    # ANNOTATION ADDING --------------------------------------------------------


    # provide annotator button
    shiny::observeEvent(input$`annotator-annotate_button`, {

        selected_data$df <- shiny::callModule(module_server_text_annotator,
                                              "annotator",
                                              sel_data = shiny::isolate(selected_data))

    })

    # handle auto-annotation
    selected_data_nrow_track <- shiny::reactiveValues(last_nrow=0,cur_nrow=0)

    shiny::observeEvent(selected_data$df,
                        {selected_data_nrow_track$last_nrow <- selected_data_nrow_track$cur_nrow;
                        selected_data_nrow_track$cur_nrow <- nrow(selected_data$df)})


    selected_data_nrow_track_lgl <- shiny::reactive({
        shiny::req(selected_data$df)
        return(selected_data_nrow_track$cur_nrow >= selected_data_nrow_track$last_nrow)
    })


    shiny::observeEvent(
        selected_data$df, {

            if(input$`annotator-autoannotate` &
               selected_data_nrow_track_lgl()){

                selected_data$df <- shiny::callModule(module_server_text_annotator,
                                                      "annotator",
                                                      sel_data = shiny::isolate(selected_data))
            }


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
                          df = recover_data(),
                          sel_points = selected_data)

    }, priority = 150)

    # // ---------------------------------------------------------------------


    # EXTRACTION --------------------------------------------------------------



    # code_out <- shiny::reactiveVal()

    # shiny::observe({

    # shiny::req(datareactive())

    # if(!is.null(input$apply_filter) | nrow(selected_data$df) > 0 ){
    # if(nrow(filter_df()) >= 0  | nrow(selected_data$df) >= 0 ){


    # code_out(
    code_out <- shiny::callModule(module_server_extract_code,
                                  id = "extract",
                                  df_label = df_name,
                                  gvar = gvar,
                                  filter_df = filter_df,
                                  statements = filter_statements_lgl,
                                  sel_points = selected_data,
                                  overwrite = shiny::reactive(input$`config-overwrite`),
                                  is_on_disk = is_on_disk,
                                  out_path = out_path)
    # )
    # }

    # })


    shiny::observeEvent(input$`config-codebtn`, {


        context <- rstudioapi::getSourceEditorContext()
        rstudioapi::insertText(text = paste0("\n", code_out(), "\n"),
                               id = context$id)
    })

    shiny::observeEvent(input$`config-copybtn`, {


        clipr::write_clip(content = code_out(),
                          object_type = "character")

    })



    out_data <- shiny::reactive({
        dcr_code <-  paste0("\n", code_out(), "\n")
        class(dcr_code) <- c(class(dcr_code), "dcr_code")

        out_data <- list(
            df_name = df_name,
            dcr_df =
                dplyr::left_join(x  = datareactive(),
                                 y  = selected_data$df,
                                 by = c(".dcrkey" = "keys")) %>%
                dplyr::select(-.data$.dcrindex),
            dcr_selected_outliers =
                selected_data$df %>%
                dplyr::rename(.dcrkey = .data$keys),
            dcr_groups = gvar(),
            dcr_condition_df =
                if(nrow(filter_df()) > 0){
                    filter_df()[filter_statements_lgl(), ]}
            else {NULL},
            dcr_code = dcr_code
        )

        return(out_data)

    })


    # EXTR - CONFIG, SAVE UI ---------------------

    has_processed <- shiny::reactive({

        selected_data$df
        filter_df()
        filter_statements_lgl()

        if(nrow(selected_data$df) > 0 |
           nrow(filter_df()[filter_statements_lgl(), ]) > 0){


            return(TRUE)

        } else {

            return(FALSE)
        }
    })

    out_path <- shiny::callModule(module_server_extract_code_fileconfig,
                                  id = "config",
                                  df_label = df_name,
                                  is_on_disk = is_on_disk,
                                  has_processed = has_processed
    )


    # EXTR - SAVE ACTION ------------------

    shiny::observeEvent(input$`config-save`,
                        {

                            # req(nchar(out_path()$dirraw > 0))

                            # file_out_raw <- make_save_filepath(
                            #     save_dir = out_path()$dirraw,
                            #     input_filepath = df_name,
                            #     suffix = "meta_RAW",
                            #     ext = "Rds")
                            #
                            # file_out_cleaned <- make_save_filepath(
                            #     save_dir = out_path()$dirclean,
                            #     input_filepath = df_name,
                            #     suffix = "CLEAN",
                            #     ext = "Rds")
                            #
                            #
                            # file_script_cleaning <- make_save_filepath(
                            #     save_dir = out_path()$dirclean,
                            #     input_filepath = df_name,
                            #     suffix = "cleaning_recipe",
                            #     ext = "R")


                            shiny::req(!is.null(code_out()))


                            saveRDS(object = out_data()$dcr_df,
                                    file = out_path()$file_out_cleaned)
                            saveRDS(object = list(dcr_condition_df = out_data()$dcr_condition_df,
                                                  dcr_selected_outliers = out_data()$dcr_selected_outliers),
                                    file = out_path()$file_out_raw)

                            writeLines(text = enc2utf8(out_data()$dcr_code),
                                       con = out_path()$file_script_cleaning,
                                       useBytes = TRUE)

                        })


    # // ----------------------------------------------------------------------


    # Help Texts --------------------------------------------------------------

    text_overview_group_panel <- shiny::tagList(
        shiny::p(
            "Select relevant groups",
            shiny::tags$b("(order matters for plotting)"),
            shiny::br(),

            "The",
            shiny::tags$b("grouping order"),
            "should reflect units that are most convenient for breaking up the data,",
            "and how you plan on cycling through them during visual cleaning. For example, useful groups could be",

            shiny::tags$ol(
                shiny::tags$li(shiny::tags$small("time periods")),
                shiny::tags$li(shiny::tags$small("locations")),
                shiny::tags$li(shiny::tags$small("sensors"))
            ),

            "Smart combinations of grouping variables can, e.g. allow to cycle through multiple sensors for a given period with ease."
        ))

    text_overview_start_panel <- shiny::tagList(
        shiny::p(
            "Click",
            shiny::tags$b("Start"),
            "to set groups for subsequent tabs and operations.",
            "It also displays a summary (by columns, and optionally, by groups), and makes the other tabs' functions available."),


        shiny::p(
            "  If you",
            shiny::tags$strong("change the grouping"),
            "make sure to click",
            shiny::tags$strong("Start"),
            "and to update your",
            shiny::tags$strong("filters and plot."),
            class = "btn btn-default action-button btn-info")
    )



    text_filtering_side_panel <- shiny::tagList(
        # shiny::p(
        # shiny::tags$b("Add/Remove"),
        # "text boxes and add unquoted filter statements."),
        shiny::p(shiny::tags$b("Add/Remove"),
                 "filter statements as necessary. These are passed to",
                 shiny::tags$b(shiny::tags$code("dplyr::filter()")), "."),
        shiny::p("Use", shiny::tags$b("single quotes"), "for values of character/factor variables."),
        shiny::tags$p("For example, valid statements for filtering",
                      shiny::tags$b("iris"),
                      "are:"),
        shiny::tags$ol(
            shiny::tags$li(shiny::tags$small(shiny::tags$code("Species == 'setosa'"))),
            shiny::tags$li(shiny::tags$small(shiny::tags$code("Species %in% c('setosa','versicolor')"))),
            shiny::tags$li(shiny::tags$small(shiny::tags$code("Sepal.Width > quantile(Sepal.Width, 0.05)")))
        ),
        shiny::p("Any function returning a logical vector (i.e. ",
                 shiny::tags$code("TRUE/FALSE", .noWS = "after"),
                 "), can be employed here!", .noWS = "inside"),
        shiny::p("A dynamic text will inform you which filter statements are
             viable, and how much of the data will be filtered when they are applied."),
        shiny::br(),
        shiny::p("Adjust the",
                 shiny::tags$b("Grouping scope"),
                 "in the drop down next to the text box. This allows to apply the filter statement to:"),
        shiny::tags$ol(
            shiny::tags$li(shiny::tags$small("none (ungrouped filtering)")),
            shiny::tags$li(shiny::tags$small("individual groups (scoped filtering)")),
            shiny::tags$li(shiny::tags$small("all groups (grouped filtering)"))
        ),
        shiny::p("Click",
                 shiny::tags$b("'Apply'"),
                 "when you're ready, and",
                 shiny::tags$b("'Reset'"),
                 "to start from scratch."),

        shiny::p(class = "btn btn-default btn-info",
                 "Code in the",
                 shiny::tags$b("Extract Tab"),
                 "is generated for viable statements only.")
        # shiny::br(),
        # shiny::br(),
        #
        # shiny::p("A table",
        #          shiny::tags$b("Data Overview"),
        #          "will inform you how many data points remain in the data set (by group)")
    )


    text_annotate_side_panel <- shiny::tagList(
        shiny::p("After selecting points by",
                 shiny::tags$b("clicking/lasso-selecting"),
                 "the",
                 shiny::tags$b("last selection"),
                 "can be annotated with a text label by clicking the",
                 shiny::tags$b("Annotate"),
                 "button",
                 shiny::br(),
                 "The annotation for the last selection can be updated or
                 removed by deleting all characters in the input box and
                 clicking the button again.",
                 shiny::br()),
        shiny::p("When",
                 shiny::tags$b("Auto-annotate"),
                 "is selected, every new selection receives the current annotation automatically.",),
        shiny::p("Labels are collected and provided as an additional column",
                 shiny::tags$code(".annotation"),
                 "in the table to the right and outputted via the",
                 shiny::tags$b("Extraction Tab.")),

    )

    text_distribution_side_panel <- shiny::tagList(
        shiny::p("Clicking the",
                 shiny::tags$b("Update"),
                 "button will generate histograms of plotted, numeric variables, i.e",
                 shiny::tags$code("X, Y, Z", .noWS = "after"),
                 ".",
                 shiny::br(),
                 "If any points have been selected via",
                 shiny::tags$b("clicking/lasso-selecting"),
                 "the histograms will show the difference between the raw and cleaned data set."),
        shiny::p(class = "btn btn-default action-button btn-info",
                 "This plot mus tbe updated manually to visualize any changes."))


    text_plot_main_panel <- shiny::tagList(
        shiny::h4(shiny::tags$b("Plotting overview"),
                  .noWS = c("before")),
        shiny::p("Select at least ",
                 shiny::tags$code("X"),
                 "and",
                 shiny::tags$code("Y"),
                 "(the ",
                 shiny::tags$code("Z"),
                 "variable adjusts point size)",
                 "and click",
                 shiny::tags$b("'Plot'.")),
        shiny::p("The legend entries correspond with the row-numbers (i.e. groups) of the",
                 shiny::tags$b("Data Overview"),
                 "Table.",
                 shiny::tags$b("Groups can be highlighted"),
                 "for selective display by clicking on the respective rows and updating the plot's groups.",
                 shiny::br(),
                 "Clicking on the plot's legend (single for hide/unhide, double for hide all others/show all) has a similar effect, but does not reset the grouping or zoom."),
        shiny::p(shiny::tags$b("'Plot'"),
                 "must be clicked after any variable input",
                 shiny::code("(X, Y, Z)"),
                 "has been",
                 shiny::tags$b("clicked or changed"),
                 "to (re-)enable selecting."),
        shiny::p("The plot's",
                 shiny::tags$b("control bar"),
                 "allows to",
                 shiny::tags$b("zoom and reset views.")),
        shiny::h4(shiny::tags$b("Selecting outliers")),

        shiny::p("To mark outliers, ",
                 shiny::tags$b("click or lasso/box select"),
                 "individual points.",
                 "Hit the",
                 shiny::tags$b("Undo last selection"),
                 "or",
                 shiny::tags$b("Clear all"),
                 "button to adjust/remove outliers.",
                 shiny::br(),
                 "Selected points appear in the",
                 shiny::tags$b(" table below"),
                 "and can be annotated with the tool (box and button) to the left."),

        shiny::p(class = "btn btn-default btn-info",
                 "The",
                 shiny::tags$b("Extract Tab"),
                 "will provide code representing the outlier selection (and its removal)."
        ),

        shiny::h4(shiny::tags$b("Other features")),
        shiny::p("If columns",
                 shiny::tags$code("lon, lat"),
                 "are selected for",
                 shiny::tags$code("x, y"),
                 "an interactive map displays."),

        shiny::p("If a column",
                 shiny::tags$code(".dcrflag"),
                 "(logical;",
                 shiny::tags$code("TRUE, FALSE", .noWS = "after"),
                 ")",
                 "is present in the data set, corresponding rows will
                 be plotted with triangle symbols, not cricles,
                 enabling e.g. comparison with manual outlier algorithms.",
        )
    )



    text_repro_panel <- shiny::tagList(

        shiny::h4(shiny::tags$b("Principle")),
        shiny::p(
            shiny::tags$code("datacleanr"),
            "allows filtering and visual cleaning/annotating.",
            "These steps are translated into code (",
            shiny::tags$b("Reproducible Recipe", .noWS = c("before", "after")),
            ")."),
        shiny::h4(shiny::tags$b("Use")),
        shiny::p(
            "If",
            shiny::tags$code("datacleanr"),
            "is run interactively (i.e. with an object from",
            shiny::tags$code("R", .noWS = "after"),
            "s environment)",
            "the code can be sent back to RStudio or copied into a script where
            you are actively developing your workflow.",
            shiny::br(),
            "If",
            shiny::tags$code("datacleanr"),
            "is run with a file from disk, the",
            shiny::tags$b("Extract Tab"),
            "offers a range of settings for saving the generated outputs to file.",
            shiny::br(),
            "Note, that filters and selected outliers are written to files in this scenario."),

        shiny::h4(shiny::tags$b("On Close")),

        shiny::p("If you click",
                 shiny::tags$b("Close"),
                 "and don't export/save any outputs, the filters, selected data and other meta info is
        sent back to",
                 shiny::tags$code("R", .noWS = "after"),
                 "s current session invisibly.",
                 shiny::br(),
                 "Using",
                 shiny::tags$code("output <- .Last.value"),
                 "will capture this output (which can also be set-up when you launch",
                 shiny::tags$code("datacleanr", .noWS = "after"),
                 "; see",
                 shiny::tags$code("?datacleanr::dcr_app()", .noWS = "after"),
                 ").")
    )
    # HELP LINKS --------------------------------------------------------------


    ## Filter Tab
    shiny::observeEvent(input$`help-ov`, {

        shiny::showModal(shiny::modalDialog(
            text_overview_group_panel,
            title = "Selecting Groups",
            size = "m",
            easyClose = TRUE,
            footer = NULL
        ))

    })
    shiny::observeEvent(input$`help-start`, {

        shiny::showModal(shiny::modalDialog(
            text_overview_start_panel,
            title = "Start, summarize and initialize other tabs",
            size = "m",
            easyClose = TRUE,
            footer = NULL
        ))

    })




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

    # VIZ tab - histogram tool
    shiny::observeEvent(input$`help-hist`, {

        shiny::showModal(shiny::modalDialog(
            text_distribution_side_panel,
            title = "Assessing outlier impact",
            size = "m",
            easyClose = TRUE,
            footer = NULL
        ))

    })
    # Viz tab - plot + selector tool
    shiny::observeEvent(input$`help-plot`, {

        shiny::showModal(shiny::modalDialog(
            text_plot_main_panel,
            # title = "Plotting data and selecting outliers",
            size = "m",
            easyClose = TRUE,
            footer = NULL
        ))

    })


    shiny::observeEvent(input$`help-repro`, {

        shiny::showModal(shiny::modalDialog(
            text_repro_panel,
            # title = "Plotting data and selecting outliers",
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
    shiny::observeEvent(input$close, {

        # dcr_code <-  paste0("\n", code_out(), "\n")
        # class(dcr_code) <- c(class(dcr_code), "dcr_code")
        #
        # out_data <- list(
        #     df_name = df_name,
        #     dcr_df =
        #         dplyr::left_join(x  = datareactive(),
        #                          y  = selected_data$df,
        #                          by = c(".dcrkey" = "keys")),
        #     dcr_selected_outliers =
        #         selected_data$df %>%
        #         dplyr::rename(.dcrkey = .data$keys),
        #     dcr_groups = gvar(),
        #     dcr_condition_df =
        #         if(nrow(filter_df()) > 0){
        #             filter_df()[filter_statements_lgl(), ]}
        #     else {NULL},
        #     dcr_code = dcr_code
        # )

        # handle plotly TZ issue
        # Sys.setenv(TZ = old_tz)
        shiny::stopApp(returnValue = invisible(out_data()))
    })
    shiny::observeEvent(input$cancel, {

        # Sys.setenv(TZ = old_tz)
        shiny::stopApp(NULL)

    })

}


