#' datacleanr server function
#' @param input,output,session standard \code{shiny} boilerplate
#' @param dataset data.frame, tibble or data.table that needs cleaning
#' @param df_name character, name of dataset passed into shiny app
#'
datacleanr_server <- function(input, output, session, dataset, df_name){


  ns <- session$ns



  # old_tz <- Sys.getenv("TZ")
  # Sys.setenv(TZ = "UTC")


  # suppress plotly warnings, etc.
  # options(warn = -1)




  # DIAGNOSTICS ----------------------

  # AllInputs <- shiny::reactive({
  #   x <- unlist(shiny::reactiveValuesToList(input))
  #   paste(names(x),
  #         x)
  #
  #
  # })
  #
  # output$show_inputs <- shiny::renderText({
  #   AllInputs()
  # })




  # Set-up ------------------------------------------------------------------


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

  max_id_original_traces <- shiny::reactive({dplyr::n_groups(datareactive()) - 1})




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

  #apply
  shiny::observeEvent(input$apply_filter, {



    shiny::validate(shiny::need(add.filter,
                                label = "add filter"))
    shiny::validate(shiny::need(input$gobutton,
                                label = "StartButton"))

    shiny::isolate(datareactive(shiny::isolate(tmp_filter()$df)))
    filter_statements_lgl(tmp_filter()$statements_lgl)


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
                        selection = 'none')
      # on viz tab
      shiny::callModule(module_server_group_selector_table,
                        id = "df",
                        df = datareactive,
                        df_label = df_name,
                        selection = 'multiple')
    }

  })



  # GROUPTABLE PLOT LIMITS --------------------------------------------------

  selected_table_rows <- shiny::reactive({!is.null(input$`df-grouptable_rows_selected`)})

  shiny::observeEvent({selected_table_rows()}, {

    shiny::validate(shiny::need(datareactive, label = "datareactive"))
    shiny::validate(shiny::need(input[["selectors-startscatter"]], label = "PlotStartbutton"))
    shiny::validate(shiny::need(action_tracking$plot_start, label = "plot_start actiontracking"))
    shiny::validate(shiny::need(input[["plot-tracemap"]],
                                label = "need tracepam"))

    handle_restyle_traces(source_id = "plot-scatterselect",
                          session = session,
                          dframe = datareactive(),
                          scaling = 0.1,
                          xvar = as.character(selector_vals$xvar),
                          yvar = as.character(selector_vals$yvar),
                          max_id_group_trace = max_id_original_traces(),
                          input_sel_rows = input$`df-grouptable_rows_selected`)

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

    shiny::validate(shiny::need(shiny::isolate(selector_vals),
                                label = "control vals"))
    action_tracking$plot_start <- FALSE
    action_tracking$controls <- TRUE

    print("used controls - disable selecting")
  })


  ## PLOTTING -----------------
  shiny::observeEvent(selector_vals$startscatter,
                      {
                        action_tracking$plot_start <- TRUE
                        action_tracking$controls <- FALSE
                        print("pressed start - enable selecting")

                        shiny::validate(shiny::need(datareactive, label = "datareactive"))
                        shiny::validate(shiny::need(input[["selectors-startscatter"]], label = "PlotStartbutton"))
                        shiny::validate(shiny::need(action_tracking$plot_start, label = "plot_start actiontracking"))

                        if(selected_table_rows()){

                          dtpr <- DT::dataTableProxy(
                            outputId = 'df-grouptable',
                            deferUntilFlush = TRUE
                          )

                          DT::selectRows(proxy = dtpr,
                                         selected = NULL)
                        }

                        shiny::callModule(module_server_plot_selectable,
                                          id = "plot",
                                          df = datareactive,
                                          selector_inputs = shiny::isolate(selector_vals),
                                          sel_points = shiny::isolate(selected_data),
                                          mapstyle = input[['lwrcontrol-mapstyle']])

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
  shiny::observe({
    # shiny::observeEvent(input[["selectors-startscatter"]], {

    # shiny::validate(shiny::need(input[["selectors-startscatter"]], label = "PlotStartbutton"))

    if(shiny::req(action_tracking$plot_start)){
      shiny::callModule(module_server_lowercontrol_btn,
                        id = "lwrcontrol",
                        selector_inputs = shiny::isolate(selector_vals))
    }
  })


  # PLOT DELETE TRACES ---------------------------------------------------------
  # undo last selection with button
  shiny::observeEvent({
    input$`lwrcontrol-undoselection`},
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


        # z <- zvar_toggle(selector_vals$zvar, df = recover_data()[ selected_data$df$keys, ])



        plotly::plotlyProxy("plot-scatterselect", session) %>%
          plotly::plotlyProxyInvoke(
            "addTraces",
            list(
              x = recover_data()[ selected_data$df$keys, as.character(selector_vals$xvar), drop = TRUE],
              y = recover_data()[ selected_data$df$keys, as.character(selector_vals$yvar), drop = TRUE],
              # size = z,
              # sizes = c(25,100),
              type = "scattergl",
              mode = "markers",
              name = "O",
              customdata = recover_data()[selected_data$df$keys, ".dcrkey" , drop = TRUE],
              text = recover_data()[selected_data$df$keys, ".dcrkey" , drop = TRUE],
              marker = list(
                symbol = "x",
                size = 12,
                color = "black",
                opacity = 1),
              unselected = list(marker = list(opacity = 1)),
              selected = list(marker = list(opacity = 1)),
              showlegend = TRUE
            ))


        print("removed points!!")
      }
      old_keys(NULL)
    })

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
    req(selected_data$df)
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




  code_out <- shiny::reactiveVal()

  # shiny::observeEvent({
  # input$apply_filter
  # selected_data$df
  # },
  # {
  shiny::observe({

    shiny::req(datareactive())

    if(!is.null(input$apply_filter) | nrow(selected_data$df) > 0 ){


      code_out(
        shiny::callModule(module_server_extract_code,
                          id = "extract",
                          df_label = df_name,
                          gvar = gvar(),
                          filter_df = add.filter$df,
                          statements = filter_statements_lgl(),
                          sel_points = selected_data$df,
                          overwrite = input$overwrite)
      )
    }

  })


  shiny::observeEvent(input$`extract-codebtn`, {


    context <- rstudioapi::getSourceEditorContext()
    rstudioapi::insertText(text = paste0("\n", code_out(), "\n"),
                           id = context$id)
  })

  shiny::observeEvent(input$`extract-copybtn`, {


    clipr::write_clip(content = code_out(),
                      object_type = "character")

  })


  # // ----------------------------------------------------------------------


  # Help Texts --------------------------------------------------------------

  text_filtering_overview_panel <- shiny::tagList(
    shiny::p(
      "Select relevant groups",
      shiny::tags$b("(order matters for plotting)"),
      "and click",
      shiny::tags$b("Start!"),
      "This displays a summary (by columns, and optionally, by groups), and makes the other tabs' functions available",
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
    )
  )



  text_filtering_side_panel <- shiny::tagList(
    # shiny::p(
    # shiny::tags$b("Add/Remove"),
    # "text boxes and add unquoted filter statements."),
    shiny::p(shiny::tags$b("Add/Remove"),
             "filter statements as necessary. These are passed to",
             shiny::tags$b("dplyr::filter()"), "."),
    shiny::p("Use", shiny::tags$b("single quotes"), "for values of character/factor variables."),
    shiny::tags$p("For example, valid statements for filtering",
                  shiny::tags$b("iris"),
                  "are:"),
    shiny::tags$ol(
      shiny::tags$li(shiny::tags$small("Species == 'setosa'")),
      shiny::tags$li(shiny::tags$small("Species %in% c('setosa','versicolor')")),
      shiny::tags$li(shiny::tags$small("Sepal.Width > quantile(Sepal.Width, 0.05)"))
    ),
    shiny::p("Any function returning a logical vector (i.e. TRUE/FALSE) can be employed here!"),
    shiny::br(),
    shiny::p("A dynamic text will inform you which filter statements are
             viable, and how much of the data will be filtered when they are applied.",
             "Adjust the",
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
    shiny::p("A table",
             shiny::tags$b("Data Overview"),
             "will inform you how many data points remain in the data set (by group)"),
    shiny::p("Note, clicking",
             shiny::tags$b("Apply!"),
             "generates output in the",
             shiny::tags$b("Extract Tab"),
             "if any viable filtering statements have been provided.")
  )


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
    shiny::p("The annotation can be updated or removed by deleting all characters in the input box and clicking the button again."))

  text_distribution_side_panel <- shiny::tagList(
    shiny::p("Clicking the",
             shiny::tags$b("Update"),
             "button will generate histograms of all plotted variables",
             shiny::tags$b("(X, Y, Z)."),
             "If any points have been selected via",
             shiny::tags$b("clicking/lasso-selecting"),
             "the histograms will show the difference between the raw and cleaned data set."),
    shiny::p("Note, that this plot mus tbe re-generated manually to visualize any changes."))


  text_plot_main_panel <- shiny::tagList(
    shiny::p("Select at least ",
             shiny::tags$b("X and Y"),
             "(the ",
             shiny::tags$b("Z"),
             "variable adjusts point size)",
             "and click",
             shiny::tags$b("'Plot!'.")),
    shiny::p("The legend entries correspond with the row-numbers (i.e. groups) of the",
             shiny::tags$b("Data Overview"),
             "Table",
             shiny::tags$b("Groups can be highlighted"),
             "for selective display by clicking on the respective rows",
             "Clicking on the plot's legend (single for hide/unhide, double for hide all others/show all) has a similar effect."),
    shiny::p("Note, that",
             shiny::tags$b("'Plot!'"),
             "must be clicked after any variable input (X, Y, Z) has been",
             shiny::tags$b("clicked or changed."),
             shiny::br(),
             shiny::br(),
             "To mark and exclude outliers, ",
             shiny::tags$b("click or lasso/box select"),
             "individual points.",
             "Hit the",
             shiny::tags$b("Undo last selection"),
             "or",
             shiny::tags$b("Clear all"),
             "button to adjust/remove outliers."),
    shiny::p(shiny::br(),
             "The plot's",
             shiny::tags$b("control bar"),
             "allows to",
             shiny::tags$b("zoom and reset views")),
    shiny::br(),

    shiny::p("Selected points appear in the",
             shiny::tags$b(" table below"),
             "and can be annotated with the tool (box and button) to the left."))

  # HELP LINKS --------------------------------------------------------------


  ## Filter Tab
  shiny::observeEvent(input$`help-ov`, {

    shiny::showModal(shiny::modalDialog(
      text_filtering_overview_panel,
      title = "Getting started",
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
    shiny::observeEvent(input$done, {

      # handle plotly TZ issue
      # Sys.setenv(TZ = old_tz)
      shiny::stopApp("Done")
    })
    # shiny::observeEvent(input$cancel, {
    #
    #   # Sys.setenv(TZ = old_tz)
    #   shiny::stopApp(NULL)
    #
    # })

  }


