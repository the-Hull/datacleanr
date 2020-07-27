#------------------------------------------------------------------------------#
# MODULE UI ----
#' UI Module: plotly plot
#'
#' @param id Character string
#'
module_ui_plot_selectable <- function(id) {
  ns <- shiny::NS(id)

  plotly::plotlyOutput(ns('scatterselect'))

}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

#' Server Module: box for str filter condition
#'
#' Server Module: box for str filter condition
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param df reactive df
#' @param selector_inputs reactive, output from module_plot_selectorcontrols
#' @param sel_points reactive, provides .dcrkey of selected points
#'
#' @details provides plot, note, that data set needs a column .dcrkey, added in initial processing step
module_server_plot_selectable <- function(input, output, session, selector_inputs, df, sel_points){
  ns = session$ns
  sessionval <- session$ns("")



  # JavaScript to add a vector of trace#, traceName;
  # note, trace# starts with 0 in JS / plotly
  # 0 trace 1, 1 trace 2, etc....
  # vector created on click or lasso event
  # and tide to x and ns arguments in "onRender"
  # e.g. data.x, data.nx
  jsfull <- "function(el, x, data){
  var id = el.getAttribute('id');
  var d3 = Plotly.d3;
  el.on('plotly_click', function(event) {
      var out = [];
      d3.select('#' + id + ' g.legend').selectAll('.traces').each(function(){
        var trace = d3.select(this)[0][0].__data__[0].trace;
        out.push([name=trace.name, index=trace.index]);
      });
      Shiny.setInputValue(data.ns + data.x, out);
  });
  el.on('plotly_selected', function(event) {
      var out = [];
      d3.select('#' + id + ' g.legend').selectAll('.traces').each(function(){
        var trace = d3.select(this)[0][0].__data__[0].trace;
        out.push([name=trace.name, index=trace.index]);
      });
      Shiny.setInputValue(data.ns + data.x, out);
  });
}"



  plot_data <- df()


  # n_groups <- length(unique(dplyr::group_indices(plot_data)))
  n_groups <- dplyr::n_groups(plot_data)

  # identifier to cross-ref with grouping table
  # plot_data$.dcrindex <- dplyr::group_indices(plot_data)


  # prepare named value-vector for plotly scale
  #

  col_value_vector <- extend_palette(
    length(
      unique(
        dplyr::group_indices(plot_data)
      ) # / unique
    ) # / length
  )
  names(col_value_vector) <- unique(dplyr::group_indices(plot_data))




  # handler for empty zvar selection
  zvar_toggle <- nchar(shiny::isolate(selector_inputs$zvar()))>0
  if(zvar_toggle){
    # print("zvar given")
    size_expression <- as.formula(paste("~", shiny::isolate(selector_inputs$zvar())))
  } else {
    size_expression <- rlang::quo_expr(NULL)
    # print("zvar empty")
  }




  # handle "Plot!" click
  # shiny::observeEvent(selector_inputs$abutton, {
  output$scatterselect <- plotly::renderPlotly({


    p <-  rlang::eval_tidy(
      rlang::quo_squash(
        rlang::quo({


          print("redrawing")
          pnew <- plotly::plot_ly(data = plot_data,
                          source = "scatterselect"
          ) %>%
            plotly::add_markers(x = ~ !!shiny::isolate(selector_inputs$xvar()),
                                y = ~ !!shiny::isolate(selector_inputs$yvar()),
                                size = eval(size_expression),
                                color = ~as.factor(.dcrindex),
                                name = ~as.factor(.dcrindex),
                                colors = col_value_vector,
                                type = 'scattergl',
                                customdata = ~.dcrkey,
                                text = ~.dcrkey,
                                showlegend = TRUE,
                                marker = list(opacity = 0.7,
                                              line = list(color = col2plotlyrgba("gray60", 0.9),
                                                          width = 1)),
                                # marker = list(color = sapply(plot_data$.color,
                                #                              col2plotlyrgba, 0.9,
                                #                              USE.NAMES = FALSE),
                                #               line = list(color = col2plotlyrgba("gray60", 0.9),
                                #                           width = 1)),
                                unselected = list(marker = list(opacity = 0.7))) %>%
            # ,
            # unselected = list(marker = list(opacity = 0.9))) %>%
            # opacity = ~.opacity) %>%
            plotly::layout(showlegend = TRUE,
                           # dragmode =  FALSE
                           dragmode = "lasso"
            )  %>%
            # plotly::event_register(event = "plotly_doubleclick") %>%
            plotly::event_register(event = "plotly_deselect") %>%
            plotly::event_register(event = "plotly_click") %>%
            plotly::event_register(event = "plotly_selected") %>%
            htmlwidgets::onRender(jsfull, data = list(x = "tracemap",
                                                      ns = sessionval))

        })
      )
    ) #\ eval_tidy
    # plotly::plotly_build(p)




    if(length(shiny::isolate(sel_points$df$keys)) > 0){


      add_data <- dplyr::left_join(shiny::isolate(sel_points$df),
                                   plot_data,
                                   by = c('keys' = '.dcrkey'))

      # print(head(add_data))
      print("READDING traces---------------\\\\")

      p <- rlang::eval_tidy(
        rlang::quo_squash(
          rlang::quo({
            purrr::reduce(.x = split(add_data, f = add_data$selection_count),
                            .f = function(oplot, spdf) {

                              plotly::add_trace(oplot,
                                                data = spdf,
                                                x = ~ !!shiny::isolate(selector_inputs$xvar()),
                                                y = ~ !!shiny::isolate(selector_inputs$yvar()),
                                                size = eval(size_expression),
                                                name = "outlier",
                                                type = "scatter",
                                                mode = "markers",
                                                marker = list(
                                                  color = "darkgray",
                                                  line = list(color = "red",
                                                              width = 2),
                                                  opacity = 1),
                                                unselected = list(marker = list(opacity = 1)))},
                            .init = shiny::isolate(p)
      )

          })
        )
      ) #\ eval_tidy

    } # /if




    #   # handle when input selector changes


    # shiny::observeEvent(selector_inputs$startscatter(),{
    # shiny::observeEvent(plotchange_observer(),{

      # shiny::validate(need(plotchange_observer,
      #                      label = "reactive for tracking plot inputs"))





                          # }) #/observeevent





    return(p)

  }) # / renderPlotly
  # })


#
#   # Handle add traces -------------------------------------------------------
#
#
#
#     old_keys <- shiny::reactiveVal()
#
#
#
#     # traces <- matrix(input$tracemap, ncol = 2, byrow = TRUE)
#     # n_original_traces <- max(traces[, 2])
#     # print(n_original_traces)
#
#
#     max_id_original_traces <- n_groups - 1
#     # n_original_traces <- shiny::reactive({req(input$tracemap)
#     #                                         traces <- matrix(shiny::isolate(input$tracemap), ncol = 2, byrow = TRUE)
#     #                                         return(max(traces[, 2]))
#     #                                         })
#
#
#
#
#     shiny::observeEvent(plotly::event_data("plotly_click", source = "scatterselect", priority = "event"),
#                         # shiny::observeEvent(sel_points,
#
#                         {
#
#
#                           print("that")
#
#
#                           ok <- handle_add_traces(sp = sel_points,
#                                                   pd = plot_data,
#                                                   ok = old_keys,
#                                                   selectors = selector_inputs,
#                                                   source = "scatterselect",
#                                                   session = session)
#
#                           old_keys(ok())
#                         },
#                         once = TRUE)
#
#
#     shiny::observeEvent(plotly::event_data("plotly_selected", source = "scatterselect", priority = "event"),
#                         # shiny::observeEvent(sel_points,
#
#                         {
#
#                           print("this")
#
#
#                           ok <- handle_add_traces(sp = sel_points,
#                                                   pd = plot_data,
#                                                   ok = old_keys,
#                                                   selectors = selector_inputs,
#                                                   source = "scatterselect",
#                                                   session = session)
#
#                           old_keys(ok())
#
#
#                         },
#                         once = TRUE)
#
#
#     # shiny::observeEvent(plotly::event_data(c("plotly_doubleclick"), source = "scatterselect", priority = "event"), {
#     #
#     #   print("remove dblclick")
#     #
#     #   req(input$tracemap)
#     #
#     #
#     #
#     #
#     #   traces <- matrix(input$tracemap, ncol = 2, byrow = TRUE)
#     #   indices <-  as.integer(traces[ as.integer(traces[, 2]) > max_id_original_traces, 2])
#     #
#     #   print(paste("indices are:", indices))
#     #
#     #   if(length(indices)>0){
#     #     plotly::plotlyProxy("plot-scatterselect", session) %>%
#     #       plotly::plotlyProxyInvoke(
#     #         "deleteTraces",
#     #         max(indices)
#     #       )
#     #     print("removed trace!!")
#     #
#     #   }
#     #     print(traces)
#     #     old_keys(NULL)
#     # })
#
#
#     shiny::observeEvent(plotly::event_data(c("plotly_deselect"), source = "scatterselect", priority = "event"), {
#
#       print("remove deselect")
#
#       shiny::validate(shiny::need(input$tracemap,
#                                   label = "need tracepam"))
#
#
#
#
#       traces <- matrix(input$tracemap, ncol = 2, byrow = TRUE)
#       indices <-  as.integer(traces[ as.integer(traces[, 2]) > max_id_original_traces, 2])
#
#       print(paste("indices are:", indices))
#
#       if(length(indices)>0){
#         plotly::plotlyProxy("plot-scatterselect", session) %>%
#           plotly::plotlyProxyInvoke(
#             "deleteTraces",
#             max(indices)
#           )
#
#         print("removed trace!!")
#
#       }
#
#       old_keys(NULL)
#       print(traces)
#     })
#


    #   # handle when input selector changes
    #   plotchange_observer <- shiny::isolate(shiny::reactive(
    # {
    #     list( selector_inputs$xvar(),
    #             selector_inputs$yvar(),
    #             selector_inputs$abutton())}))
    #   #
    #   # # shiny::observeEvent(selector_inputs,
    #   shiny::observeEvent(plotchange_observer(),
    #   # shiny::observeEvent(selector_inputs$yvar(),
    #   #
    #   {
    #
    #     shiny::validate(need(plotchange_observer, label = "reactive for tracking plot inputs"))
    #   #
    #
    #                 print(selector_inputs$xvar())
    #                   print(shiny::is.reactive(selector_inputs$xvar))
    #                   print("new case here")
    #
    #                 })
    #
    #
    #

}


