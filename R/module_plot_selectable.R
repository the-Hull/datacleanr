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
#' @param df reactive df, with df as element
#' @param df reactive df, with df as element
#' @param selector_inputs reactive, output from module_plot_selectorcontrols
#' @param sel_points reactive, provides .dcrkey of selected points
#'
#' @details provides plot, note, that data set needs a column .dcrkey, added in initial processing step
module_server_plot_selectable <- function(input, output, session, df, selector_inputs, sel_points){
  ns = session$ns
  sessionval <- session$ns("")


#   jsfull <- "function(el, x, data){
#   var id = el.getAttribute('id');
#   var d3 = Plotly.d3;
#   el.on('plotly_click', function(event) {
#       var out = [];
#       d3.select('#' + id + ' g.legend').selectAll('.traces').each(function(){
#         var trace = d3.select(this)[0][0].__data__[0].trace;
#         out.push([name=trace.name, index=trace.index]);
#       });
#       Shiny.setInputValue(data.ns + data.x, out);
#   });
#   el.on('plotly_selected', function(event) {
#       var out = [];
#       d3.select('#' + id + ' g.legend').selectAll('.traces').each(function(){
#         var trace = d3.select(this)[0][0].__data__[0].trace;
#         out.push([name=trace.name, index=trace.index]);
#       });
#       Shiny.setInputValue(data.ns + data.x, out);
#   });
# }"
  jsfull <- "function(el, x, data){
  var id = el.getAttribute('id');
  var d3 = Plotly.d3;
  el.on('plotly_afterplot', function(event) {
      var out = [];
      d3.select('#' + id + ' g.legend').selectAll('.traces').each(function(){
        var trace = d3.select(this)[0][0].__data__[0].trace;
        out.push([name=trace.name, index=trace.index]);
      });
      Shiny.setInputValue(data.ns + data.x, out);
  });
}"

  plot_data <- df$df$data

  n_groups <- length(unique(dplyr::group_indices(plot_data)))

  # adjust colors
  # cols <-  data.frame(.index = unique(dplyr::group_indices(plot_data)),
  #                     .color = extend_palette(
  #                       length(
  #                         unique(
  #                           dplyr::group_indices(plot_data)
  #                         ) # / unique
  #                       ) # / length
  #                     ),
  #                     stringsAsFactors = FALSE) # palette





  plot_data$.index <- dplyr::group_indices(plot_data)


  # prepare named value-vector for ggplot scale
  col_value_vector <- extend_palette(
    length(
      unique(
        dplyr::group_indices(plot_data)
      ) # / unique
    ) # / length
  )
  names(col_value_vector) <- unique(dplyr::group_indices(plot_data))





  # handle "Plot!" click
  shiny::observeEvent(selector_inputs$abutton, {
    output$scatterselect <- plotly::renderPlotly({


      p <-  rlang::eval_tidy(
        rlang::quo_squash(
          rlang::quo({


            print("redrawing")
            plotly::plot_ly(data = plot_data,
                            source = "scatterselect"
            ) %>%
              plotly::add_markers(x = ~ !!selector_inputs$xvar,
                                  y = ~ !!selector_inputs$yvar,
                                  color = ~as.factor(.index),
                                  name = ~as.factor(.index),
                                  colors = col_value_vector,
                                  type = 'scatter',
                                  customdata = ~.dcrkey,
                                  text = ~.dcrkey,
                                  showlegend = TRUE,
                                  marker = list(opacity = 0.9,
                                                line = list(color = col2plotlyrgba("gray60", 0.9),
                                                            width = 1)),
                                  # marker = list(color = sapply(plot_data$.color,
                                  #                              col2plotlyrgba, 0.9,
                                  #                              USE.NAMES = FALSE),
                                  #               line = list(color = col2plotlyrgba("gray60", 0.9),
                                  #                           width = 1)),
                                  unselected = list(marker = list(opacity = 0.9))) %>%
              # ,
              # unselected = list(marker = list(opacity = 0.9))) %>%
              # opacity = ~.opacity) %>%
              plotly::layout(showlegend = TRUE,
                             # dragmode =  FALSE
                             dragmode = "lasso"
              )  %>%
              plotly::event_register(event = "plotly_doubleclick") %>%
              plotly::event_register(event = "plotly_deselect") %>%
              plotly::event_register(event = "plotly_click") %>%
              plotly::event_register(event = "plotly_selected") %>%
              htmlwidgets::onRender(jsfull, data = list(x = "tracemap",
                                                        ns = sessionval))

          })
        )
      ) #\ eval_tidy
      plotly::plotly_build(p)
    })
  })



# Handle add traces -------------------------------------------------------


  old_keys <- shiny::reactiveVal()
  # traces <- matrix(input$tracemap, ncol = 2, byrow = TRUE)
  # n_original_traces <- max(traces[, 2])
  # print(n_original_traces)


  max_id_original_traces <- n_groups - 1
  # n_original_traces <- shiny::reactive({req(input$tracemap)
  #                                         traces <- matrix(shiny::isolate(input$tracemap), ncol = 2, byrow = TRUE)
  #                                         return(max(traces[, 2]))
  #                                         })




  shiny::observeEvent(plotly::event_data("plotly_click", source = "scatterselect", priority = "event"),
  # shiny::observeEvent(sel_points,

                      {




                        ok <- handle_add_traces(sp = sel_points,
                                          pd = plot_data,
                                          ok = old_keys,
                                          selectors = selector_inputs,
                                          source = "scatterselect",
                                          session = session)

                        old_keys(ok())
                      })


  shiny::observeEvent(plotly::event_data("plotly_selected", source = "scatterselect", priority = "event"),
  # shiny::observeEvent(sel_points,

                      {



                        ok <- handle_add_traces(sp = sel_points,
                                          pd = plot_data,
                                          ok = old_keys,
                                          selectors = selector_inputs,
                                          source = "scatterselect",
                                          session = session)

                        old_keys(ok())


                      })


  shiny::observeEvent(plotly::event_data(c("plotly_doubleclick"), source = "scatterselect", priority = "event"), {

    print("remove dblclick")

    req(input$tracemap)




    traces <- matrix(input$tracemap, ncol = 2, byrow = TRUE)
    indices <-  as.integer(traces[ as.integer(traces[, 2]) > max_id_original_traces, 2])

    print(traces)
    print(indices)

    if(length(indices)>0){
      plotly::plotlyProxy("plot-scatterselect", session) %>%
        plotly::plotlyProxyInvoke(
          "deleteTraces",
          max(indices)
        )
    }
  })


  shiny::observeEvent(plotly::event_data(c("plotly_deselect"), source = "scatterselect", priority = "event"), {

    print("remove deselect")

    req(input$tracemap)




    traces <- matrix(input$tracemap, ncol = 2, byrow = TRUE)
    indices <-  as.integer(traces[ as.integer(traces[, 2]) > max_id_original_traces, 2])

    print(traces)
    print(indices)

    if(length(indices)>0){
      plotly::plotlyProxy("plot-scatterselect", session) %>%
        plotly::plotlyProxyInvoke(
          "deleteTraces",
          max(indices)
        )
    }

  })
}
