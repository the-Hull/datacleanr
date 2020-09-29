#------------------------------------------------------------------------------#
# MODULE UI ----
#' UI Module: plotly plot
#'
#' @param id Character string
#'
module_ui_plot_selectable <- function(id) {
  ns <- shiny::NS(id)

  plotly::plotlyOutput(ns('scatterselect'),
                       height = "500px")

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
#' @param mapstyle reactive, selected mapstyle from below-plot controls
#'
#' @importFrom rlang .data
#'
#' @details provides plot, note, that data set needs a column .dcrkey, added in initial processing step
module_server_plot_selectable <- function(input, output, session, selector_inputs, df, sel_points, mapstyle){
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
   el.on('plotly_afterplot', function(event) {
      var out = [];
      d3.select('#' + id + ' g.legend').selectAll('.traces').each(function(){
        var trace = d3.select(this)[0][0].__data__[0].trace;
        out.push([name=trace.name, index=trace.index]);
      });
      Shiny.setInputValue(data.ns + data.x, out);
  });
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


  # Check for .dcrflag column
  has_flag_column <- utils::hasName(plot_data, ".dcrflag")


  # n_groups <- dplyr::n_groups(plot_data)
  n_groups_original <- max(plot_data$.dcrindex)


  # prepare named value-vector for plotly scale
  col_value_vector <- extend_palette(
    n_groups_original
  )

  set.seed(123)
  col_value_vector <- col_value_vector[sample(seq_len(n_groups_original),
                                              size = n_groups_original,
                                              replace = FALSE)]

  names(col_value_vector) <- seq_len(n_groups_original)

  # subset to available groups
  groups_available <- names(col_value_vector) %in% seq_len(n_groups_original)
  col_value_vector <- col_value_vector[groups_available]

  is_spatial_plot <- identical(c(as.character(selector_inputs$xvar),
                                 as.character(selector_inputs$yvar)),
                               c("lon", "lat"))


  # handler for empty zvar selection
  zvar_toggle <- nchar(shiny::isolate(selector_inputs$zvar))>0
  if(zvar_toggle){
    size_expression <- stats::as.formula(paste("~", shiny::isolate(selector_inputs$zvar)))
    sizes_expression <- expression(c(5, 100))
    print("sizes adjusted")
  } else {
    # size_expression <- rlang::quo_squash(NULL)

    sz <- ifelse(is_spatial_plot,
                 45,
                 10)

    size_expression <- expression(I(sz))
    sizes_expression <- NULL
  }







  opac <- 0.7

  if(is_spatial_plot){
    opac <- 1

    zoom <- 0

    # total_range_lon <- diff(range(plot_data[ , as.character(shiny::isolate(selector_inputs$xvar)), drop = TRUE],
    #                           na.rm = TRUE))
    #
    # if(total_range_lon <= 180 & total_range_lon > 90){
    #   zoom <- 1
    # } else if(total_range_lon <= 90 & total_range_lon > 45){
    #   zoom <- 1.5
    # } else if(total_range_lon <= 45){
    #   zoom <- 2
    # }


    geo_def <-  list(style = ifelse(is.null(mapstyle),
                                    "basic",
                                    mapstyle),
                     zoom = zoom,
                     center = list(
                       lon = ~ median(plot_data[ , as.character(shiny::isolate(selector_inputs$xvar)), drop = TRUE]),
                       lat = ~ median(plot_data[ , as.character(shiny::isolate(selector_inputs$yvar)), drop = TRUE])
                     ))

  } else {
    geo_def <- list()
  }



  output$scatterselect <- plotly::renderPlotly({

    p <-  rlang::eval_tidy(
      rlang::quo_squash(
        rlang::quo({

          pnew <- { if(is_spatial_plot){
            plotly::plot_mapbox(data = plot_data,
                                source = "scatterselect",
                                marker = list(
                                  allowoverlap = TRUE))
          } else {
            plotly::plot_ly(data = plot_data,
                            type = "scattergl",
                            mode = "markers",
                            source = "scatterselect",
                            symbols = c("circle", "star-triangle-down"),
                            symbol = if(has_flag_column){
                              ~as.numeric(!.dcrflag)}
                            else{NULL})
          }
          } %>%
            plotly::add_markers(x = ~ !!shiny::isolate(selector_inputs$xvar),
                                y = ~ !!shiny::isolate(selector_inputs$yvar),
                                type = 'scattergl',

                                size = eval(size_expression),
                                sizes = eval(sizes_expression),



                                color = ~as.factor(.dcrindex),
                                colors = col_value_vector,

                                name = ~as.factor(.dcrindex),
                                text = ~.dcrkey,
                                customdata = ~.dcrkey,

                                showlegend = TRUE,
                                marker = list(opacity = opac,
                                              allowoverlap = TRUE
                                              # size = eval(size_expression),
                                              # sizes = c(10, 100)),
                                              # sizes = eval(sizes_expression)
                                              ),
                                unselected = list(marker = list(opacity = opac))
                                ) %>%

            plotly::layout(showlegend = TRUE,
                           dragmode = "lasso",
                           mapbox = geo_def,
                           updatemenus = list(
                             list(
                               type = "buttons",
                               direction = "right",
                               xanchor = 'center',
                               yanchor = "top",
                               pad = list('r'= 0, 't'= 10, 'b' = 10),
                               x = 0.5,
                               y = 1.2,
                               buttons = list(
                                 list(method = "restyle",
                                      args = list(list(mode = "markers"),
                                                  as.list(seq_len(n_groups_original)-1)),
                                      # args = list(mode = "markers"),
                                      args2 = list(list(mode = "lines+markers",
                                                        line = list(width = 1)),
                                                   as.list(seq_len(n_groups_original)-1)
                                                   ),
                                      label = "Toggle Lines")
                               )
                             ))
            )  %>%
            plotly::config(displaylogo = FALSE,
                           modeBarButtonsToRemove = list("hoverCompareCartesian")) %>%
            plotly::event_register(event = "plotly_afterplot") %>%
            plotly::event_register(event = "plotly_deselect") %>%
            plotly::event_register(event = "plotly_click") %>%
            plotly::event_register(event = "plotly_selected") %>%
            htmlwidgets::onRender(jsfull,
                                  data = list(x = "tracemap",
                                              ns = sessionval)) %>%
            plotly::toWebGL()

        })
      )
    ) #\ eval_tidy

    # re-add outlier traces on "Plot!" click
    if(length(shiny::isolate(sel_points$df$keys)) > 0){

      add_data <- dplyr::left_join(
        shiny::isolate(sel_points$df),
        plot_data,
        by = c('keys' = '.dcrkey')) %>%
        dplyr::rename(.dcrkey = .data$keys)

      # add_data <- plot_data[plot_data$.dcrkey %in% sel_points$df$keys, ]

      add_color <- "black"

      p <- rlang::eval_tidy(
        rlang::quo_squash(
          rlang::quo({

            plotly::add_trace(p,
                              data = add_data,
                              x = ~ !!shiny::isolate(selector_inputs$xvar),
                              y = ~ !!shiny::isolate(selector_inputs$yvar),
                              name = "O",
                              mode = "markers",
                              customdata = ~.dcrkey,
                              text = ~.dcrkey,
                              showlegend = TRUE,
                              marker =
                                if(is_spatial_plot){
                                  list(
                                    symbol = "hospital",
                                    size = 12,
                                    allowoverlap = TRUE)

                                  # size = 12)
                                } else {
                                  list(color = add_color,
                                       symbol = "x",
                                       size = 12
                                  )
                                },
                              unselected = list(marker = list(opacity = 1)))

            #
            #       purrr::reduce(.x = split(add_data, f = add_data$selection_count),
            #                     .f = function(oplot, spdf) {
            #
            #                       plotly::add_trace(oplot,
            #                                         data = spdf,
            #                                         x = ~ !!shiny::isolate(selector_inputs$xvar),
            #                                           y = ~ !!shiny::isolate(selector_inputs$yvar),
            #                                           size = eval(size_expression),
            #                                           name = "O",
            #                                           type = "scattergl",
            #                                           mode = "markers",
            #                                           legendgroup = "out",
            #                                           customdata = ~.dcrkey,
            #                                           text = ~.dcrkey,
            #                                           showlegend = TRUE,
            #                                           marker =
            #                                             if(is_spatial_plot){
            #                                               list(color = add_color,
            #                                                    opacity = 1)
            #                                             } else {
            #                                               list(color = add_color
            #                                                    # opacity = 1
            #                                                    # line = list(color = add_color,
            #                                                                # width = 2)
            #                                               )
            #                                             },
            #                         # }
            #                     # ,
            #                                           unselected = list(marker = list(opacity = 1)))},
            #                       .init = shiny::isolate(p)
            # )
          })
        )
      ) #\ eval_tidy
    } # /if
    return(p)

  }) # / renderPlotly
}
