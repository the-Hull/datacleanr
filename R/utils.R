#' Initial checks for data set
#' @param dframe dframe supplied to \code{dcr_app}
dcr_checks <- function(dframe) {

  # check if dframe is string or tibble

  classes <- c("fs_path", "character", "tbl", "data.frame", "data.table")

  from_file <- FALSE
  file_path <- NULL

  if (rlang::inherits_any(dframe, classes[c(1, 2)])) {
    if (!identical(
      "rds",
      tolower(tools::file_ext(dframe))
    )) {
      stop("Please provide a *.Rds file.")
    }

    # check if file exists, if not, error out
    if (!file.exists(dframe)) {
      stop("File does not exist.")
    }




    file_path <- dframe
    dframe <- readRDS(dframe)
    from_file <- TRUE
  }




  if (!rlang::inherits_any(dframe, classes[-c(1, 2)])) {
    msg <- paste0(
      "Please provide a data.frame, tibble, or data.table",
      ifelse(from_file, " in your file path", ""),
      "."
    )

    stop(msg)
  }


  if (rlang::inherits_any(dframe, "data.table")) {
    dframe <- dplyr::as_tibble(dframe)
  }


  if (utils::hasName(dframe, ".dcrflag")) {
    if (!rlang::inherits_only(dframe$.dcrflag, "logical")) {
      msg <- paste0("Detected column .dcrflag - Please ensure it is of class logical (i.e. TRUE/FALSE).")

      stop(msg)
    }
  }



  if (utils::hasName(dframe, ".annotation")) {
    msg <- paste0("A .annotation column already exists. Please rename it (e.g., to .dcrflag) to prevent any clashes and try again.")

    stop(msg)
  }




  n_points_warn <- 1.5e6
  if (nrow(dframe) >= n_points_warn) {
    message(paste("Data has more than", n_points_warn, "observations. \n"))
    message("Consider breaking it up into smaller chunks with a split-check-combine approach.\n ")
    message("See ?dcr_app() for details.\n ")
    message(".. delaying launch (5s) \n")
    if (interactive()) {
      Sys.sleep(5)
    }
  }


  return(list(
    dataset = dframe,
    file_path = file_path
  ))
}


#' Identify columns carrying non-numeric values
#' @param x data.frame
#' @return logical, is column in x non-numeric?
get_factor_cols_idx <- function(x) {
  if (ncol(x) < 1) {
    stop("Error: supply data set with multiple columns (data.frame, tbl, data.table)!")
  }
  return(!unname(unlist(lapply(x, is.numeric))))
}


#' Navbar with Input
#'
#' Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
#' want to add an input to the navbar
#'
#' @param ... Regular Navbar elements, like tabPanel
#' @param inputs shiny ui inputs
#' @return Navbar function that allows adding inputs
#' @source \url{https://github.com/daattali/advanced-shiny/tree/master/navbar-add-text}
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form
  )
  navbar
}

#' Applies grouping to data set conditionally
#' @param df data frame
#' @param group supply reactive output from group selector
#' @return returns df either grouped or not
apply_data_set_up <- function(df, group) {
  if (!is.null(group) & !is.character(group)) {
    stop("group must be NULL or supplied as a string.")
  }

  if (is.null(group)) {
    return(df)
  } else {
    group <- rlang::syms(group)
    dplyr::group_by(df, !!!group)
  }
}





#' check if a filter statement is valid
#' @param df data frame / tibble to be filtered
#' @param statement character string,
#' @return logical, did filter statement work?
check_individual_statement <- function(df, statement) {
  is.error <- function(x) inherits(x, "try-error")

  condition_worked <- tryCatch(
    {
      expr <- str2expression(statement)

      filter_try <- base::subset(
        df,
        eval(expr)
      )
      return(TRUE)
    },
    error = function(cond) {
      return(FALSE)
    }
  )
  return(condition_worked)
}






#' Split data.frame/tibble based on grouping
#' @param dframe data.frame
#' @return list of data frames
split_groups <- function(dframe) {
  if (!all(c(".dcrindex") %in% names(dframe))) {
    dframe <- dplyr::mutate(dframe,
      .dcrindex = dplyr::cur_group_id()
    )
  }

  outlist <- base::split(
    dframe,
    f = as.factor(dframe$.dcrindex)
    # f = as.factor(dplyr::cur_group_id(dframe))
  )

  return(outlist)
}


#' Apply filter based on a statement, scoped to \code{dplyr} groups
#'
#' @param dframe data.frame/tbl, grouped or ungrouped
#' @param statement character, statement for filtering (only VALID expressions; use \code{check_individual_statement} to grab only valid.
#' @param scope_at numeric, group indices to apply filter statements to
#' @return List, containing item \code{filtered_df}, a \code{data.frame} filtered based on statements and scope.
filter_scoped <- function(dframe, statement, scope_at = NULL) {
  scope_at <- unlist(scope_at)


  n_groups <- dplyr::n_groups(dframe)
  var_groups <- rlang::syms(dplyr::group_vars(dframe))

  # statement <- rlang::enquos(statement)
  statement <- parse(text = statement)


  # statement_check <- check_individual_statement(df = dframe,
  #                                               statement = statement)
  #
  #
  #
  # if(isFALSE(statement_check)){
  #
  #
  #     return(list(succeeded = statement_check
  #                 # ,
  #                 # filtered_df = NULL,
  #                 # statement_strings = NULL,
  #                 # statement_strings_grouped = NULL,
  #                 # statement_strings_ungrouped = NULL
  #     ))
  #
  #
  # } else if(isTRUE(statement_check)){


  # if(rlang::is_missing(scope_at)){
  #     scope_at <- NULL
  # }

  if (is.null(scope_at) | n_groups == 1) {


    # df passed in function might be grouped previously (i.e. in call to dcr_app)
    # needs ungrouping first
    filtered_df <- dplyr::group_by(
      dplyr::filter(dplyr::ungroup(dframe),
        eval(statement),
        na.rm = TRUE
      ),
      !!!var_groups
    )

    #
    #             filt_expr <- paste0('dplyr::group_by(dplyr::filter(dplyr::ungroup(dframe),',
    #                                 statement,
    #                                 '), !!! var_groups)')
    #             filtered_df <- eval(parse(text = filt_expr))
  } else if (length(scope_at) == n_groups) {
    # filt_expr <- paste0('dplyr::filter(dframe,',
    #                     statement,
    #                     ')')
    # filtered_df <- eval(parse(text = filt_expr))
    #

    filtered_df <- dplyr::filter(dframe, eval(statement), na.rm = TRUE)
  } else if (n_groups > 1 &
    length(scope_at) >= 1 &
    length(scope_at) != n_groups) {

    # filt_expr <- glue::glue(
    #     "
    #     dplyr::bind_rows(purrr::map_at(
    #         .x = split_groups(dframe),
    #         .at = {{deparse(scope_at)}},
    #         .f = function(x){dplyr::filter(x,
    #         {{statement}}
    #         )}
    #         ))
    #     ",
    #     .open = "{{",
    #     .close = "}}"
    # )
    # filtered_df <- eval(str2expression(filt_expr))


    filtered_df <- dplyr::bind_rows(purrr::map_at(
      .x = split_groups(dframe),
      .at = scope_at,
      .f = function(x) {
        dplyr::filter(x,
          eval(statement),
          na.rm = TRUE
        )
      }
    ))
  }
  return(list(
    # succeeded = statement_check,
    filtered_df = filtered_df
    # ,
    # expression_string = filt_expr
  ))
  # }
}




#' Filter / Subset data \code{dplyr}-groupwise
#'
#' \code{filter_scoped_df} subsets rows of a data frame based on grouping structure
#' (see \code{\link[dplyr]{group_by}}). Filtering statements are provided in a separate \code{tibble}
#' where each row represents a combination of a logical expression and a list of groups
#' to which the expression should be applied to corresponding to see indices from
#' \code{\link[dplyr]{cur_group_id}}).
#'
#' @param dframe A grouped or ungrouped \code{tibble} or \code{data.frame}
#' @param condition_df A \code{tibble} with two columns; \code{condition_df[ ,1]} with
#'  \code{character} strings which evaluate to valid logical expressions applicable in
#'  \code{\link{subset}} or \code{\link[dplyr]{filter}}, and \code{condition_df[ ,2]},
#'  a list-column with group scoping levels (\code{numeric}) or \code{NULL} for
#'  unscoped filtering. If all groups are given for a statement, the operation is
#'  the same as for a grouped \code{data.frame} in \code{\link[dplyr]{filter}}.
#'
#' @details This function is applied in the "Filtering" tab of the \code{datacleanr} app,
#' and applied in the reproducible code recipe in the "Extract" tab.
#'  Note, that multiple checks for valid statements are performed in the app (and only valid operations
#'  printed in the "Extract" tab). It is therefore not advisable to manually alter this code or use
#'  this function interactively.
#'
#'
#' @return An object of the same type as \code{dframe}. The output is a subset of
#'  the input, with groups and rows appearing in the same order, and an additional column
#'  \code{.dcrindex} representing the group indices.
#'  The output may have less groups as the input, depending on subsetting.
#'
#' @export
#'
#' @examples
#' # set-up condition_df
#' cdf <- dplyr::tibble(
#'   statement = c(
#'     "Sepal.Width > quantile(Sepal.Width, 0.1)",
#'     "Petal.Width > quantile(Petal.Width, 0.1)",
#'     "Petal.Length > quantile(Petal.Length, 0.8)"
#'   ),
#'   scope_at = list(NULL, NULL, c(1, 2))
#' )
#'
#'
#' fdf <- filter_scoped_df(
#'   dplyr::group_by(
#'     iris,
#'     Species
#'   ),
#'   condition_df = cdf
#' )
#'
#' # Example of invalid expression:
#' # column 'Spec' does not exist in iris
#' # "Spec == 'setosa'"
filter_scoped_df <- function(dframe, condition_df) {
  if (!rlang::inherits_any(dframe, class = c("data.frame", "tbl_df", "tbl"))) {
    stop("Please provide a data.frame or tibble as dframe")
  }
  if (!rlang::inherits_any(condition_df, class = c("tbl_df", "tbl"))) {
    stop("Please provide a tibble as condition_df")
  }

  if (!rlang::inherits_any(condition_df[[2]], c("list", NULL))) {
    stop("Please provide a list column or NULL in condition_df[ ,2]")
  }


  if (nrow(condition_df) == 0) {
    return(dframe)
  }




  # keep_checks <- which(checks)
  for (j in seq_len(nrow(condition_df))) {

    # keep_idx <- keep_checks[j]
    if (j == 1) {
      tmp <- filter_scoped(
        dframe = dframe,
        statement = condition_df[j, 1, drop = TRUE],
        scope_at = condition_df[j, 2, drop = TRUE]
      )$filtered_df
    } else {
      tmp <- filter_scoped(
        dframe = tmp,
        statement = condition_df[j, 1, drop = TRUE],
        scope_at = condition_df[j, 2, drop = TRUE]
      )$filtered_df
    }
  }
  return(tmp)

  # }
  # return(NULL)
}





#' extend brewer palette
#' @param n numeric, number of colors
#' @return color vector of length n
extend_palette <- function(n) {
  pal <- "Dark2"
  # pal <- "Accent"
  # pal <- "Set1"

  if (n < 3) {
    cols <- RColorBrewer::brewer.pal(3, pal)[1:n]
  }
  else if (n >= 3 && n <= 8) {
    cols <- RColorBrewer::brewer.pal(n, pal)
  } else {
    cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, pal))(n)
  }

  return(cols)
}



#' Handle selection of outliers (with select - unselect capacity)
#' @param sel_old_df data.frame of selection info
#' @param sel_new data.frame, event data from plotly, must have column \code{customdata}
#' @return updated selection data frame
handle_sel_outliers <- function(sel_old_df, sel_new) {
  if (NROW(sel_new) > 0) {
    new_df <- data.frame(
      keys = as.integer(sel_new$customdata),
      selection_count = ifelse(nrow(sel_old_df) > 0,
        max(sel_old_df$selection_count) + 1,
        1
      ),
      .annotation = "",
      stringsAsFactors = FALSE
    )


    # check which points may be duplicates in selection
    if (nrow(sel_old_df) > 0 &&
      NROW(intersect(sel_old_df$keys, new_df$keys)) > 0) {
      sel_out <- rbind(sel_old_df, new_df)

      dup_idcs_lgl <- duplicated(sel_out[, "keys"]) |
        duplicated(sel_out[, "keys"], fromLast = TRUE)


      sel_out <- sel_out[!dup_idcs_lgl, ]


      if (length(unique(sel_out$selection_count)) == 1) {
        sel_out$selection_count <- 1
      }

      # no duplicates
    } else {
      sel_out <- rbind(sel_old_df, new_df)
    }
    # /    if (!is.null(sel_new))
  } else {
    sel_out <- sel_old_df
  }
  return(sel_out)
}



# #' Provide trace ids to set to invisible
# #' @param max_groups numeric, number of groups in grouptable
# #' @param selected_groups groups highlighted in grouptable
# #' @details Provides the indices (JS notation, starting at 0) for indices
# #' that are set to \code{visible = 'legendonly'} through \code{plotly.restyle}
# # hide_trace_idx <- function(max_groups, selected_groups){
#
#     all_row_ids <- seq_len(max_groups)
#
#     if(is.null(selected_groups)){
#         deselect_ids <- NULL
#     } else if(length(selected_groups) < max_groups){
#         # deselect_ids <- all_row_ids[all_row_ids %nin% selected_groups] - 1
#         deselect_ids <- all_row_ids[all_row_ids %nin% selected_groups]
#     } else if(length(selected_groups) == max_groups){
#         deselect_ids <- NULL
#     }
#
#     return(deselect_ids)
#
# }


#' Provide trace ids to set to invisible
#' @param trace_map matrix, with cols trace name (col 1), trace id (col 2)
#' @param max_groups numeric, number of groups in grouptable
#' @param selected_groups groups highlighted in grouptable
#' @details Provides the indices (JS notation, starting at 0) for indices
#' that are set to \code{visible = 'legendonly'} through \code{plotly.restyle}
hide_trace_idx <- function(trace_map, max_groups, selected_groups) {


  # grab only non-outlier traces


  # if exists, drop outlier trace
  outlier_id <- which(trace_map[, 1] == "O")

  if (length(outlier_id) > 0) {
    trace_map <- trace_map[-outlier_id, ]
  }


  if (is.null(selected_groups)) {
    deselect_ids <- NULL
  } else if (length(selected_groups) < max_groups) {
    # deselect_ids <- all_row_ids[all_row_ids %nin% selected_groups] - 1
    # deselect_ids <- trace_map[-selected_groups, 2]

    # match group ids with index ids (important when NA groups present)
    selected_index_in_tracemap_lgl <- trace_map[, 1] %nin% selected_groups
    deselect_ids <- trace_map[selected_index_in_tracemap_lgl, 2]
  } else if (length(selected_groups) == max_groups) {
    deselect_ids <- NULL
  }

  return(deselect_ids)
}


#' Wrapper for adjusting axis lims and hiding traces
#'
#' @param source_id character, plotly source id
#' @param session session object
#' @param dframe data frame/tibble (grouped/ungrouped)
#' @param scaling numeric, 1 +/- scaling applied to x lims for xvar and yvar
#' @param xvar character, name of xvar, must be in dframe
#' @param yvar character, name of yvar, must be in dframe
#' @param trace_map matrix, with columns for trace name (col 1) and trace id (col 2)
#' @param max_id_group_trace numeric, max id of plotly trace from original data (not outlier traces)
#' @param input_sel_rows numeric, input from DT grouptable
#' @param flush character, \code{plotlyProxy} settings
#'
#' @return Used for it's side effect - no return
handle_restyle_traces <- function(source_id,
                                  session,
                                  dframe,
                                  scaling = 0.05,
                                  xvar,
                                  yvar,
                                  trace_map,
                                  max_id_group_trace,
                                  input_sel_rows,
                                  flush = TRUE) {
  if (any(c(xvar, yvar) %nin% names(dframe))) {
    stop("xvar or yvar not in dframe, please try again")
  }


  pproxy <- plotly::plotlyProxy(source_id, session,
    deferUntilFlush = flush
  )


  # hide/show traces

  deselect_trace_id <- hide_trace_idx(trace_map,
    # max_groups = max_id_group_trace + 1,
    max_groups = max_id_group_trace,
    selected_groups = input_sel_rows
  )



  if (!is.null(deselect_trace_id)) {
    plotly::plotlyProxyInvoke(
      pproxy,
      "restyle",
      list(visible = TRUE)
    )

    plotly::plotlyProxyInvoke(
      pproxy,
      "restyle",
      list(visible = "legendonly"),
      # deselect_trace_id)
      as.list(deselect_trace_id)
    )
  } else {
    plotly::plotlyProxyInvoke(
      pproxy,
      "restyle",
      list(visible = TRUE)
    )
  }


  # reset limits

  lims <- calc_limits_per_groups(dframe,
    group_index = input_sel_rows,
    xvar = xvar,
    yvar = yvar,
    scaling = scaling
  )






  plotly::plotlyProxyInvoke(
    pproxy,
    "relayout",
    list(
      yaxis = list(range = lims$ylim),
      xaxis = list(range = lims$xlim)
    )
  )
}






# #' Handle Add traces
# #'
# #' @param sp selected points
# #' @param dframe plot data
# #' @param ok reactive, old keys
# #' @param selectors reactive input selectors
# #' @param max_trace numeric, previous max trace id
# #' @param source plotly source
# #' @param session active session
# handle_add_traces <- function(sp, dframe, ok, selectors, max_trace, source = "scatterselect", session){
#
#     add_color <- "black"
#
#     is_spatial_plot <- identical(c(as.character(selectors$xvar),
#                                    as.character(selectors$yvar)),
#                                  c("lon", "lat"))
#
#
#     if(is_spatial_plot){
#         geo_def <-  list(style = "light")
#     } else {
#         geo_def <- list()
#     }
#
#
#     if(length(sp$df$keys) > 0){
#
#         # check if selection is new
#         if(!identical(ok(),
#                       sp$df$keys)){
#
#             max_sel_count <- max(sp$df$selection_count)
#
#             last_sel_keys <- as.integer(sp$df$keys[sp$df$selection_count == max_sel_count])
#             # grab points
#             add_points <- dframe()[last_sel_keys, ]
#             # handle plotly - only adds trace for array > 2L
#             if(nrow(add_points) == 1){
#                 add_points <- rbind(add_points, add_points)
#             }
#
#             z <- zvar_toggle(selectors$zvar, df = add_points)
#
#             if(is_spatial_plot){
#
#                 plotly::plotlyProxy(source, session) %>%
#                     plotly::plotlyProxyInvoke(
#                         "addTraces",
#                         list(
#                             lon = add_points[ , as.character(selectors$xvar), drop = TRUE],
#                             lat = add_points[ , as.character(selectors$yvar), drop = TRUE],
#                             customdata = add_points[ , ".dcrkey", drop = TRUE],
#                             text = add_points[ , ".dcrkey", drop = TRUE],
#                             legendgroup = "out",
#                             size = z,
#                             sizes = c(25,100),
#                             type = "scattermapbox",
#                             mode = "markers",
#                             name = "O",
#                             opacity = 1,
#                             marker = list(
#                                 color = add_color,
#                                 opacity = 1),
#                             unselected = list(marker = list(opacity = 1)),
#                             showlegend = list(TRUE)
#                         )
#                     )
#
#             } else {
#
#
#                 plotly::plotlyProxy(source, session) %>%
#                     plotly::plotlyProxyInvoke(
#                         "addTraces",
#                         list(
#                             x = add_points[ , as.character(selectors$xvar), drop = TRUE],
#                             y = add_points[ , as.character(selectors$yvar), drop = TRUE],
#                             size = z,
#                             type = "scattergl",
#                             mode = "markers",
#                             name = "O",
#                             customdata = add_points[ , ".dcrkey", drop = TRUE],
#                             text = add_points[ , ".dcrkey", drop = TRUE],
#                             legendgroup = "out",
#                             marker = list(
#                                 color = add_color,
#                                 opacity = 1),
#                             unselected = list(marker = list(opacity = 1)),
#                             selected = list(marker = list(opacity = 1)),
#                             showlegend = TRUE
#                         ),
#                         max_trace+1
#                     )
#             }
#             ok(sp$df$keys)
#
#
#         } else {
#
#             cat("WE DOWN\n")
#
#         }
#     }
#     return(ok)
#
# }
#




#' Handle outlier trace
#'
#' Single outlier trace is added to plotly; interactive select/deselect
#' was implemented by adjusting \code{selected_points}, and subsequently adding, or deleting+adding
#' the (modified) trace at the end of the existing JS data array. Requires tracemap with
#' trace names and corresponding indices.
#' Simple check for re-execution was implemented by passing on the selection keys to compare against
#' on pertinent \code{plotly_event}.
#'
#' @param sp selected points
#' @param dframe plot data
#' @param ok reactive, old keys
#' @param selectors reactive input selectors
#' @param trace_map numeric, max trace id
#' @param source plotly source
#' @param session active session
#'
handle_add_outlier_trace <- function(sp,
                                     dframe,
                                     ok,
                                     selectors,
                                     trace_map,
                                     source = "scatterselect",
                                     session) {



  # get info on outlier trace
  outidx <- as.numeric(trace_map[which(trace_map[, 1] == "O"), 2])

  # do nothing when selections haven't changed (e.g. empty lasso)
  if (identical(ok(), sp$df$keys) & NROW(outidx) > 0) {
    return(ok)
  }

  pprox <- plotly::plotlyProxy(source, session)



  # handle case when removing all points from current trace (i.e. going back to normal)
  if (NROW(outidx) > 0 & nrow(sp$df) == 0) {
    plotly::plotlyProxyInvoke(
      pprox,
      "deleteTraces",
      outidx
    )
  }

  # general config
  add_color <- "black"

  # handle spatial config
  is_spatial_plot <- identical(
    c(
      as.character(selectors$xvar),
      as.character(selectors$yvar)
    ),
    c("lon", "lat")
  )

  if (is_spatial_plot) {
    geo_def <- list(style = "light")
  } else {
    geo_def <- list()
  }

  # handle case when points exist in selection
  if (length(sp$df$keys) > 0) {
    add_points <- dframe()[sp$df$keys, ]
    # handle plotly - only adds trace for array > 2L
    if (nrow(add_points) == 1) {
      add_points <- rbind(add_points, add_points)
    }

    # handle size
    # currently using fixed size of 12 for all outlier markers
    # may change in future again
    # z <- zvar_toggle(selectors$zvar, df = add_points)

    if (is_spatial_plot) {


      # for first trace
      if (length(outidx) == 0) {
        plotly::plotlyProxyInvoke(
          pprox,
          "addTraces",
          list(
            lon = add_points[, as.character(selectors$xvar), drop = TRUE],
            lat = add_points[, as.character(selectors$yvar), drop = TRUE],
            customdata = add_points[, ".dcrkey", drop = TRUE],
            text = add_points[, ".dcrkey", drop = TRUE],
            type = "scattermapbox",
            mode = "markers",
            name = "O",
            opacity = 1,
            marker = list(
              symbol = "hospital",
              size = 12,
              allowoverlap = TRUE,
              color = "black",
              opacity = 1
            ),
            selected = list(marker = list(opacity = 1)),
            unselected = list(marker = list(opacity = 1)),
            showlegend = list(TRUE)
          )
        )

        # for second trace
      } else if (length(outidx) > 0) {

        # delete initial outlier trace
        plotly::plotlyProxyInvoke(
          pprox,
          "deleteTraces",
          outidx
        )

        # re-add selected points
        plotly::plotlyProxyInvoke(
          pprox,
          "addTraces",
          list(
            lon = add_points[, as.character(selectors$xvar), drop = TRUE],
            lat = add_points[, as.character(selectors$yvar), drop = TRUE],
            customdata = add_points[, ".dcrkey", drop = TRUE],
            text = add_points[, ".dcrkey", drop = TRUE],
            # legendgroup = "out",
            # size = z,
            # sizes = c(25,100),
            type = "scattermapbox",
            mode = "markers",
            name = "O",
            opacity = 1,
            marker = list(
              symbol = "hospital",
              size = 12,
              color = "black",
              allowoverlap = TRUE,
              opacity = 1
            ),
            selected = list(marker = list(opacity = 1)),
            unselected = list(marker = list(opacity = 1)),
            showlegend = list(TRUE)
          )
        )
      }
    } else {

      # for first trace
      if (length(outidx) == 0) {
        plotly::plotlyProxyInvoke(
          pprox,
          "addTraces",
          list(
            x = add_points[, as.character(selectors$xvar), drop = TRUE],
            y = add_points[, as.character(selectors$yvar), drop = TRUE],
            type = "scattergl",
            mode = "markers",
            name = "O",
            customdata = add_points[, ".dcrkey", drop = TRUE],
            text = add_points[, ".dcrkey", drop = TRUE],
            marker = list(
              symbol = "x",
              size = 12,
              color = add_color,
              opacity = 1
            ),
            unselected = list(marker = list(opacity = 1)),
            selected = list(marker = list(opacity = 1)),
            showlegend = TRUE
          )
        )

        # for second trace
      } else if (length(outidx) > 0) {

        # delete initial outlier trace
        plotly::plotlyProxyInvoke(
          pprox,
          "deleteTraces",
          outidx
        )

        # re-add selected points
        plotly::plotlyProxyInvoke(
          pprox,
          "addTraces",
          list(
            x = add_points[, as.character(selectors$xvar), drop = TRUE],
            y = add_points[, as.character(selectors$yvar), drop = TRUE],
            type = "scattergl",
            mode = "markers",
            name = "O",
            customdata = add_points[, ".dcrkey", drop = TRUE],
            text = add_points[, ".dcrkey", drop = TRUE],
            marker = list(
              symbol = "x",
              size = 12,
              color = add_color,
              opacity = 1
            ),
            unselected = list(marker = list(opacity = 1)),
            selected = list(marker = list(opacity = 1)),
            showlegend = TRUE
          )
        )
      }
    }
    # update the old keys
    ok(sp$df$keys)
  }
  return(ok)
}

# helpers ------------

# zvar_toggle <- function(zvar, df){
#     if(nchar(zvar)>0){
#         z <- df[ , as.character(zvar), drop = TRUE]
#     } else {
#         z <- NULL
#     }
#     return(z)
# }


`%nin%` <- Negate(`%in%`)

# drop null or empty values from a list
drop_empty <- function(l) {
  l[vapply(l, shiny::isTruthy, logical(1))]
}



#' Make grouping overview table
#'
#' @param dframe data.frame
#' @importFrom rlang .data
#'
#' @return tibble with one row per group
make_group_table <- function(dframe) {
  group_table <- dplyr::summarise(dframe,
    `Group` = dplyr::cur_group_id(),
    `n obs.` = dplyr::n()
  )
  group_table <- dplyr::relocate(group_table, .data$Group)
}


#' Return x and y limits of "group-subsetted" dframe
#'
#' @description Used for adjusting layout of plotly plot based on selected
#' groups in \code{group_selector_table}; currently used in viz tab
#'
#' @param dframe dataframe/tibble, grouped/ungrouped
#' @param group_index numeric, group indices for which to return lims
#' @param xvar character, name of x var for plot (must exist in dframe)
#' @param yvar character, name of y var for plot (must exist in dframe)
#' @param scaling numeric, 1 +/- \code{scaling} times limits
#'
#' @return list with xlim and ylim
calc_limits_per_groups <- function(dframe, group_index, xvar, yvar, scaling = 0.02) {
  if (is.null(group_index)) {
    group_index <- seq_len(dplyr::n_groups(dframe))
  }

  if (!rlang::inherits_any(dframe, c("data.frame", "tibble"))) {
    stop("need data.frame or tibble to proceed")
  }

  if (dplyr::n_groups(dframe) == 1 &
    length(group_index) == 1) {
    if (group_index > 1) {
      warning("supplied group index outside of group stack, setting to 1")
      group_index <- 1
    }
  }

  if (any(c(xvar, yvar) %nin% names(dframe))) {
    stop("xvar or yvar not in dframe, please try again")
  }

  group_rows <- unlist(
    dplyr::group_data(
      dframe
    )$.rows[group_index]
  )

  if (any(
    rlang::inherits_any(dframe[, xvar, drop = TRUE], "POSIXct"),
    rlang::inherits_any(dframe[, yvar, drop = TRUE], "POSIXct")
  )) {
    posix_scale <- function(timestamp) {
      if (scaling == 0) {
        scaling <- 0.01
      }

      total_range <- as.numeric(range(timestamp, na.rm = TRUE))
      # offset <- (diff(total_range)) * c(-1,1) + scaling
      offset <- (diff(total_range) * scaling) * c(-1, 1)


      lim <- range(timestamp, na.rm = TRUE) +
        offset
      return(lim)
    }

    if (rlang::inherits_any(dframe[, xvar, drop = TRUE], "POSIXct")) {
      xlim <- posix_scale(dframe[group_rows, xvar, drop = TRUE])
    }
    if (rlang::inherits_any(dframe[, yvar, drop = TRUE], "POSIXct")) {
      ylim <- posix_scale(dframe[group_rows, yvar, drop = TRUE])
    }
  }

  if (!exists("xlim")) {
    if (rlang::inherits_any(dframe[, xvar, drop = TRUE], c("character", "factor"))) {
      xlim <- NULL
    }
    else {
      xlim <- (diff(range(dframe[group_rows, xvar, drop = TRUE],
        na.rm = TRUE
      )) * scaling) * c(-1, 1) +
        range(dframe[group_rows, xvar, drop = TRUE],
          na.rm = TRUE
        )
    }
  }

  if (!exists("ylim")) {
    if (rlang::inherits_any(dframe[, yvar, drop = TRUE], c("character", "factor"))) {
      ylim <- NULL
    }
    else {
      ylim <- (diff(range(dframe[group_rows, yvar, drop = TRUE],
        na.rm = TRUE
      )) * scaling) * c(-1, 1) +
        range(dframe[group_rows, yvar, drop = TRUE],
          na.rm = TRUE
        )
    }
    # ylim <- range(dframe[group_rows, yvar, drop = TRUE],
    #               na.rm = TRUE) *
    #     c(1 - scaling, 1 + scaling)
  }

  return(list(xlim = xlim, ylim = ylim))
}

#' Wrapper for saving files
#'
#' @param save_dir character, selected save dir
#' @param input_filepath character, original file path to folder
#' @param suffix character, e.g. 'CLEAN' or 'cleaning_script'
#' @param ext character, file extension, no dot!!
#'
#' @return OS-conform file path for saving
#'
make_save_filepath <- function(save_dir, input_filepath, suffix, ext) {
  file_name <- fs::path_ext_remove(fs::path_file(input_filepath))
  file_name <- paste0(file_name, "_", suffix)

  out_filepath <- fs::path(save_dir, file_name, ext = ext)

  return(out_filepath)
}

# methods -----------------------------------------------------------------


#' Method for printing dcr_code output
#'
#' @param x character, code  output from \code{dcr_app}
#' @param ... additional arguments passed to \code{cat}
#' @method print dcr_code
print.dcr_code <- function(x, ...) {
  cat(x, ...)
}
