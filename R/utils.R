#' Initial checks for data set
#'
#' @param dframe dframe supplied to \code{dcr_app}
#'
#' @return Nothing - just check!
dcr_checks <- function(dframe){

    if(!rlang::inherits_any(dframe, c("tbl", "data.frame", "data.table"))){

        stop("Please provide a data.frame, tibble, or data.table")
    }

    if(nrow(dframe) > 10000){

        warning(paste("Data set has over", nrow(dframe), "observations. Consider breaking it up into smaller chunks."))

    }


}


#' Identify columns carrying non-numeric values
#'
#' @param x data.frame
#'
#' @return logical, is column in x non-numeric?
#'
get_factor_cols_idx <- function(x){

    if(ncol(x) < 1){
        stop("Error: supply data set with multiple columns (data.frame, tbl, data.table)!")
    }
    return(!unname(unlist(lapply(x, is.numeric))))
}


#' Navbar with Input
#'
#' Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
#' want to add an input to the navbar
#'
#'
#' @param ... Regular Navbar elements, like tabPanel
#' @param inputs shiny ui inputs
#'
#' @return Navbar function that allows adding inputs
#'
#' @author Dean Attali
#' @source \url{https://github.com/daattali/advanced-shiny/tree/master/navbar-add-text}
#'
navbarPageWithInputs <- function(..., inputs) {
    navbar <- shiny::navbarPage(...)
    form <- shiny::tags$form(class = "navbar-form", inputs)
    navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
        navbar[[3]][[1]]$children[[1]], form)
    navbar
}

#' Applies grouping to data set conditionally
#'
#' @param df data frame
#' @param group supply reactive output from group selector
#'
#' @return returns df either grouped or not
#'
apply_data_set_up <- function(df, group){

    if(!is.null(group) & !is.character(group)){

        stop("group must be NULL or supplied as a string.")
    }

    if(is.null(group)){
        return(df)
    } else {
        group <- rlang::syms(group)
        dplyr::group_by(df, !!! group)
    }
}





#' check if a filter statement is valid
#'
#' @param df data frame / tibble to be filtered
#' @param statement character string,
#'
#' @return logical, did filter statement work?
#'
check_individual_statement <- function(df, statement){

    is.error <- function(x) inherits(x, "try-error")

    condition_worked <- tryCatch({

        expr <- str2expression(statement)

        filter_try <- base::subset(df,
                                   eval(expr))
        return(TRUE)
    },

    error = function(cond){
        return(FALSE)
    })
    return(condition_worked)
}

#' Filter a data frame with series of statements
#'
#' @param df data frame / tibble to be filtered
#' @param statements character vector of individual conditional statements; need not evaluate successfully individually
#' @param apply_grouped logical, should filter be applied to each group?
#'
#' @return list, logical vector of success and failures, and
#'
checked_filter <- function(df, statements, apply_grouped){

    is.error <- function(x) inherits(x, "try-error")

    checks <- sapply(statements,
                     function(x)
                         check_individual_statement(df = df,
                                                    statement = x))


    if(any(checks)){





        grouped_checks <- checks
        grouped_checks[which(!apply_grouped)] <- FALSE
        ungrouped_checks <- checks
        ungrouped_checks[which(apply_grouped)] <- FALSE


        # generate strings for grouped and ungrouped checks

        if(any(grouped_checks)){


            cond_string_full <- paste(statements[grouped_checks],
                                      collapse = " & ")
            # filtered_df <- dplyr::filter(df,
            #                             eval(str2expression(cond_string_full)))


            filtered_df <- do.call(rbind,
                                   by(df,
                                      # as.factor(dplyr::group_indices(df)),
                                      as.factor(df$.dcrindex),
                                      function(x) base::subset(x,
                                                               eval(str2expression(cond_string_full)))
                                   )
            )

        }

        if(any(ungrouped_checks)) {

            if(all(!grouped_checks)){
                filtered_df <- df
            }

            cond_string_full <- paste(statements[ungrouped_checks],
                                      collapse = " & ")
#
#             filtered_df <- do.call(rbind,
#                     by(filtered_df,
#                        as.factor(dplyr::groups(filtered_df)),
#                        function(x) base::subset(filtered_df,
#                                                 eval(str2expression(cond_string_full)))
#                        )
#                     )

            filtered_df <- dplyr::filter(dplyr::ungroup(filtered_df),
                                        eval(str2expression(cond_string_full)))


        }






        return(list(succeeded = checks,
                    filtered_df = filtered_df,
                    statement_strings = statements[checks],
                    statement_strings_grouped = statements[grouped_checks],
                    statement_strings_ungrouped = statements[ungrouped_checks]
                    ))
    } else {

        return(list(succeeded = checks))
    }
}

#' Identify filter inputs by id and label
#'
#' @param allinputs list of all currently active inputs
#'
#' @return list with id of last filter (character), and number of last filter (numeric)
#'
# check_active_filters <- function(allinputs){
#
#
#
#     # get filters from all inputs
#     all_filters_lgl <- grepl("filter[0-9]+-strfilter", allinputs)
#     all_filters <- allinputs[all_filters_lgl]
#
#
#     # extract filter numbers
#     filter_numbers <- gsub(pattern = "[^0-9]",


split_groups <- function(dframe){

    outlist <- base::split(
        dframe,
        f = as.factor(dframe$.dcrindex)
    )

    return(outlist)

}


#' Apply filter based on a statement, scoped to groups
#'
#' @param dframe data.frame/tbl, grouped or ungrouped
#' @param statement character, statement for filtering (valid AND invalid)
#' @param scope_at numeric, group indices to apply filter statements to
#'
#' @return List, containing logical item stating \code{success}, and if \code{TRUE}
#' additional items \coce{filtered_df} and the respective \code{expression_string} (character) for generating
#' the data.frame \ tibble
#' @export
filter_scoped <- function(dframe, statement, scope_at){


    scope_at <- unlist(scope_at)


    n_groups <- dplyr::n_groups(dframe)
    var_groups <- rlang::syms(dplyr::group_vars(dframe))

    statement_check <- check_individual_statement(df = dframe,
                                                  statement = statement)



    if(isFALSE(statement_check)){


        return(list(succeeded = statement_check
                    # ,
                    # filtered_df = NULL,
                    # statement_strings = NULL,
                    # statement_strings_grouped = NULL,
                    # statement_strings_ungrouped = NULL
        ))


    } else if(isTRUE(statement_check)){


        if(rlang::is_missing(scope_at)){
            scope_at <- NULL
        }

        if(is.null(scope_at) | n_groups == 1){


            filt_expr <- paste0('dplyr::group_by(dplyr::filter(dplyr::ungroup(dframe),',
                                statement,
                                '), !!! var_groups)')
            filtered_df <- eval(parse(text = filt_expr))

        } else if(length(scope_at) == n_groups){
            filt_expr <- paste0('dplyr::filter(dframe,',
                                statement,
                                ')')
            filtered_df <- eval(parse(text = filt_expr))
        } else if(n_groups > 1 &
                  length(scope_at) >= 1 &
                  length(scope_at) != n_groups){

            filt_expr <- glue::glue(
                "dplyr::bind_rows(purrr::map_at(
                    .x = split_groups(dframe),
                    .at = {{deparse(scope_at)}},
                    .f = function(x){
                        dplyr::filter(
                            x,
                            {{statement}})
                    }
                ))",
                .open = "{{",
                .close = "}}"
            )
            filtered_df <- eval(str2expression(filt_expr))
        } else {
        }
        return(list(succeeded = statement_check,
                    filtered_df = filtered_df,
                    expression_string = filt_expr))
    }

}



filter_scoped_iterate <- function(dframe, condition_df){

    checks <- sapply(condition_df[ , 1, drop = TRUE],
                     function(x)
                         check_individual_statement(df = dframe,
                                                    statement = x))


    if(any(checks)){

        keep_checks <- which(checks)
        for(j in seq_along(keep_checks)){

            keep_idx <- keep_checks[j]
            if(j == 1){

                tmp <- filter_scoped(dframe = dframe,
                                     statement = condition_df[keep_idx, 1, drop = TRUE],
                                     scope_at = condition_df[keep_idx, 2, drop = TRUE])$filtered_df

            } else {

                tmp <- filter_scoped(dframe = tmp,
                                     statement = condition_df[keep_idx, 1, drop = TRUE],
                                     scope_at = condition_df[keep_idx, 2, drop = TRUE])$filtered_df
            }

        }
        return(tmp)

    }
    return(NULL)


}



#' Tag to display code
#'
#' @param ... Character strings
#'
#' @noRd
rCodeContainer <- function(...) {
    code <- htmltools::HTML(as.character(htmltools::tags$code(class = "language-r", ...)))
    htmltools::tags$div(htmltools::tags$pre(code))
}
#                            replacement = "",
#                            x = all_filters)
#
#     # get latest filter
#     max_filter <- which.max(filter_numbers)
#     last_filter <- all_filters[max_filter]
#
#
#     return(list(last_filter = last_filter,
#                 filter_number = as.numeric(filter_numbers[max_filter])))
#
# }

# #' check if filtering statement is successfull
# #'
# #' @param df dataframe to be filtered
# #' @param statements character, vector with individual statements for filtering
# #'
# #' @return logical vector of fails and successes
# #'
# check_filter <- function(df, statements){
#
#
#     is.error <- function(x) inherits(x, "try-error")
#
#
#     # checks on validity of filter expressions
#
#     checks_expression <- sapply(x, function(x)
#         try({str2expression}))
#     succeeded_expression <- !vapply(checks, is.error, logical(1))
#
#     if(any(!succeeded_expression)){
#         return(succeeded_expression)
#     }
#
#
#
#
#     # checks on filtering (see if variable is available)
#     checks <- lapply(statements, function(x)
#
#         try({dplyr::filter(df,
#                            eval(str2expression(x)))}))
#
#
#     succeeded <- !vapply(checks, is.error, logical(1))
#
#     cond_string_full <- paste(statements[succeeded], collapse = " & ")
#     #
#     filtered_df <- dplyr::filter(df, eval(str2expression(cond_string_full)))
#
#     return(list(condition_success = succeeded,
#                 filtered_df = filtered_df))
#
#
#
#
# }



#' extend brewer palette
#'
#' @param n numeric, number of colors
#'
#' @return color vector of length n
extend_palette <- function(n){

    if(n < 3){
        cols <- RColorBrewer::brewer.pal(3, "Accent")[1:n]}
    else if(n >= 3 & n <= 8){
        cols <- RColorBrewer::brewer.pal(n, "Accent")
    } else {
        cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Accent"))(n)

    }

    return(cols)

}


#' handle plotly selections
#'
#' @param old numeric or NULL
#' @param new numeric or NULL
#'
#' @return numeric or NULL
#'
handle_selection <- function(old, new){



    if(length(new)==0 & length(old) == 0){
        sel <- NULL
        print("no selected points")

    } else if (length(new)==0 & length(old) > 0){

        sel <- old
        print("selection was empty")

    } else if (any(new %in% old)) {
        sel <- setdiff(old, new)
        print("removing duplicates via selection")

    } else {
        sel <- c(new, old)
        print("adding new points via selection")
    }

    print("data after selection is")
    print(sel)

    return(sel)


}




#' Handle selections/clicks of outliers
#'
#' @param sel_data_old \code{data.frame}, which must have columns \code{keys}, \code{selection_count} and \code{.annotation}
#' @param sel_data_new \code{data.frame}, from \code{plotly::event_data}, with column \code{customData} as \code{.dcrkey}
#'
#' @return \code{data.frame}, to update the input of \code{sel_data_old}.
#'
handle_outlier_selection <- function(sel_data_old, sel_data_new){



    if(length(sel_data_new)>0){

        if(nrow(sel_data_old) > 0 & nrow(sel_data_new) > 0){
            new <- data.frame(keys = as.integer(sel_data_new$customdata),
                              selection_count = max(sel_data_old$selection_count) + 1,
                              .annotation = "",
                              stringsAsFactors = FALSE)

            if(any(new$keys %in% sel_data_old$keys)){
                new <- new[!{new$keys %in% sel_data_old$keys}, ]
            }

            sel_data_out <- rbind(sel_data_old, new)

        } else {
            new <- data.frame(keys = as.integer(sel_data_new$customdata),
                              selection_count = 1,
                              .annotation = "",
                              stringsAsFactors = FALSE)
            sel_data_out <- new
        }

    }

    return(sel_data_out)
}








#' Handle selection of outliers (with select - unselect capacity)
#'
#' @param sel_old_df data.frame of selection info
#' @param sel_new data.frame, event data from plotly, must have column \code{customdata}
#'
#' @return updated selection data frame
handle_sel_outliers <- function(sel_old_df, sel_new){







    if (NROW(sel_new) > 0){


        new_df <- data.frame(keys = as.integer(sel_new$customdata),
                             selection_count = ifelse(nrow(sel_old_df) > 0,
                                                      max(sel_old_df$selection_count) + 1,
                                                      1),
                             .annotation = "",
                             stringsAsFactors = FALSE)



        # check which points may be duplicates in selection
        if(nrow(sel_old_df) > 0 &&
           NROW(intersect(sel_old_df$keys, new_df$keys)) > 0){


            sel_out <- rbind(sel_old_df, new_df)

            dup_idcs_lgl <- duplicated(sel_out[ ,"keys"]) |
                duplicated(sel_out[ ,"keys"], fromLast = TRUE)


            sel_out <- sel_out[!dup_idcs_lgl, ]


            if(length(unique(sel_out$selection_count)) == 1){

                print("single selection left")
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


    # reset selection count if necessary (i.e. if only one count remains)


    print("COMPLETED!!!")


    return(sel_out)
}



#' Provide traces ids to set to invisible
#'
#' @param max_groups numeric, number of groups in grouptable
#' @param selected_groups groups highlighted in grouptable
#'
#' @details Provides the indices (JS notation, starting at 0) for indices
#' that are set to \code{visible = 'legendonly'} through \code{plotly.restyle}
#'
#' @return
#' @export
#'
#' @examples
hide_trace_idx <- function(max_groups, selected_groups){

    all_row_ids <- seq_len(max_groups)

    if(is.null(selected_groups)){
        deselect_ids <- NULL
    } else if(length(selected_groups) < max_groups){
        deselect_ids <- all_row_ids[all_row_ids %nin% selected_groups] - 1
    } else if(length(selected_groups) == max_groups){
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
#' @param max_id_group_trace numeric, max id of plotly trace from original data (not outlier traces)
#' @param input_sel_rows numeric, input from DT grouptable
#' @param flush character, \code{plotlyProxy} settings
#'
#' @return Used for it's side effect - no return
handle_restyle_traces <- function(source_id,
                                  session,
                                  dframe,
                                  scaling = 0.075,
                                  xvar,
                                  yvar,
                                  max_id_group_trace,
                                  input_sel_rows,
                                  flush = TRUE){

    if(any(c(xvar, yvar) %nin% names(dframe))){
        stop("xvar or yvar not in dframe, please try again")
    }


    pproxy <- plotly::plotlyProxy(source_id, session,
                                  deferUntilFlush = flush)


    # hide/show traces

    deselect_trace_id <- hide_trace_idx(
        max_groups = max_id_group_trace + 1,
        selected_groups = input_sel_rows)


    if(!is.null(deselect_trace_id)){


        plotly::plotlyProxyInvoke(pproxy,
                                  "restyle",
                                  list(visible = TRUE))

        plotly::plotlyProxyInvoke(pproxy,
                                  "restyle",
                                  list(visible = 'legendonly'),
                                  # deselect_trace_id)
                                  as.list(deselect_trace_id))



    } else {
        plotly::plotlyProxyInvoke(pproxy,
                                  "restyle",
                                  list(visible = TRUE))
    }


    # reset limits

    lims <- calc_limits_per_groups(dframe,
                                   group_index = input_sel_rows,
                                   xvar = xvar,
                                   yvar = yvar,
                                   scaling = scaling)






    plotly::plotlyProxyInvoke(pproxy,
                              "relayout",
                              list(yaxis = list(range = lims$ylim),
                                   xaxis = list(range = lims$xlim)))


}

#' Color conversion for plotly with alpha
#'
#' @param colorname string, hex value or R color name
#' @param alpha numeric, value for alpha (transparency)
#'
#' @return string, length 1, with rgb code for plotly
#'
col2plotlyrgba <- function(colorname, alpha){



    rgbstring <- paste0("rgba(", paste(grDevices::col2rgb(colorname), collapse = ","), ", ", alpha, ")")

    return(rgbstring)
}




#' Color conversion for plotly with alpha
#'
#' @param colorname string, hex value or R color name
#'
#' @return string, length 1, with rgb code for plotly
#'
col2plotlyrgb <- function(colorname){



    rgbstring <- paste0("rgb(", paste(grDevices::col2rgb(colorname), collapse = ","), ")")

    return(rgbstring)
}




#' Handle Add traces
#'
#' @param sp selected points
#' @param dframe plot data
#' @param ok reactive, old keys
#' @param selectors reactive input selectors
#' @param max_trace numeric, previous max trace id
#' @param source plotly source
#' @param session active session
#'
handle_add_traces <- function(sp, dframe, ok, selectors, max_trace, source = "scatterselect", session){


    is_spatial_plot <- identical(c(as.character(selectors$xvar),
                                   as.character(selectors$yvar)),
                                 c("lon", "lat"))


    if(is_spatial_plot){

        # geo_def <- list(
        #   # scope = 'usa',
        #   # projection = list(type = 'albers usa'),
        #   projection = list(type = 'mercator'),
        #   showland = TRUE,
        #   landcolor = plotly::toRGB("gray95"),
        #   subunitcolor = plotly::toRGB("gray85"),
        #   countrycolor = plotly::toRGB("gray85"),
        #   countrywidth = 0.5,
        #   subunitwidth = 0.5,
        #   showocean=TRUE,
        #   oceancolor="steelblue1",
        #   showlakes=TRUE,
        #   lakecolor="darkblue",
        #   showrivers=TRUE,
        #   rivercolor="darkblue"
        # )
        geo_def <-  list(style = "light")
    } else {
        geo_def <- list()
    }




    if(length(sp$df$keys) > 0){



        # check if selection is new
        if(!identical(ok(),
                      sp$df$keys)){

            max_sel_count <- max(sp$df$selection_count)

            last_sel_keys <- as.integer(sp$df$keys[sp$df$selection_count == max_sel_count])
            # grab points
            # add_points <- dframe()[dframe()$.dcrkey %in% last_sel_keys, ]
            add_points <- dframe()[last_sel_keys, ]
            # handle plotly - only adds trace for array > 2L
            if(nrow(add_points) == 1){
                add_points <- rbind(add_points, add_points)
            }

            print("---- adding traces -----")



            zvar_toggle <- nchar(selectors$zvar)>0
            if(zvar_toggle){
                z <- add_points[ , as.character(selectors$zvar), drop = TRUE]
            } else {
                z <- NULL
                # print("no zvar")
            }



            if(is_spatial_plot){

                plotly::plotlyProxy(source, session) %>%
                    plotly::plotlyProxyInvoke(
                        "addTraces",
                        list(
                            # lon = list(add_points[ , as.character(selectors$xvar), drop = TRUE]),
                            # lat = list(add_points[ , as.character(selectors$yvar), drop = TRUE]),
                            lon = add_points[ , as.character(selectors$xvar), drop = TRUE],
                            lat = add_points[ , as.character(selectors$yvar), drop = TRUE],
                            customdata = add_points[ , ".dcrkey", drop = TRUE],
                            text = add_points[ , ".dcrkey", drop = TRUE],
                            legendgroup = "out",
                            size = z,
                            # sizes = c(20,45),
                            type = "scattermapbox",
                            mode = "markers",
                            name = "outlier",
                            opacity = 1,
                            marker = list(
                                color = "red",
                                opacity = 1),
                            unselected = list(marker = list(opacity = 1)),
                            showlegend = list(TRUE)
                        )
                    )

            } else {


                plotly::plotlyProxy(source, session) %>%
                    plotly::plotlyProxyInvoke(
                    "addTraces",
                    list(
                        x = add_points[ , as.character(selectors$xvar), drop = TRUE],
                        y = add_points[ , as.character(selectors$yvar), drop = TRUE],
                        size = z,
                        # type = "scattergl",
                        # type = ifelse(is_spatial_plot,"scattermapbox", "scattergl"),
                        type = "scattergl",
                        mode = "markers",
                        # mode = ifelse(is_spatial_plot,"scattermapbox", "markers"),
                        name = "outlier",
                        customdata = add_points[ , ".dcrkey", drop = TRUE],
                        text = add_points[ , ".dcrkey", drop = TRUE],
                        legendgroup = "out",
                        marker = list(
                            color = "red",
                            opacity = 1),
                        unselected = list(marker = list(opacity = 1)),
                        selected = list(marker = list(opacity = 1)),
                        showlegend = TRUE
                    ),
                max_trace+1
                )
            }
            # update the old keys
            ok(sp$df$keys)


        } else {

            cat("WE DOWN\n")

        }



    }

    return(ok)


}
#' Handle Add traces
#'
#' @param sp selected points
#' @param dframe plot data
#' @param ok reactive, old keys
#' @param selectors reactive input selectors
#' @param trace_map numeric, previous max trace id
#' @param source plotly source
#' @param session active session
#'
handle_add_outlier_trace <- function(sp,
                                     dframe,
                                     ok,
                                     selectors,
                                     trace_map,
                                     source = "scatterselect",
                                     session){


    outidx <- as.numeric(trace_map[which(trace_map[ ,1] == "outlier"), 2])

    # handle case when removing all points from current trace (i.e. going back to normal)
    if(NROW(outidx) > 0 & nrow(sp$df)==0){
        plotly::plotlyProxy(source, session) %>%
            plotly::plotlyProxyInvoke(
                "deleteTraces",
                outidx)
    }


    # handle spatial config
    is_spatial_plot <- identical(c(as.character(selectors$xvar),
                                   as.character(selectors$yvar)),
                                 c("lon", "lat"))

    if(is_spatial_plot){
        geo_def <-  list(style = "light")
    } else {
        geo_def <- list()
    }





    # handle case when points exist in selection
    if(length(sp$df$keys) > 0){

            add_points <- dframe()[sp$df$keys, ]
            # handle plotly - only adds trace for array > 2L
            if(nrow(add_points) == 1){
                add_points <- rbind(add_points, add_points)
            }

            print("---- adding traces -----")

            zvar_toggle <- nchar(selectors$zvar)>0
            if(zvar_toggle){
                z <- add_points[ , as.character(selectors$zvar), drop = TRUE]
            } else {
                z <- NULL
            }

            if(is_spatial_plot){


                if(length(outidx) == 0){

                    plotly::plotlyProxy(source, session) %>%
                        plotly::plotlyProxyInvoke(
                            "addTraces",
                            list(
                                lon = add_points[ , as.character(selectors$xvar), drop = TRUE],
                                lat = add_points[ , as.character(selectors$yvar), drop = TRUE],
                                customdata = add_points[ , ".dcrkey", drop = TRUE],
                                text = add_points[ , ".dcrkey", drop = TRUE],
                                # legendgroup = "out",
                                size = z,
                                # sizes = c(20,45),
                                type = "scattermapbox",
                                mode = "markers",
                                name = "outlier",
                                opacity = 1,
                                marker = list(
                                    color = "red",
                                    opacity = 1),
                                unselected = list(marker = list(opacity = 1)),
                                showlegend = list(TRUE)))
                    } else if(length(outidx) > 0){

                            plotly::plotlyProxy(source, session) %>%
                                plotly::plotlyProxyInvoke(
                                    "deleteTraces",
                                    outidx
                                )

                            plotly::plotlyProxy(source, session) %>%
                                plotly::plotlyProxyInvoke(
                                    "addTraces",
                                    list(
                                        lon = add_points[ , as.character(selectors$xvar), drop = TRUE],
                                        lat = add_points[ , as.character(selectors$yvar), drop = TRUE],
                                        customdata = add_points[ , ".dcrkey", drop = TRUE],
                                        text = add_points[ , ".dcrkey", drop = TRUE],
                                        # legendgroup = "out",
                                        size = z,
                                        # sizes = c(20,45),
                                        type = "scattermapbox",
                                        mode = "markers",
                                        name = "outlier",
                                        opacity = 1,
                                        marker = list(
                                            color = "red",
                                            opacity = 1),
                                        unselected = list(marker = list(opacity = 1)),
                                        showlegend = list(TRUE)
                                    ))



                        }

            } else {


                if(length(outidx) == 0){

                    print("NO OUTLIER TRACE YET")

                    plotly::plotlyProxy(source, session) %>%
                        plotly::plotlyProxyInvoke(
                            "addTraces",
                            list(
                                x = add_points[ , as.character(selectors$xvar), drop = TRUE],
                                y = add_points[ , as.character(selectors$yvar), drop = TRUE],
                                size = z,
                                # type = "scattergl",
                                # type = ifelse(is_spatial_plot,"scattermapbox", "scattergl"),
                                type = "scattergl",
                                mode = "markers",
                                # mode = ifelse(is_spatial_plot,"scattermapbox", "markers"),
                                name = "outlier",
                                customdata = add_points[ , ".dcrkey", drop = TRUE],
                                text = add_points[ , ".dcrkey", drop = TRUE],
                                # legendgroup = "out",
                                marker = list(
                                    color = "red",
                                    opacity = 1),
                                unselected = list(marker = list(opacity = 1)),
                                selected = list(marker = list(opacity = 1)),
                                showlegend = TRUE
                            ),
                            max(as.numeric(trace_map[ ,2]))+1


                        )} else if(length(outidx) > 0){


                            print("OUTLIER TRACE EXISTING")


                            plotly::plotlyProxy(source, session) %>%
                                plotly::plotlyProxyInvoke(
                                    "deleteTraces",
                                    outidx
                                )


                            plotly::plotlyProxy(source, session) %>%
                                plotly::plotlyProxyInvoke(
                                    "addTraces",
                                    list(
                                        x = add_points[ , as.character(selectors$xvar), drop = TRUE],
                                        y = add_points[ , as.character(selectors$yvar), drop = TRUE],
                                        size = z,
                                        # type = "scattergl",
                                        # type = ifelse(is_spatial_plot,"scattermapbox", "scattergl"),
                                        type = "scattergl",
                                        mode = "markers",
                                        # mode = ifelse(is_spatial_plot,"scattermapbox", "markers"),
                                        name = "outlier",
                                        customdata = add_points[ , ".dcrkey", drop = TRUE],
                                        text = add_points[ , ".dcrkey", drop = TRUE],
                                        # legendgroup = "out",
                                        marker = list(
                                            color = "red",
                                            opacity = 1),
                                        unselected = list(marker = list(opacity = 1)),
                                        selected = list(marker = list(opacity = 1)),
                                        showlegend = TRUE
                                    ),
                                    outidx)
                        }
            }
            # update the old keys
            ok(sp$df$keys)

    }

    return(ok)
}

# helpers ------------


`%nin%` <-  Negate(`%in%`)

# drop null or empty values from a list
drop_empty <- function(l){
    l[vapply(l, shiny::isTruthy, logical(1))]
}



#' Make grouping overview table
#'
#' @param dframe
#' @importFrom rlang .data
#'
#' @return tibble with one row per group
make_group_table <- function(dframe){
    group_table <- dplyr::summarise(dframe,
                                    `Group` = dplyr::cur_group_id(),
                                    `n obs.` = dplyr::n())
        group_table <- dplyr::relocate(group_table, .data$Group)


}


#' Return x and y limits of "group-subsetted" dframe
#'
#' @param dframe dataframe/tibble, grouped/ungrouped
#' @param group_index numeric, group indices for which to return lims
#' @param xvar character, name of x var for plot (must exist in dframe)
#' @param yvar character, name of y var for plot (must exist in dframe)
#' @param scaling numeric, 1 +/- \code{scaling} times limits
#'
#' @return list with xlim and ylim
calc_limits_per_groups <- function(dframe, group_index, xvar, yvar, scaling = 0.02){


    if(is.null(group_index)){

        group_index <- seq_len(dplyr::n_groups(dframe))
    }

    if(!rlang::inherits_any(dframe, c("data.frame", "tibble"))){
        stop("need data.frame or tibble to proceed")
    }

    if(dplyr::n_groups(dframe) == 1 &
       length(group_index) == 1){

        if(group_index > 1){
        warning("supplied group index outside of group stack, setting to 1")
        group_index <- 1
        }

    }

    if(any(c(xvar, yvar) %nin% names(dframe))){
        stop("xvar or yvar not in dframe, please try again")
    }

    group_rows <- unlist(
        dplyr::group_data(
            dframe)$.rows[group_index])




    if(any(rlang::inherits_any(dframe[, xvar, drop = TRUE], "POSIXct"),
           rlang::inherits_any(dframe[, xvar, drop = TRUE], "POSIXct"))){

        posix_scale <- function(timestamp){

            if(scaling == 0){
                scaling <- 0.01
            }

            total_range <-  as.numeric(range(timestamp, na.rm = TRUE))
            # offset <- (diff(total_range)) * c(-1,1) + scaling
            offset <- (diff(total_range) * scaling) * c(-1,1)


            lim <- range(timestamp, na.rm = TRUE) +
                offset
            return(lim)

        }

        if(rlang::inherits_any(dframe[, xvar, drop = TRUE], "POSIXct")){
            xlim <- posix_scale(dframe[group_rows, xvar, drop = TRUE])
        }
        if(rlang::inherits_any(dframe[, yvar, drop = TRUE], "POSIXct")){
            ylim <- posix_scale(dframe[group_rows, yvar, drop = TRUE])
        }



    }

    if(!exists("xlim")){
    xlim <- range(dframe[group_rows, xvar, drop = TRUE],
                  na.rm = TRUE) *
        c(1 - scaling, 1 + scaling)
    }


    if(!exists("ylim")){


    ylim <- range(dframe[group_rows, yvar, drop = TRUE],
                  na.rm = TRUE) *
        c(1 - scaling, 1 + scaling)
    }

    return(list(xlim = xlim, ylim = ylim))

}


