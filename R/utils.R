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
                                      as.factor(dplyr::group_indices(df)),
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
        f = as.factor(dplyr::group_indices(dframe))
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

        message("statement passed")

        if(rlang::is_missing(scope_at)){
            scope_at <- NULL
        }

        if(is.null(scope_at) | n_groups == 1){
            filt_expr <- paste0('dplyr::filter(dplyr::ungroup(dframe),',
                                statement,
                                ')')
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
            print("don't know how we got here")
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

    print(checks)

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

            sel_data_old <- rbind(sel_data_old, new)

        } else {
            new <- data.frame(keys = as.integer(sel_data_new$customdata),
                              selection_count = 1,
                              .annotation = "",
                              stringsAsFactors = FALSE)
            sel_data_old <- new
        }

    }

    return(sel_data_old)
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
#' @param source plotly source
#' @param session active session
#'
handle_add_traces <- function(sp, dframe, ok, selectors, source = "scatterselect", session){


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
            add_points <- dframe()[dframe()$.dcrkey %in% last_sel_keys, ]
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
                                marker = list(
                                    color = "red",
                                    line = list(color = "red",
                                                width = 2),
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
                        mode = "markers",
                        # mode = ifelse(is_spatial_plot,"scattermapbox", "markers"),
                        name = "outlier",
                        customdata = add_points[ , ".dcrkey", drop = TRUE],
                        text = add_points[ , ".dcrkey", drop = TRUE],
                        legendgroup = "out",
                        marker = list(
                            color = "darkgray",
                            line = list(color = "red",
                                        width = 2),
                            opacity = 1),
                        unselected = list(marker = list(opacity = 1)),
                        showlegend = TRUE
                    )
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


# helpers ------------


`%nin%` <-  Negate(`%in%`)

# drop null or empty values from a list
drop_empty <- function(l){
    l[vapply(l, shiny::isTruthy, logical(1))]
}
