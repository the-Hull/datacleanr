#' Identify columns carrying non-numeric values
#'
#' @param x data.frame
#'
#' @return logical, is column in x non-numeric?
#'
get_factor_cols_idx <- function(x){

    if(ncol(x) < 1){
        stop("Error: Elements of your input vector do not have the same length!")
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
#' @param inputs
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
#' @param statement character vector of individual conditional statements; need not evaluate successfully individually
#'
#' @return list, logical vector of success and failures, and
#'
checked_filter <- function(df, statements){

    is.error <- function(x) inherits(x, "try-error")

    checks <- sapply(statements,
                     function(x)
                         check_individual_statement(df = df,
                                                    statement = x))

    if(any(checks)){

        cond_string_full <- paste(statements[checks],
                                  collapse = " & ")
        filtered_df <- base::subset(df,
                                    eval(str2expression(cond_string_full)))

        return(list(succeeded = checks,
                    filtered_df = filtered_df))
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

#' check if filtering statement is successfull
#'
#' @param df dataframe to be filtered
#' @param statements character, vector with individual statements for filtering
#'
#' @return logical vector of fails and successes
#'
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
        cols <- RColorBrewer::brewer.pal(3, "Set2")[1:n]}
    else if(n >= 3 & n <= 8){
        cols <- RColorBrewer::brewer.pal(n, "Set2")
    } else {
        cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n)

    }

    return(cols)

}
