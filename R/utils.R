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





#' Identify filter inputs by id and label
#'
#' @param allinputs list of all currently active inputs
#'
#' @return list with id of last filter (character), and number of last filter (numeric)
#'
check_active_filters <- function(allinputs){



    # get filters from all inputs
    all_filters_lgl <- grepl("filter[0-9]+-strfilter", allinputs)
    all_filters <- allinputs[all_filters_lgl]


    # extract filter numbers
    filter_numbers <- gsub(pattern = "[^0-9]",
                           replacement = "",
                           x = all_filters)

    # get latest filter
    max_filter <- which.max(filter_numbers)
    last_filter <- all_filters[max_filter]


    return(list(last_filter = last_filter,
                filter_number = as.numeric(filter_numbers[max_filter])))

}

