
# UI ----------------------------------------------------------------------



#' UI Module: group selection
#'
#' @param df data.frame loaded into gadget; should support df, tibble, data.table
#' @param id Character, identifier for variable selection
#'
#'
module_ui_group_select <- function(df, id){
    ns <- NS(id)

    vars <- colnames(df)[get_factor_cols_idx(df)]

    tagList(selectInput(inputId = ns("select"),
                        label = "Grouping Variables",
                        choices = vars,
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE)
    )


}


module_ui_summarytool

# Server ------------------------------------------------------------------


