

# alternative -------------------------------------------------------------


#------------------------------------------------------------------------------#
# MODULE UI ----
#' UI Module: box for str filter condition
#'
#' @param id Character string
#'
module_ui_group_selector_table <- function(id) {
    ns <- shiny::NS(id)


    shiny::tagList(
        DT::DTOutput(ns('grouptable'))
    )

}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

#' Server Module: box for str filter condition
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param df data frame (either from overview or filtering tab)
#' @param df_label character, original input data frame
#' @param ... arguments passed to \code{datatable()}
#' @importFrom rlang .data
#'
#' @details provides UI text box element
module_server_group_selector_table <- function(input, output, session, df, df_label, ...){
    ns = session$ns

    # print(df)
    # print(paste("this:", df$df$data))

    # if(!is.null(df$df$data)){




        # group_table <- dplyr::group_data(df()) %>%
        #     dplyr::mutate(n_obs = sapply(.rows, length),
        #            .rows = NULL)


    group_table <- dplyr::summarise(df(),
                                    `Group` = as.character(unique(.data$.dcrindex)),
                                    `n obs.` = dplyr::n()) %>%
        dplyr::relocate(.data$Group)

        if(identical(dim(dplyr::group_data(df())), as.integer(c(1,1)))){

            group_table <- data.frame(`Group` = df_label,
                                      `n obs.` = nrow(df()),
                                      stringsAsFactors = FALSE)
            }

            output$grouptable <- DT::renderDT(group_table,
                                              rownames = FALSE,
                                              ...
                                              # selection = 'multiple')
                                              # selection = 'single'
                                              )



        # output$grouptable <- DT::renderDT(df,
        #                                   options = list(selection = 'single'))






        # output$selected_row <- shiny::renderText({
        #
        #     dt_selected_row <- input$grouptable_rows_selected
        #
        #     # if(req(dt_selected_row)){
        #     paste0("Selected Row is: ", dt_selected_row)
        #     # } else {
        #     #
        #     # NULL
        #     # }
        # })

        # return(reactive(input$grouptable_rows_selected))

    # }
}
