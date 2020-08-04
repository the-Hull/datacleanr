#------------------------------------------------------------------------------#
# MODULE UI ----
#' UI Module: Extraction Text output
#'
#' @param id Character string
#'
module_ui_extract_code <- function(id) {
    ns <- shiny::NS(id)


    shiny::tagList(

        shiny::fluidRow(
            align = "left",
            column(width = 4,
                   shiny::selectInput(
                inputId = ns("paradigm"),
                label = "Choose coding style",
                choices = c("base", "data.table", "dplyr"),
                selected = "base",
                selectize = TRUE,
                multiple = FALSE
            )),

            shiny::column(width = 4,
                          style = "margin-top: 25px;",
                   shiny::actionButton(
                inputId = ns("codebtn"),
                label = "Send to RStudio",
                class = "btn-info",
                icon = shiny::icon("check-double")
            )),

            shiny::column(width = 4,
                          style = "margin-top: 25px;",
                   shiny::actionButton(
                inputId = ns("copybtn"),
                label = "Copy to clipboard",
                class = "btn-info",
                icon = shiny::icon("check-double")
            ))
        ),

        shiny::verbatimTextOutput(ns("codeprint"))
        # shiny::uiOutput(ns("codeprint")))
    )

}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

#' Server Module:  Selection Annotator
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param df_label string, name of original df input
#' @param filter_strings reactive value, individual strings from filtering
#' @param filter_strings reactive values, data frame with selected point keys, annotations, and selection count
#'
#'
module_server_extract_code  <-
    function(input,
             output,
             session,
             df_label,
             filter_strings,
             sel_points) {
        ns = session$ns


        setup_string <-
            glue::glue('{df_label}$.dcrkey <- seq_len(nrow({df_label}))')





        if (!is.null(filter_strings()) |
            nrow(sel_points$df) > 0) {
            text_out <- setup_string




        } else {
            text_out <- "## data has not been cleaned yet"
        }

        # text_out(setup_string)



        # Filtering ---------------------------------------------------------------




        subset_strings <- shiny::reactiveValues(base = NULL,
                                                dplyr = NULL,
                                                dt = NULL)


        if (!is.null(filter_strings())) {
            df_label_filtered <- paste0(df_label, "_filtered")

            subset_strings$base <- glue::glue('
                    subset({df_label},
                       {paste0(filter_strings(), collapse = ", ")})
                                  ')


            subset_strings$dplyr <- glue::glue('
                    dplyr::filter({df_label},
                       {paste0(filter_strings(), collapse = ", ")})
                                  ')

            subset_strings$dt <- glue::glue('
                    subset({df_label},
                       {paste0(filter_strings(), collapse = ", ")})
                                  ')


            use_code_subset <- base::switch(
                input$paradigm,
                base = subset_strings$base,
                dplyr = subset_strings$dplyr,
                data.table = subset_strings$dt
            )

            text_out <- glue::glue('{text_out}

                                {df_label_filtered} <- {use_code_subset}')


        }


        # Outlier selection -------------------------------------------------------



        sel_points_strings <-
            shiny::reactiveValues(
                code_make_outlier_var = NULL,
                code_join_outlier_dplyr = NULL,
                code_join_outlier_dt = NULL
            )

        if (nrow(sel_points$df) > 0) {
            if (!is.null(filter_strings())) {
                df_label <- df_label_filtered

            }

            sepo <- sel_points$df %>%
                dplyr::rename(.dcrkey = keys)


            sepo$.annotation[which(nchar(sepo$.annotation) == 0)] <- NA


            sel_points_str <-
                paste(capture.output(dput(sepo[, colnames(sepo) %nin% "selection_count"])),
                      collapse = " ")


            sel_points_strings$code_make_outlier_var <-
                glue::glue('{df_label}_outlier_selection <- {sel_points_str}')

            sel_points_strings$code_join_outlier_dplyr <-
                glue::glue(
                    '{df_label}_outlier  <- dplyr::left_join({df_label}, {df_label}_outlier_selection, by = ".dcrkey")'
                )

            sel_points_strings$code_join_outlier_dt <-
                glue::glue(
                    '{df_label}_outlier  <- merge({df_label}, {df_label}_outlier_selection, by = ".dcrkey", all = TRUE)'
                )


            use_code_outlier <-  base::switch(
                input$paradigm,
                base = sel_points_strings$code_join_outlier_dt,
                dplyr = sel_points_strings$code_join_outlier_dplyr,
                data.table = sel_points_strings$code_join_outlier_dt
            )


            text_out <- glue::glue(
                '{text_out}

                                {sel_points_strings$code_make_outlier_var}

                                {use_code_outlier}
                                '
            )
        }




        # Rendering ---------------------------------------------------------------


        # text_out <- paste(capture.output(styler::style_text(as.character(text_out),

        text_out <- paste(formatR::tidy_source(text = text_out)$text.tidy, collapse = "\n")


        output$codeprint <- shiny::renderText(text_out)
        # output$codeprint <- renderUI({
        #     htmltools::tagList(
        #     rCodeContainer(id = ns("codefilter"), text_out))
        # })







    }
