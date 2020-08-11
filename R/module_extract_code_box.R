#------------------------------------------------------------------------------#
# MODULE UI ----
#' UI Module: Extraction Text output
#'
#' @param id Character string
#'
module_ui_extract_code <- function(id) {
    ns <- shiny::NS(id)


    shiny::tagList(shiny::fluidRow(
        shiny::column(
            # align = "left",
            width = 6,
            shiny::selectInput(
                inputId = ns("paradigm"),
                label = "Choose coding style",
                choices = c("base", "data.table", "dplyr"),
                selected = "base",
                selectize = TRUE,
                multiple = FALSE
            )
        ),

        shiny::column(
            # align = "left",
            width = 3,
            style = "margin-top: 25px;",
            shiny::actionButton(
                inputId = ns("codebtn"),
                label = "Send to RStudio",
                class = "btn-info",
                icon = shiny::icon("share-square")
            )
        ),
        shiny::column(
            width = 3,
            style = "margin-top: 25px;",
            shiny::actionButton(
                inputId = ns("copybtn"),
                label = "Copy to clipboard",
                class = "btn-info",
                icon = shiny::icon("copy")
            )
        )),

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
#' @param sel_points reactive values, data frame with selected point keys, annotations, and selection count
#' @param overwrite reacive value, TRUE/FALSE from checkbox input
#' @importFrom rlang .data
#'
module_server_extract_code  <-
    function(input,
             output,
             session,
             df_label,
             filter_strings,
             sel_points,
             overwrite) {

        ns = session$ns

        setup_string <-  base::switch(
            input$paradigm,
            base =  glue::glue('{df_label}$.dcrkey <- seq_len(nrow({df_label}))'
            ),
            dplyr = glue::glue(
                '
                library(dplyr)\n
                {df_label} <- {df_label} %>% dplyr::mutate(.dcrkey = seq_len(nrow(.)))
                '
            ),
            data.table = glue::glue(
                'library(data.table)\n
                {df_label} <- data.table::as.data.table({df_label});
                {df_label}[ , .dcrkey := seq_len(nrow(.SD))]
                '
            )
        )



        if (!is.null(filter_strings()) |
            nrow(sel_points$df) > 0) {
            text_out <- setup_string


        } else {
            text_out <- "## data has not been cleaned yet"
        }


        # Filtering ---------------------------------------------------------------
        subset_strings <- shiny::reactiveValues(base = NULL,
                                                dplyr = NULL,
                                                dt = NULL)
        if (!is.null(filter_strings())) {
            if (!overwrite) {
                df_label_filtered <- paste0(df_label, "_filtered")
            } else {
                df_label_filtered <- df_label
            }

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
                as.character(input$paradigm),
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

            # extra string as comment

            info_comment_outlier_obs <-
                " observations from manual selection (Viz tab);"
            info_comment_outlier_merge <-
                " create data set with annotation column (non-outliers are NA);"
            info_comment_outlier_removal <-
                " comment out below to keep manually selected obs in data set;"




            if (!overwrite) {
                df_label_outlier <- paste0(df_label, "_outlier")
            } else {
                df_label_outlier <- df_label
            }

            sepo <- sel_points$df %>%
                dplyr::rename(.dcrkey = .data$keys)

            sel_points_str <-
                paste(utils::capture.output(dput(sepo[, colnames(sepo) %nin% "selection_count"])),
                      collapse = " ")


            sel_points_strings$code_make_outlier_var <-
                glue::glue(
                    '
                           # {info_comment_outlier_obs}
                           {df_label}_outlier_selection <- {sel_points_str}
                           '
                )

            sel_points_strings$code_join_outlier_dplyr <-
                glue::glue(
                    '
                    # {info_comment_outlier_merge}
                    {df_label_outlier}  <- dplyr::left_join({df_label}, {df_label}_outlier_selection, by = ".dcrkey");

                    # {info_comment_outlier_removal}
                    {df_label_outlier}  <- {df_label_outlier} %>% dplyr::filter(is.na(.annotation))

                    '
                )

            sel_points_strings$code_join_outlier_dt <-
                glue::glue(
                    '
                    # {info_comment_outlier_merge}
                    {df_label_outlier}  <- merge({df_label}, {df_label}_outlier_selection, by = ".dcrkey", all = TRUE)

                    # {info_comment_outlier_removal}
                    {df_label_outlier}  <- {df_label_outlier}[is.na(.annotation), ]
                    '
                )


            sel_points_strings$code_join_outlier_base <-
                glue::glue(
                    '
                    # {info_comment_outlier_merge}
                    {df_label_outlier}  <- merge({df_label}, {df_label}_outlier_selection, by = ".dcrkey", all = TRUE)

                    # {info_comment_outlier_removal}
                    {df_label_outlier}  <- {df_label_outlier}[is.na({df_label_outlier}$.annotation), ]
                    '
                )

            use_code_outlier <-  base::switch(
                input$paradigm,
                base = sel_points_strings$code_join_outlier_base,
                dplyr = sel_points_strings$code_join_outlier_dplyr,
                data.table = sel_points_strings$code_join_outlier_dt
            )


            text_out <- glue::glue(
                '
                {text_out}

                                # observations from manual selection in Viz. Tab
                                {sel_points_strings$code_make_outlier_var}

                                {use_code_outlier}

                                '
            )
        }


        # Rendering ---------------------------------------------------------------

        text_out <-
            paste(
                formatR::tidy_source(
                    text = text_out,
                    output = FALSE,
                    width = 100
                )$text.tidy,
                collapse = "\n"
            )


        output$codeprint <- shiny::renderText(text_out)

        return(text_out)


    }
