
# Helpers -----------------------------------------------------------------

text_out_interactive <- function(sepo, stats, fdf, df_lab, ovt, gv) {
    info_comment_outlier_index <-
        " adding column for unique IDs;"




    if(nrow(sepo) == 0){
        index_str <- ""
    } else {
        index_str <- glue::glue(
            '

                      # {info_comment_outlier_index}
                      {df_lab}$.dcrkey <- seq_len(nrow({df_lab}))

                      ')
    }

    setup_string <- glue::glue(
        '
                  library(dplyr)
                  library(datacleanr)
                  {index_str}
                  ')




    # Output from filtering tab
    if(any(stats)  &
       nrow(fdf) > 0){
        # FILTERING ---------------------------------------------------------------
        fdf <- fdf[stats, ]




        #### INFO COMMENTS
        info_comment_filt_df <-
            " stats and scoping level for filtering"
        info_comment_filters <-
            " applying (scoped) filtering by groups;"


        filt_df_dput <-
            paste(utils::capture.output(dput(fdf)),
                  collapse = " ")


        filt_df_string <- glue::glue('
                                      filter_conditions <- {filt_df_dput}
                                      ')


        if (!ovt) {
            df_lab_filtered <- paste0(df_lab, "_filtered")
        } else {
            df_lab_filtered <- df_lab
        }

        if(!is.null(gv())){
            group_string <- glue::glue('
                                 {df_lab} <- dplyr::group_by({df_lab}, {paste(gv(), collapse = ", ")})

                      ')
        } else {

            group_string <- ""
        }


        apply_filter_string <- glue::glue('
                  {group_string}
                  # {info_comment_filt_df}
                  {filt_df_string}

                  # {info_comment_filters}
                  {df_lab_filtered} <- datacleanr::filter_scoped_df(dframe = {df_lab},
                  condition_df = filter_conditions)
                                           ')

    } else {

        apply_filter_string <- ""
        df_lab_filtered <- df_lab
    }





    if(nrow(sepo)>0){
        # VIZ SELECT --------------------------------------------------------------

        if(!ovt){
            df_lab_viz <- paste0(df_lab, "_vizclean")
        } else {
            df_lab_viz <- df_lab
        }

        #### INFO COMMENTS
        info_comment_outlier_obs <-
            " observations from manual selection (Viz tab);"
        info_comment_outlier_merge <-
            " create data set with annotation column (non-outliers are NA);"
        info_comment_outlier_removal <-
            "remove comment below to drop manually selected obs in data set;"




        sepo <- sepo %>%
            dplyr::rename(.dcrkey = .data$keys)

        sepo_dput <-
            paste(utils::capture.output(dput(sepo[, colnames(sepo) %nin% "selection_count"])),
                  collapse = " ")

        sepo_str <-
            glue::glue(
                '
                          # {info_comment_outlier_obs}
                          {df_lab}_outlier_selection <- {sepo_dput}
                          '
            )




        if(!ovt){
            df_lab_viz_final <- paste0(df_lab_viz, "_out")
        } else {
            df_lab_viz_final <- df_lab
        }


        code_join_outlier_dplyr <-
            glue::glue(
                '
                       {sepo_str}

                      # {info_comment_outlier_merge}
                      {df_lab_viz}  <- dplyr::left_join({df_lab_filtered}, {df_lab}_outlier_selection, by = ".dcrkey");

                      # {info_comment_outlier_removal}
                      # {df_lab_viz_final}  <- {df_lab_viz} %>% dplyr::filter(is.na(.annotation))

                      '
            )

    } else {

        code_join_outlier_dplyr <- ""

    }


    text_out <- glue::glue(
        '
              # datacleaning with datacleanr ({utils::packageVersion("datacleanr")})
              # {utils::timestamp(quiet = TRUE)}

              {setup_string}

              {apply_filter_string}

              {code_join_outlier_dplyr}

              '
    )
    return(text_out)
}



text_out_file <- function(sepo, stats, fdf, df_lab, ovt, gv, out_path) {


    file_path <- df_lab
    df_lab <- fs::path_ext_remove(fs::path_file(file_path))


    info_comment_outlier_index <-
        " adding column for unique IDs;"




    if(nrow(sepo) == 0){
        index_str <- ""
    } else {
        index_str <- glue::glue(
            '
                      # {info_comment_outlier_index}
                      {df_lab}$.dcrkey <- seq_len(nrow({df_lab}))
                      ')
    }

    setup_string <- glue::glue(
        '
                  library(dplyr)
                  library(datacleanr)

                  {df_lab} <- readRDS("{file_path}")

                  {index_str}
                  ')




    # Output from filtering tab
    if(any(stats)  &
       nrow(fdf) > 0){
        # FILTERING ---------------------------------------------------------------
        fdf <- fdf[stats, ]




        #### INFO COMMENTS
        info_comment_filt_df <-
            " stats and scoping level for filtering"
        info_comment_filters <-
            " applying (scoped) filtering by groups;"


        # filt_df_dput <-
        #     paste(utils::capture.output(dput(fdf)),
        #           collapse = " ")


        filt_df_string <- glue::glue('
                                      filter_conditions <- readRDS("{out_path$file_out_raw}")$dcr_condition_df
                                      ')


        if (!ovt) {
            df_lab_filtered <- paste0(df_lab, "_filtered")
        } else {
            df_lab_filtered <- df_lab
        }

        if(!is.null(gv)){
            group_string <- glue::glue('
                                 {df_lab} <- dplyr::group_by({df_lab}, {paste(gv, collapse = ", ")})
                      ')
        } else {

            group_string <- ""
        }


        apply_filter_string <- glue::glue('
                  {group_string}
                  # {info_comment_filt_df}
                  {filt_df_string}

                  # {info_comment_filters}
                  {df_lab_filtered} <- datacleanr::filter_scoped_df(dframe = {df_lab},
                  condition_df = filter_conditions)
                                           ')

    } else {

        apply_filter_string <- ""
        df_lab_filtered <- df_lab
    }





    if(nrow(sepo)>0){
        # VIZ SELECT --------------------------------------------------------------

        if(!ovt){
            df_lab_viz <- paste0(df_lab, "_vizclean")
        } else {
            df_lab_viz <- df_lab
        }

        #### INFO COMMENTS
        info_comment_outlier_obs <-
            " observations from manual selection (Viz tab);"
        info_comment_outlier_merge <-
            " create data set with annotation column (non-outliers are NA);"
        info_comment_outlier_removal <-
            "remove comment below to drop manually selected obs in data set;"



        sepo <- sepo %>%
            dplyr::rename(.dcrkey = .data$keys)

        sepo_dput <-
            paste(utils::capture.output(dput(sepo[, colnames(sepo) %nin% "selection_count"])),
                  collapse = " ")

        sepo_str <-
            glue::glue(
                '
                          # {info_comment_outlier_obs}
                          {df_lab}_outlier_selection <- readRDS("{out_path$file_out_raw}")$dcr_selected_outliers
                          '
            )




        if(!ovt){
            df_lab_viz_final <- paste0(df_lab_viz, "_out")
        } else {
            df_lab_viz_final <- df_lab
        }


        code_join_outlier_dplyr <-
            glue::glue(
                '
                       {sepo_str}

                      # {info_comment_outlier_merge}
                      {df_lab_viz}  <- dplyr::left_join({df_lab_filtered}, {df_lab}_outlier_selection, by = ".dcrkey");

                      # {info_comment_outlier_removal}
                      # {df_lab_viz_final}  <- {df_lab_viz} %>% dplyr::filter(is.na(.annotation))

                      saveRDS({df_lab_viz_final}, "{out_path$file_out_cleaned}")

                      '
            )

    } else {

        code_join_outlier_dplyr <- ""

    }


    text_out <- glue::glue(
        '
              # datacleaning with datacleanr ({utils::packageVersion("datacleanr")})
              # {utils::timestamp(quiet = TRUE)}

              {setup_string}

              {apply_filter_string}

              {code_join_outlier_dplyr}

              '
    )
    return(text_out)
}



#------------------------------------------------------------------------------#
# MODULE UI ----
#' UI Module: Extraction Text output
#'
#' @param id Character string
#'
module_ui_extract_code <- function(id) {
    ns <- shiny::NS(id)


    shiny::uiOutput(ns("codeDiv"))

    # shiny::tagList(shiny::fluidRow(
    #     # shiny::column(
    #     #     # align = "left",
    #     #     width = 6,
    #     #     shiny::selectInput(
    #     #         inputId = ns("paradigm"),
    #     #         label = "Choose coding style",
    #     #         choices = c("base", "data.table", "dplyr"),
    #     #         selected = "base",
    #     #         selectize = TRUE,
    #     #         multiple = FALSE
    #     #     )
    #     # ),
    #
    #     shiny::column(
    #         # align = "left",
    #         width = 3,
    #         style = "margin-top: 25px;",
    #         shiny::actionButton(
    #             inputId = ns("codebtn"),
    #             label = "Send to RStudio",
    #             class = "btn-info",
    #             icon = shiny::icon("share-square")
    #         )
    #     ),
    #     shiny::column(
    #         width = 3,
    #         style = "margin-top: 25px;",
    #         shiny::actionButton(
    #             inputId = ns("copybtn"),
    #             label = "Copy to clipboard",
    #             class = "btn-info",
    #             icon = shiny::icon("copy")
    #         )
    #     )),
    #     shiny::br(),
    #
    #     shiny::verbatimTextOutput(ns("codeprint"))
    #     # shiny::uiOutput(ns("codeprint")))
    # )
}
#------------------------------------------------------------------------------#
# MODULE SERVER ----

#' Server Module:  Selection Annotator
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param df_label string, name of original df input
#' @param filter_df reactiveValue data frame with filter statements and scoping lvl
#' @param gvar reactive character, grouping vars for \code{dplyr::group_by}
#' @param statements reactive, lgl, vector of working statements
#' @param sel_points reactiveValue, data frame with selected point keys, annotations, and selection count
#' @param overwrite reacive value, TRUE/FALSE from checkbox input
#' @param is_on_disk Logical, whether df represented by \code{df_label} was on disk or from interactive \code{R} use
#' @param out_path reactive, List, with character strings providing directory paths and file names for saving/reading in code output
#'
#' @importFrom rlang .data
#'
module_server_extract_code  <-
    function(input,
             output,
             session,
             df_label,
             filter_df,
             gvar,
             statements,
             sel_points,
             overwrite,
             is_on_disk,
             out_path) {

        ns = session$ns

        # grab only valid statements

        rv_text_out <- shiny::reactive({

            # handle initialization
            if(!is.logical(overwrite())){
                # overwrite() <- TRUEove
                overwrite <- shiny::reactive(TRUE)
            }

            # 'fail' early if nothing selected/filtered
            if(nrow(sel_points$df) == 0 &
               !any(statements())
            ){
                # text_out <- "## data has not been cleaned yet"
                text_out <- NULL

            } else {

                if(!is_on_disk){
                    text_out <- text_out_interactive(sel_points$df,statements(),filter_df(),df_label,overwrite(),gvar())
                } else if(is_on_disk){
                    # ONLY FOR TESTING PURPOSES!
                    # MAKE DEDICATED FUNCTION FOR ON-DISK
                    text_out <- text_out_file(sel_points$df,statements(),filter_df(),df_label,overwrite(),gvar(),out_path())
                }
            }

            if(!is.null(text_out)){
                text_out <-
                    paste(
                        formatR::tidy_source(
                            text = text_out,
                            output = FALSE,
                            width = 100,
                            wrap = FALSE
                        )$text.tidy,
                        collapse = "\n"
                    )
            } else {
                NULL
            }

        })

        # UI
        # Rendering ---------------------------------------------------------------

        output$codeprint <- shiny::renderText(
            {
                shiny::req(rv_text_out())
                rv_text_out()
            }
        )


        output$codeDiv <- shiny::renderUI({

            # shiny::req(rv_text_out())

            shiny::validate(shiny::need(rv_text_out(),
                                        "Filter or manually select data to generate the code recipe."))

            shiny::tagList(
                shiny::verbatimTextOutput(ns("codeprint"))
            )}
        )


        return(rv_text_out)

    }

