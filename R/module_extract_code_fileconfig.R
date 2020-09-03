


#------------------------------------------------------------------------------#
# MODULE UI ----
#' UI Module: Extraction File selection menu
#'
#' @param id Character string
#'
module_ui_extract_code_fileconfig <- function(id) {
    ns <- shiny::NS(id)



    shiny::tagList(
        shiny::uiOutput(ns("codeconfig")),
        shiny::uiOutput(ns("codebuttons")),
        shiny::uiOutput(ns("fileRawExportConfig")),
        shiny::uiOutput(ns("fileCleanedExportConfig")),
        shiny::uiOutput(ns("savebutton"))
    )


}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

#' UI Module: Extraction File selection menu
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param df_label string, name of original df input
#' @param is_on_disk Logical, whether df represented by \code{df_label} was on disk or from interactive \code{R} use
#'
#' @importFrom rlang .data
#'
module_server_extract_code_fileconfig  <- function(input,
                                                   output,
                                                   session,
             df_label,
             is_on_disk)
    {

        ns = session$ns

        # grab only valid statements



        output$codeconfig <- shiny::renderUI({
            shiny::tagList(

                # shiny::h4(shiny::tags$strong("Code config")),
                shiny::br(),

                shiny::checkboxInput(ns("overwrite"),
                                     label = "Concise code?",
                                     value = TRUE))
        })



        output$codebuttons <- shiny::renderUI({
            shiny::tagList(shiny::fluidRow(

                shiny::br(),
            shiny::column(
                width = 6,
                align = "left",
                # style = "margin-top: 25px;",
                shiny::actionButton(
                    inputId = ns("codebtn"),
                    label = "Send to RStudio",
                    class = "btn-secondary",
                    icon = shiny::icon("share-square")
                )
            ),
            shiny::column(
                width = 6,
                align = "center",
                # style = "margin-top: 25px;",
                shiny::actionButton(
                    inputId = ns("copybtn"),
                    label = "Copy to clipboard",
                    class = "btn-secondary",
                    icon = shiny::icon("copy")
                )
            ))

        )
            })


        output$fileRawExportConfig <- shiny::renderUI(
            {
                req(is_on_disk)

                            shiny::tagList(
                                shiny::br(),
                shiny::h4(shiny::tags$strong("File outputs")),
                shiny::br(),

                "Outlier (meta) data and cleaning script",
                shiny::br(),
                shiny::br(),


            shiny::fluidRow(
                shiny::column(5, shinyFiles::shinyDirButton(id = ns("dirraw"),
                                       "Folder selection",
                                       "Please choose a folder to save raw-data outputs to",
                                       FALSE,
                                       class = 'btn-info',
                                       icon = shiny::icon("folder"))),

shiny::column(7,
            shiny::checkboxInput(inputId = ns("dirchooseIdentical"),
                                 label = "Use same output folder for cleaned data?",
                                 value = TRUE)
        )
)
)
            })



        output$fileCleanedExportConfig <- shiny::renderUI({

            req(input$dirchooseIdentical == FALSE)

            shiny::tagList(
                shiny::br(),
                "Cleaned data",
                shiny::br(),
                shiny::br(),


            shinyFiles::shinyDirButton(id = ns("dirclean"),
                                       "Folder selection",
                                       "Please choose a folder to save raw-data outputs to",
                                       FALSE,
                                       class = 'btn-info',
                                       icon = shiny::icon("folder"))
            )
        })


        output$savebutton <- shiny::renderUI({

            req(is_on_disk, )

            shiny::tagList(

                shiny::br(),
                shiny::actionButton(inputId = ns("save"),
                                     label = "Save Recipe",
                                    icon = shiny::icon("save"),
                                    class = "btn-success"))
        })




# file path and selection logic ---------------------------------------------------------



        # if(is_on_disk){
        roots = c(`Dataset dir` = fs::path_dir(df_label),
            `Working dir` ='.',
                  `Home dir` = Sys.getenv("HOME"))

            shinyFiles::shinyDirChoose(input,
                                       id = "dirraw",
                                       roots=roots)

            shinyFiles::shinyDirChoose(input,
                                       id = "dirclean",
                                       roots=roots)
#
#             shiny::observeEvent(input$dirchoose,
#                                 ignoreNULL = TRUE,
#                                 ignoreInit = TRUE,
#                                 {
#                 print(paste("test", shinyFiles::parseDirPath(roots = roots,
#                                                input$dirchoose)))
#                 })


            outpath <- shiny::reactive({

                dirraw <- shinyFiles::parseDirPath(roots = roots,
                                                   input$dirraw)

                dirraw <- ifelse(length(dirraw) == 0,
                                 fs::path_dir(df_label),
                                 dirraw)

                dirclean <- shinyFiles::parseDirPath(roots = roots,
                                                    input$dirclean)
                dirclean <- ifelse(length(dirclean) == 0,
                       dirraw, dirclean)


                file_out_raw <- make_save_filepath(
                    save_dir = dirraw,
                    input_filepath = df_label,
                    suffix = "meta_RAW",
                    ext = "Rds")

                file_out_cleaned <- make_save_filepath(
                    save_dir = dirclean,
                    input_filepath = df_label,
                    suffix = "CLEAN",
                    ext = "Rds")


                file_script_cleaning <- make_save_filepath(
                    save_dir = dirraw,
                    input_filepath = df_label,
                    suffix = "cleaning_recipe",
                    ext = "R")

                return(list(
                    dirraw = dirraw,
                    dirclean = dirclean,
                    file_out_raw = file_out_raw,
                    file_out_cleaned = file_out_cleaned,
                    file_script_cleaning = file_script_cleaning))


            })


            return(outpath)

}
