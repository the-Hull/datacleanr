


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
        shiny::uiOutput(ns("fileRawExportConfig")),
        shiny::uiOutput(ns("fileCleanedExportConfig"))
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

                shiny::h4(shiny::tags$strong("Code config")),
                shiny::br(),

                shiny::checkboxInput(ns("overwrite"),
                                     label = "Concise code?",
                                     value = FALSE))
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


        # if(is_on_disk){
        roots = c(wd='.')

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


}
