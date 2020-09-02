


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
        shiny::uiOutput(ns("fileexportconfig"))
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



        output$fileexportconfig <- shiny::renderUI(

            {
                # req(is_on_disk)

            shiny::tagList(
                shiny::h4(shiny::tags$strong("File output config")),
                shiny::br(),

            shinyFiles::shinyDirButton(id = ns("dirchoose"),
                                       "Folder selection",
                                       "Please choose a folder to save raw-data outputs to",
                                       FALSE,
                                       class = 'btn-info',
                                       icon = shiny::icon("folder")),


        )
            })


        # if(is_on_disk){
        roots = c(wd='.')

            shinyFiles::shinyDirChoose(input,
                                       id = "dirchoose",
                                       roots=roots)

            shiny::observeEvent(input$dirchoose,
                                ignoreNULL = TRUE,
                                ignoreInit = TRUE,
                                {
                print(paste("test", shinyFiles::parseDirPath(roots = roots,
                                               input$dirchoose)))
                })




        # print(getwd())
        # shinyFiles::shinyDirChoose(input = input,
        #                            id = ns("dirchoose"))
                                   # defaultPath = fs::path_dir(df_label))

        # }
        # )
}
