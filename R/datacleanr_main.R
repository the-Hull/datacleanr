#' Data cleanr
#'
#' @param dframe dataframe
#'
#' @return idxs of selected data
#' @export
#'
#'
datacleanr_module <- function(dframe, viewer = "browser"){





    df_name <- deparse(substitute(dframe))



    if (viewer == "browser") {
        inviewer <- shiny::browserViewer(browser = getOption("browser"))
    } else if (viewer == "pane") {
        inviewer <- shiny::paneViewer(minHeight = "maximize")
    } else {
        inviewer <- shiny::dialogViewer(
            dialogName = "Clean it up!",
            width = 1200,
            height = 850
        )
    }




    shiny::runGadget(app     = datacleanr_ui(id = "dcr",
                                             dataset = dframe),
                     server = function(input, output, session){

                         shiny::callModule(
                             module = datacleanr_server,
                             id = "dcr",
                             dataset = dframe,
                             df_name = df_name
                         )
                     },
                     viewer = inviewer
    )




}




