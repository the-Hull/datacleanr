#' Data cleanr
#'
#' @param dframe dataframe
#'
#' @export
#'
#'
datacleanr_module <- function(dframe){





    df_name <- deparse(substitute(dframe))



    # if (viewer == "browser") {
    #     vw <- shiny::browserViewer(browser = getOption("browser"))
    # } else if (viewer == "pane") {
    #     vw <- shiny::paneViewer(minHeight = "maximize")
    # } else {
    #     vw <- shiny::dialogViewer(
    #         dialogName = "Clean it up!",
    #         width = 1200,
    #         height = 850
    #     )
    # }
    #



    # shiny::runGadget(app     = datacleanr_ui(id = "dcr",
    shiny::shinyApp(ui     = datacleanr_ui,
                    server = function(input, output, session){
                        datacleanr_server(input,
                                          output,
                                          session,
                                          dataset = dframe,
                                          df_name = df_name)},
                    enableBookmarking = "server"
    )
    # # shiny::runGadget(app     = datacleanr_ui(id = "dcr",
    # shiny::runGadget(app     = datacleanr_ui(id = "dcr",
    #                                          dataset = dframe),
    #                  server = function(input, output, session){
    #
    #                      shiny::callModule(
    #                          module = datacleanr_server,
    #                          id = "dcr",
    #                          dataset = dframe,
    #                          df_name = df_name
    #                      )
    #                  },
    #                  viewer = vw
    # )




}




