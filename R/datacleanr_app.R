#' Interactive data cleaning
#'
#' @param dframe dataframe, tbl or data.table with data for viewing and/or cleaning
#' @param browser logical, should app start in OS's default browser? (default \code{TRUE})
#'
#' @details \code{datacleanr} provides an interactive data overview, and allows
#'  reproducible subsetting and (manual, interactive) visual outlier detection and annotation.
#'  The app works with \code{data.frame}, \code{tibble} and \code{data.table} objects.
#'  Note, that spatial data is supported; this requires that \code{lon} and \code{lat} columns in degrees
#'  are present.
#'  The interactive visualization relies on subsetting (i.e. highlighting) sub-groups of the provided data.
#'  These groups are composed of unique combinations of columns in the data set (must be \code{factor}),
#'  and are carried through the app for exploratory analyses (tab \strong{Overview}), and plotting
#'  (tab \strong{Visualization}).
#'  These groups should ideally be chosen to facilitate a convenient viewing and cleaning process.
#'  For example, a data set with time series of multiple sensors could be grouped by sensor and/or additional columns,
#'  such that periods of interest can be visualized and cleaned simultaneously in the interactive plot.
#'  The tab \strong{Extraction} provides code to reproduce the entire procedure, which can be copied,
#'  or sent directly to an active \code{RStudio} script.
#'
#' @export
dcr_app <- function(dframe, browser = TRUE){



    dcr_checks(dframe)
    opts_list <- if(browser) {
        list(launch.browser = browser)
    } else {
        list()
    }

    df_name <- deparse(substitute(dframe))

    shiny::shinyApp(ui     = datacleanr_ui,
                    server = function(input, output, session){
                        datacleanr_server(input,
                                          output,
                                          session,
                                          dataset = dframe,
                                          df_name = df_name)},
                    enableBookmarking = "server",
                    options = opts_list
    )

}



