#' Interactive and reproducible data cleaning
#'
#' Launches the `datacleanr` app for interactive and reproducible cleaning.
#' See Details for more information.
#'
#' @param dframe Character, a string naming a `data.frame`, `tbl` or `data.table` in the environment
#'   or a path to a `.Rds` file. **Note, that `data.table`s are converted to tibbles internally.**`
#' @param browser logical, should app start in OS's default browser? (default \code{TRUE})
#'
#' @details `datacleanr` provides an interactive data overview, and allows
#'  reproducible filtering and (manual, interactive) visual outlier detection and annotation across multiple app tabs:
#'
#'    - **Overview and Set-up**: set groups (see below) and generate a exploratory summary of `dframe`
#'    - **Filtering**: Provide and apply filter statements (groupwise, see below and \code{\link{filter_scoped_df}})
#'    - **Visualization and Annotating**: interactive visualization allowing outlier highlighting, annotating and before/after histograms of displayed (numeric) variables
#'    - **Extraction**: generates *Reproducible Recipe* and outputs
#'
#'  For data sets exceeding 1.5 million rows, we suggest splitting the data, if possible, by a grouping factor.
#'  This is because at this volume interactive visualizations using \code{\link{plotly}} stretch the limits of what modern web browsers can handle.
#'  A simple example using \code{\link{iris}} is:
#'
#'   ```
#'   iris_split <- split(iris, iris$Species)
#'   dcr_app(iris_split[[1]])
#'   # or
#'   lapply(iris_split, dcr_app)
#'
#'    ```
#'
#'  Extensive documentation is provided on each of the tabs for individual procedures in help links.
#'  `datacleanr` relies on 1) generating a column of unique IDs (`.dcrkey`) and subsetting `dframe` into sub-groups (generated in-app,
#'  added as column `.dcrindex`) for filtering and visualization.
#'  These groups are composed of unique combinations of columns in the data set (must be `factor`) and are passed to \code{\link[dplyr]{group_by}},
#'  and are carried through the app for exploratory analyses (tab **Overview and Set-up**), filtering (tab **Filtering**) and plotting
#'  (tab **Visualization**).
#'  These groups should ideally be chosen to facilitate a convenient filtering and viewing/cleaning process.
#'  For example, a data set with time series of multiple sensors could be grouped by sensor and/or additional columns,
#'  such that periods of interest can be visualized and cleaned simultaneously in the interactive plot.
#'
#'  Filtering is achieved by providing expressions that evaluate to `TRUE` \ `FALSE`, and can be applied to the entire
#'  data set, or individual/all groups via scoped filtering (see \code{\link{filter_scoped_df}}).
#'
#'  The interactive visualization allows selecting and deselecting points with lasso and box select tools,
#'  as well as interactive zooming (toolbar or clicking on legend items or group overview table, see tab in-app)
#'   as well as panning (toolbar and hover over plot's axes).
#'  Data formats supported are
#'
#'    1. Observational (numeric), timeseries (`POSIXct`) and categorical data in `x` and `y` dimensions/axis
#'    2. Observational (numeric) data in `z` dimension (point size)
#'    3. Spatial data, when `lon` and `lat` in decimal degrees are present in `x` and `y`.
#'
#'  Displaying spatial data requires a [Mapbox](https://www.mapbox.com/) account, from which an access token needs
#'  to be copied into your `.Renviron` (e.g. `MAPBOX_TOKEN=your_copied_token`).
#'
#'  Note, that when a column `.dcrflag` (logical, `TRUE` \ `FALSE`) is present in `dframe`,
#'  respective observations are given contrasting
#'  symbols (`FALSE` = circle, `TRUE` = star-triangle).
#'  This column is employed as a cross-referencing tool for e.g.other outlier detection or data-processing algorithms
#'  that were applied prior.
#'
#'
#'  The tab **Extraction** provides code to reproduce the entire procedure (a *Reproducible Recipe*), which
#'  1. can be copied, or sent directly to an active `RStudio` script when used interactively (i.e. when `dframe` is an object in `R`'s
#'  environment),
#'  2. can be saved to disk with intermediate outputs (filter statements and selected outliers),
#'    where file names are based on the input file and configurable suffixes when `dframe` is a path.
#'
#' @return When `datacleanr` is ended by clicking on `Close` in the app's navigation bar, a list is **invisibly** returned
#'  with the following items:
#'
#'  1. **df_name**: character, object name/file path passed into `dcr_app`
#'  2. **dcr_df**: tibble, filtered data set **with** additional columns `.dcrkey`, `.dcrindex`, `.annotation` - the latter is `NA` for non-outliers, an empty string for outliers without annotation, and a custom string for annotated outliers
#'  3. **dcr_selected_outliers**: data.frame, contains the outlier `.dcrkey`, the `.annotation` and a `selection_count` (integer, count incrementer) column
#'  4. **dcr_groups**: character, a vector defining the groups (via \code{\link[dplyr]{group_by}}) used throughout `datacleanr`
#'  5. **dcr_condition_df**: tibble, with columns `filter` (character, statement used for filtering) and `group` (list, of integers), defining groups that correspond to `.dcrindex`
#'  6. **dcr_code**: character string, containing *Reproducible Recipe*
#'
#'
#' @export
dcr_app <- function(dframe, browser = TRUE) {



  if(!can_internet()){
    stop("Please connec to the internet. datacleanr currently relies on an internet connection to provide d3 methods for plotting and data selection. \n These will be packaged into an upcoming version to allow offline use.\n")
  }

    use_data <- dcr_checks(dframe)



  opts_list <- if (browser) {
    list(launch.browser = browser)
  } else {
    list()
  }


  if (is.null(use_data$file_path)) {
    df_name <- deparse(substitute(dframe))
  } else {
    df_name <- use_data$file_path
  }



  base::suppressWarnings(
    shiny::runApp(
      appDir =

        shiny::shinyApp(
          ui = datacleanr_ui,
          server = function(input, output, session) {
            datacleanr_server(input,
              output,
              session,
              dataset = use_data$dataset,
              df_name = df_name,
              is_on_disk = !is.null(use_data$file_path)
            )
          },
          options = opts_list
        )
    )
  )
}
