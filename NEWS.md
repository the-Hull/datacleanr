# datacleanr 1.0.4

* improved plotting performance of large, "narrowly distributed" distributed data, such as a 1:1 scatter or a high resolution time series (i.e., little variation in either x or y dimension). 
The low performance seems to come from changes made in how hover events are handled in `plotly` in newer versions (2.x.x). See [this issue](https://github.com/plotly/plotly.js/issues/5790#issuecomment-1067844436), which is still unresolved at 06 June, 2022. 
The fix uses the default `hovermode = "closest"`, but sets `hoverdistance = 200`, which should ensure a better responsiveness in most cases.
The increased `hoverdistance` called for a disabling of hover labels to prevent confusion.
Note, the increased `hoverdistance` may cause some unintended consequences when clickin into plotting 'white space', as the click snaps to the closest point within `200` pixels.

# datacleanr 1.0.3

* fixed `filter_scoped()` test to conform with `dplyr 1.0.8` release (dropped dplyr error class)

# datacleanr 1.0.2

* removed `navbarPageWithInputs`, replaced with `shiny::navbarpage()` and `bslib::nav_item()`, as recommended
* adjusted button placement (close/exit) with css styles 
* `plotly` no longer serves `Plotly.d3`, which was used to select individual traces on the plot in the viz tab; it now requires additional import. This is currently done via a D3 script dependency from a CDN, which requires an internet connection (checked by `can_internet()`


# datacleanr 1.0.1

* `dcr_checks:`  
  - Added support for `fs_path`-type filepaths (from `fs` package) when launching `dcr_app()` with a path to an`*.Rds` file.
  - Added check for existing `.annotation` column, which produces useful error when present.  
  - Added check for data sets above 1.5 million observations (rows) - this threshold is at the limit of modern browsers for visualizing points with D3/plotly.
* Updated minimum versions for `R` and dependencies
* Changed `summarytools::view()` to `print()` for data overview (dropping dependency on X11 / XQuartz)
* Separated setting grouping structure and enabling filtering, plotting, and extracting from data frame summary.
This saves significant time for larger data sets.
Help links and panel texts were adjusted accordingly.


# datacleanr 1.0.0

* First major version with features for exploration, filtering, visualizing and cleaning/annotating data in a reproducible manner.
* Added a `NEWS.md` file to track changes to the package.
