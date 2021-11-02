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
