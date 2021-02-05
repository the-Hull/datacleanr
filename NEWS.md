# datacleanr 1.0.1

* `dcr_checks:`  
  - Added support for `fs_path`-type filepaths (from `fs` package) when launching `dcr_app()` with a path to an`*.Rds` file.
  - Added check for existing `.annotation` column, which produces useful error when present.  
  - Added check for data sets above 1.5 million observations (rows) - this threshold is at the limit of modern browsers for visualizing points with D3/plotly.
* Updated minimum versions for `R` and dependencies
* changed `summarytools::view()` to `print()` for data overview (dropping dependency on X11 / XQuartz)


# datacleanr 1.0.0

* First major version with features for exploration, filtering, visualizing and cleaning/annotating data in a reproducible manner.
* Added a `NEWS.md` file to track changes to the package.
