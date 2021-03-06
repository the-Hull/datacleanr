% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{filter_scoped_df}
\alias{filter_scoped_df}
\title{Filter / Subset data \code{dplyr}-groupwise}
\usage{
filter_scoped_df(dframe, condition_df)
}
\arguments{
\item{dframe}{A grouped or ungrouped \code{tibble} or \code{data.frame}}

\item{condition_df}{A \code{tibble} with two columns; \code{condition_df[ ,1]} with
\code{character} strings which evaluate to valid logical expressions applicable in
\code{\link{subset}} or \code{\link[dplyr]{filter}}, and \code{condition_df[ ,2]},
a list-column with group scoping levels (\code{numeric}) or \code{NULL} for
unscoped filtering. If all groups are given for a statement, the operation is
the same as for a grouped \code{data.frame} in \code{\link[dplyr]{filter}}.}
}
\value{
An object of the same type as \code{dframe}. The output is a subset of
the input, with groups and rows appearing in the same order, and an additional column
\code{.dcrindex} representing the group indices.
The output may have less groups as the input, depending on subsetting.
}
\description{
\code{filter_scoped_df} subsets rows of a data frame based on grouping structure
(see \code{\link[dplyr]{group_by}}). Filtering statements are provided in a separate \code{tibble}
where each row represents a combination of a logical expression and a list of groups
to which the expression should be applied to corresponding to see indices from
\code{\link[dplyr]{cur_group_id}}).
}
\details{
This function is applied in the "Filtering" tab of the \code{datacleanr} app,
and applied in the reproducible code recipe in the "Extract" tab.
Note, that multiple checks for valid statements are performed in the app (and only valid operations
printed in the "Extract" tab). It is therefore not advisable to manually alter this code or use
this function interactively.
}
\examples{
# set-up condition_df
cdf <- dplyr::tibble(
  statement = c(
    "Sepal.Width > quantile(Sepal.Width, 0.1)",
    "Petal.Width > quantile(Petal.Width, 0.1)",
    "Petal.Length > quantile(Petal.Length, 0.8)"
  ),
  scope_at = list(NULL, NULL, c(1, 2))
)


fdf <- filter_scoped_df(
  dplyr::group_by(
    iris,
    Species
  ),
  condition_df = cdf
)

# Example of invalid expression:
# column 'Spec' does not exist in iris
# "Spec == 'setosa'"
}
