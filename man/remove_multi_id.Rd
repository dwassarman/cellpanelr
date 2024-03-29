% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_add_ids.R
\name{remove_multi_id}
\alias{remove_multi_id}
\title{Remove IDs that map to the same cell line}
\usage{
remove_multi_id(data, cell_col, id_col = "depmap_id")
}
\arguments{
\item{data}{A tibble}

\item{cell_col}{Name of column containing cell line names}

\item{id_col}{Name of column containing IDs}
}
\value{
A tibble with bad IDs changed to NA
}
\description{
Remove IDs that map to the same cell line
}
