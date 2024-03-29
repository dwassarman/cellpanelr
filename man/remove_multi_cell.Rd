% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_add_ids.R
\name{remove_multi_cell}
\alias{remove_multi_cell}
\title{Remove IDs that map to more than one cell line}
\usage{
remove_multi_cell(data, cell_col, id_col = "depmap_id")
}
\arguments{
\item{data}{A tibble}

\item{cell_col}{Name of column containing cell line names}

\item{id_col}{Name of column containing IDs}
}
\value{
Tibble with bad IDs changed to NA
}
\description{
Remove IDs that map to more than one cell line
}
