% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_expression.R
\name{exp_tooltip}
\alias{exp_tooltip}
\title{Render a tooltip when user hovers mouse over plot}
\usage{
exp_tooltip(hover, data, cell, response)
}
\arguments{
\item{hover}{Hover input from plot}

\item{data}{A tibble of user-uploaded data}

\item{cell}{Name of cell line column (to be presented)}

\item{response}{Name of response column (y-axis on plot)}
}
\value{
wellPanel located at the cursor containing the cell line name of
  the nearest point
}
\description{
Render a tooltip when user hovers mouse over plot
}
