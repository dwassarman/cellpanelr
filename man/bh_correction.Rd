% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_cor_expression.R
\name{bh_correction}
\alias{bh_correction}
\title{Benjamini-Hochberg correction for p-value significance}
\usage{
bh_correction(x, fdr)
}
\arguments{
\item{x}{vector of p-values. MUST be ordered from smallest to largest!}

\item{fdr}{False discovery rate. Must be between 0 and 1.}
}
\value{
Logical vector of same length as \code{x} indicating whether the
  given p-values were sufficient to reject the null hypothesis with the
  given false discovery rate.
}
\description{
Determine significance of p-values after repeated hypothesis testing using the
  Benjamini-Hochberg correction.
}
