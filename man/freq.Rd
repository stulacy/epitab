% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crosstab_functions.R
\name{freq}
\alias{freq}
\title{Builds a function to calculate cross-tabulated frequencies}
\usage{
freq(proportion = c("column", "row", "none"), display = c("percentage",
  "ratio"), digits = 3, missing = NULL)
}
\arguments{
\item{proportion}{Whether to display proportions in brackets after the counts
as either row-wise or column-wise, or not at all.}

\item{display}{How to display the proportions if required.}

\item{digits}{The number of digits to specify \code{proportion} to.}

\item{missing}{Specifies covariates that shouldn't be included in calculating proportions
for column-wise summaries.}
}
\value{
A function that calculates the frequency of a cell in a contingency table.
}
\description{
Builds a function to calculate cross-tabulated frequencies
}
