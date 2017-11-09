## epitab

[![Travis-CI Build Status](https://travis-ci.org/stulacy/epitab.svg?branch=master)](https://travis-ci.org/stulacy/epitab)

Provides contingency tables for descriptive stats for epidemiology in R. Alongside the cross-tabulated frequencies, `epitab` can provide summary statistics such as coefficients from regression models, and summary statistics of continuous variables with respect to the outcome of interest. A variety of useful summary measures are provided with `epitab`, but the package is also designed in such a way to allow the user to supply their own statistics, allowing for a flexible approach to table design that can be employed in a variety of situations. The ability to export the resulting tables to publication friendly formats is also provided, including HTML, LaTeX, and Word.

## Usage

See the *User Guide* vignette for examples of the tables that can be built, and how to export these to other documents.

## Installation

The latest stable release can be installed from CRAN by running `install.packages('epitab')`, while the latest development version can be installed from [the development Github repository](https://github.com/stulacy/epitab) by using `devtools::install_github('stulacy/epitab')`.
