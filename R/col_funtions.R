build_continuous_summary <- function(func, var, digits=2) {
    function(data, outlevel=NULL, outcome=NULL) {
        if (!is.null(outlevel) && !is.null(outcome))
            data <- data[data[[outcome]] == outlevel, ]
        round(func(data[, var]), digits)
    }
}



#' Builds a function to calculate the mean of a
#' continuous variable for each level of an outcome.
#'
#' @param var A continuous variable name as a string.
#' @return A function that calculates the mean value of \code{var}
#' for each outcome level.
#' @export
summary_mean <- function(var) {
    build_continuous_summary(mean, var)
}

#' Builds a function to calculate the median of a
#' continuous variable for each level of an outcome.
#'
#' @inheritParams summary_mean
#' @return A function that calculates the median value of \code{var}
#' for each outcome level.
#' @export
summary_median <- function(var) {
    build_continuous_summary(stats::median, var)
}