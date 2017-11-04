build_continuous_summary <- function(func, var, digits=2) {
    function(outcome, data) {
        sapply(levels(data[[outcome]]), function(out) {
            sub_data <- data[data[[outcome]]==out, ]
            round(func(sub_data[, var]), digits)
        })

    }
}



#' Calculates the mean of a specified continuous variable
#' for each level of an outcome.
#'
#' @param var A continuous variable name as a string.
#' @return A function that calculates the mean value of \code{var}
#' for each outcome level.
#' @export
summary_mean <- function(var) {
    build_continuous_summary(mean, var)
}

#' Calculates the median of a specified continuous variable
#' for each level of an outcome.
#'
#' @param var A continuous variable name as a string.
#' @return A function that calculates the median value of \code{var}
#' for each outcome level.
#' @export
summary_median <- function(var) {
    build_continuous_summary(median, var)
}