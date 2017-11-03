build_continuous_summary <- function(func, var, digits=2) {
    function(outcome, data) {
        sapply(levels(data[[outcome]]), function(out) {
            sub_data <- data[data[[outcome]]==out, ]
            round(func(sub_data[, var]), digits)
        })

    }
}



#' @export
summary_mean <- function(var) {
    build_continuous_summary(mean, var)
}

#' @export
summary_median <- function(var) {
    build_continuous_summary(median, var)
}