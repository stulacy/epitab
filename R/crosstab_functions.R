#' Builds a function to calculate cross-tabulated frequencies
#'
#' @param proportion Whether to display proportions in brackets after the counts
#'   as either row-wise or column-wise, or not at all.
#' @param display How to display the proportions if required.
#' @param digits The number of digits to specify \code{proportion} to.
#' @return A function that calculates the frequency of a cell in a contingency table.
#'
#' @export
freq <- function(proportion=c("column", "row", "none"),
                 display=c("percentage", "ratio"),
                 digits=2) {
    proportion <- match.arg(proportion)
    display <- match.arg(display)

    function(out_level, out_var, data, in_level=NULL, in_var=NULL) {

        level_count <- if (is.null(in_level) | is.null(in_var)) {
            sum(data[[out_var]] == out_level)
        } else {
            sum(data[[out_var]] == out_level & data[[in_var]] == in_level)
        }

        if (proportion != "none") {
            denom <- if (proportion == 'row') {
                if (is.null(in_var) | is.null(in_level)) {
                    level_count
                } else {
                    sum(data[[in_var]] == in_level)
                }
            } else {
                sum(data[[out_var]] == out_level)
            }
            prop <- level_count / denom
            prop <- round(prop, digits)

            if (display == "percentage") {
                prop <- prop * 100
            }

            outcome <- paste0(level_count, " (", prop, ")")
        } else {
            outcome <- level_count
        }
        outcome
    }
}