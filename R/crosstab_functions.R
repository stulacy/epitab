#' Builds a function to calculate cross-tabulated frequencies
#'
#' @param proportion If 'percentage' or 'ratio', the proportion of counts
#' is included in parentheses.
#' @param digits The number of digits to specify \code{proportion} to.
#' @return A function that calculates the frequency of a cell in a contingency table.
#'
#' @export
freq <- function(proportion=c("percentage", "ratio", "none"), digits=2) {
    proportion <- match.arg(proportion)

    function(out_level, out_var, data) {

        level_count <- sum(data[[out_var]] == out_level)

        if (proportion != "none") {
            prop <- level_count / sum(nrow(data))
            prop <- round(prop, digits)
            if (proportion == "percentage")
                prop <- prop * 100

            outcome <- paste0(level_count, " (", prop, ")")
        } else {
            outcome <- level_count
        }
        outcome
    }
}