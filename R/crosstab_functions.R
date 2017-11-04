#' Calculates cross-tabulated frequencies
#'
#' @param proportion Whether to include the proportion of counts this represents in parantheses.
#' @param digits The number of digits to specify \code{proportion} to.
#' @return A function that calculates the frequency of a cell in a contingency table.
#'
#' @export
freq <- function(proportion=c("percentage", "ratio", "none"), digits=2) {
    proportion <- match.arg(proportion)

    function(out_level, out_var, data, ind_level=NULL, ind_var=NULL) {
        if (!is.null(ind_level) & !is.null(ind_var))
            data <- data[data[[ind_var]]==ind_level, ]

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