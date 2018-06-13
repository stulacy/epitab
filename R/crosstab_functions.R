#' Builds a function to calculate cross-tabulated frequencies
#'
#' @param proportion Whether to display proportions in brackets after the counts
#'   as either row-wise or column-wise, or not at all.
#' @param display How to display the proportions if required.
#' @param digits The number of digits to specify \code{proportion} to.
#' @param missing Specifies covariates that shouldn't be included in calculating proportions
#'   for column-wise summaries.
#' @return A function that calculates the frequency of a cell in a contingency table.
#'
#' @export
freq <- function(proportion=c("column", "row", "none"),
                 display=c("percentage", "ratio"),
                 digits=3,
                 missing=NULL) {
    proportion <- match.arg(proportion)
    display <- match.arg(display)

    function(data, outcome_level=NULL, outcome_name=NULL, independent_level=NULL, independent_name=NULL) {

        level_count <- if (is.null(independent_level) || is.null(independent_name)) {
            sum(data[[outcome_name]] == outcome_level)
        } else if (is.null(outcome_level) || is.null(outcome_name)) {
            sum(data[[independent_name]] == independent_level)
        } else {
            sum(data[[outcome_name]] == outcome_level & data[[independent_name]] == independent_level)
        }

        if (proportion == "none" || (!is.null(independent_level) && !is.null(missing) && independent_level %in% missing)) {
            outcome <- level_count
        } else {
            denom <- if (proportion == 'row') {
                if (is.null(independent_name) || is.null(independent_level)) {
                    level_count
                } else {
                    sum(data[[independent_name]] == independent_level)
                }
            } else {
                if (is.null(missing) || is.null(independent_name)) {
                    if (is.null(outcome_name) || is.null(outcome_level)) {
                        nrow(data)
                    } else {
                        sum(data[[outcome_name]] == outcome_level)
                    }
                } else {
                    if (is.null(outcome_name) || is.null(outcome_level)) {
                        nrow(data) - sum(data[[independent_name]] %in% missing)
                    } else {
                        sum(data[[outcome_name]] == outcome_level) - sum(data[[outcome_name]] == outcome_level & data[[independent_name]] %in% missing)
                    }
                }
            }
            prop <- level_count / denom
            prop <- round(prop, digits)

            if (display == "percentage") {
                prop <- prop * 100
            }

            outcome <- paste0(level_count, " (", prop, ")")
        }
        outcome
    }
}