#' Displays a neatly formatted contingency table.
#'
#' This function provides a default means of converting a contingency table
#' into HTML or LaTeX for publishing. By default, multiple column and row
#' spanning cells are formed to accentuate the hierarchical nature of the data.
#' The output of this function is a \code{kable} object and so can be further
#' manipulated.
#'
#' @param table A \code{contintab} object, output by \code{contingency_table}.
#' @param format A string specifying output format passed to \code{knitr::kable}.
#'   Currently only 'html' and 'pdf' are supported.
#' @param ... Other arguments passed to \code{knitr::kable}.
#'
#' @return A \code{kable} object.
#'
#' @examples
#'
#' # This example uses a dummy data set of whether an individual was treated or not
#' treat <- data.frame(age=abs(rnorm(100, 60, 20)),
#'                     sex=factor(sample(c("M", "F"), 100, replace=TRUE)),
#'                     variant=factor(sample(c("A", "B"), 100, replace=TRUE)),
#'                     treated=factor(sample(c("Yes", "No"), 100, replace=TRUE),
#'                                    levels=c("Yes", "No")))
#' treat$agebin <- cut(treat$age, breaks=c(0, 40, 60, 80, 9999),
#'                     labels=c("0-40", "41-60", "61-80", "80+"))
#'
#' tab <- contingency_table(list("Age"='agebin', "Sex"='sex'),
#'                          outcomes=list('Treated'='treated'),
#'                          crosstab_funcs=list(freq()),
#'                          col_funcs=list("Mean age"=summary_mean('age')),
#'                          data=treat)
#'
#' # For use in an Rmarkdown that outputs to HTML
#' neat_table(tab, 'html')
#'
#' # When outputting to PDF, the \code{booktabs} option produces well-formatted tables
#' neat_table(tab, 'latex', booktabs=TRUE)
#' @export
neat_table <- function(table, format=c("html", "latex"), ...) {
    format <- match.arg(format)
    mat <- table$mat
    content <- mat[(table$num_headers+1):nrow(mat), ]

    # Remove empty rows added for ASCII
    is_empty <- apply(content, 1, function(row) all(sapply(row, function(cell) cell == "")))
    content <- content[!is_empty, ]

    # Remove first column and set any col func labels to the next column
    ncolfuncs <- length(table$col_func_labels)
    if (ncolfuncs > 0) {
        start_index <- 2 + sum(sapply(table$cat_levels, length))
        for (i in seq(ncolfuncs)) {
            content[start_index, 2] <- content[start_index, 1]
        }
    }
    content <- content[, -1]

    header <- mat[table$num_headers, -1]
    obj <- knitr::kable(content, col.names=header, format=format, ...)

    # Group categorical variables together
    indices <- sapply(table$cat_levels, length)
    labels <- names(table$cat_levels)
    if (!is.null(table$crosstab_funcs) | table$frequency) {
        indices <- c(1, indices)
        labels <- c(" ", labels)
    }
    names(indices) <- labels
    obj <- kableExtra::group_rows(obj, index=indices)

    if (table$num_headers > 1) {
        nrowfuncs <- length(table$row_func_labels)
        cnames <- c(rep(" ", 1 + as.numeric(table$frequency)),
                    names(table$outcomes),
                    rep(" ", nrowfuncs))
        widths <- c(rep(1, 1 + as.numeric(table$frequency)),
                    sapply(table$outcome_levels, length),
                    rep(1, nrowfuncs))
        names(widths) <- cnames
        obj <- kableExtra::kable_styling(obj, full_width=TRUE)
        obj <- kableExtra::add_header_above(obj, widths)
    }
    obj
}