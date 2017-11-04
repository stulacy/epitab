#' Displays a neatly formatted table.
#'
#' @param table A \code{contintab} object.
#' @param format A string specifying output format passed to \code{knitr::kable}.
#'   Currently only 'html' and 'pdf' are supported.
#' @param ... Other arguments passed to \code{knitr::kable}.
#'
#' @return A \code{kable} object.
#'
#' @importFrom magrittr %>%
#' @export
neat_table <- function(table, format=c("html", "pdf"), ...) {
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
    indices <- c(1, sapply(table$cat_levels, length))
    labels <- c(" ", names(table$cat_levels))
    names(indices) <- labels
    obj <- obj %>% kableExtra::group_rows(index=indices)

    if (table$num_headers > 1) {
        nrowfuncs <- length(table$row_func_labels)
        cnames <- c(rep(" ", 1 + as.numeric(table$frequency)),
                    names(table$outcomes),
                    rep(" ", nrowfuncs))
        widths <- c(rep(1, 1 + as.numeric(table$frequency)),
                    sapply(table$outcome_levels, length),
                    rep(1, nrowfuncs))
        names(widths) <- cnames
        obj <- obj %>% kableExtra::kable_styling(full_width=TRUE) %>% kableExtra::add_header_above(widths)
    }
    obj
}