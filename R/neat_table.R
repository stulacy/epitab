#' Displays a neatly formatted table.
#'
#' @param table A \code{crosstab} object.
#' @param format A string specifying output format passed to \code{knitr::kable}.
#'   Currently only 'html' and 'pdf' are supported.
#' @param ... Other arguments passed to \code{knitr::kable}.
#'
#' @return A \code{kable} object.
#'
#' @importFrom magrittr %>%
#' @export
neat_table <- function(table, format="html", ...) {
    mat <- table$mat
    content <- mat[(table$num_headers+1):nrow(mat), ]
    header <- mat[table$num_headers, ]
    obj <- knitr::kable(content, col.names=header, format=format, ...)
    if (table$num_headers > 1) {
        cnames <- c(rep(" ", 2 + as.numeric(table$frequency)),
                    names(table$outcomes),
                    rep(" ", length(table$funcs)))
        widths <- c(rep(1, 2 + as.numeric(table$frequency)),
                    sapply(table$outcome_levels, length),
                    rep(1, length(table$funcs))
                    )
        names(widths) <- cnames
        obj <- obj %>% kableExtra::add_header_above(widths)
    }
    obj
}