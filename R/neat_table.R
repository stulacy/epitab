#' Displays a neatly formatted table.
#'
#' @param table A \code{crosstab} object.
#'
#' @return A \code{kable} object.
#'
#' @importFrom magrittr %>%
#' @export
#' TODO Add funtionality for HTML and LaTeX
neat_table <- function(table, format="html") {
    mat <- table$mat
    content <- mat[(table$num_headers+1):nrow(mat), ]
    header <- mat[table$num_headers, ]
    obj <- knitr::kable(content, col.names=header, format=format)
    if (table$num_headers > 1) {
        cnames <- c(rep(" ", 2 + as.numeric(table$frequency)),
                    table$outcome_label,
                    rep(" ", length(table$funcs)))
        widths <- c(rep(1, 2 + as.numeric(table$frequency)),
                    length(table$overall_counts),
                    rep(1, length(table$funcs))
                    )
        names(widths) <- cnames
        obj <- obj %>% kableExtra::add_header_above(widths)
    }
    obj
}