print.crosstab <- function(x, ...) {
    spaces <- function(n) paste(rep(" ", n), collapse='')
    cat("Here's my object!")
    cont <- x$content
    cat_vars <- names(cont)
    cross <- x$cross_refs
    funcs <- x$funcs

    # First column is for variable names + buffer
    len_first_col <- max(sapply(cat_vars, nchar)) + 2
    # TODO How to generate this? maximum level size?
    len_second_col <- 10
    header <- paste(spaces(len_first_col),
                    spaces(len_second_col),
                    paste0(spaces(5), cross, spaces(5)),
                    paste0(spaces(3), funcs, spaces(3), collapse='|'),
                    sep='|')
    hrow <- paste(rep("-", nchar(header)), collapse='')

    # First row is overall totals
    x$overall_counts
    first_row <- paste(spaces(len_first_col),
                       "Total",
                       "R ")




}
