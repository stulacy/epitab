#' Prints the contingency table as an ASCII table
#'
#' @param x An object of class \code{contintab} return by
#'   \code{contingency_table}.
#' @param ... Options passed to \code{print}.
#'
#' @return None. Prints a table to standard output.
#'
#' @method print contintab
#' @export
print.contintab <- function(x, ...) {
    # Will add column headers to the main matrix now as makes it easier rather than
    # having to make exceptions for headers everywhere
    cont <- x$mat
    cont_padded <- cont

    # Pad columns
    padding <- 5
    max_col_size <- apply(apply(cont, 2, nchar), 2, max)  + padding

    # Loop through and apply padding
    dim <- dim(cont)
    for (i in seq(1, dim[1])) {
        for (j in seq(1, dim[2])) {
            cont_padded[i, j] <- sprintf(paste0("%-", max_col_size[j], "s"), cont_padded[i,j])
        }
    }

    # Next step is to apply formatting for ASCII, i.e. line breaks, column lines, and header rows

    # Firstly will add column lines to every column
    for (i in seq(1, dim[1])) {
        for (j in seq(1, dim[2])) {
            cont_padded[i, j] <- sprintf("%s|", cont_padded[i, j])
        }
    }

    # Now create a vector of rows
    rows <- apply(cont_padded, 1, function(row) paste0(row, collapse=''))

    # Check all same size
    row_length <- unique(sapply(rows, nchar))
    if (length(row_length) != 1) {
        stop("Error: Have mismatched row size, with row sizes ", row_length)
    }

    # Add header row after first column
    header_row <- paste(rep("-", row_length), collapse='')
    rows <- c(rows[1:x$num_headers], header_row, rows[-(seq(x$num_headers))])

    # Now combine into single value with line breaks and cat it
    full_tab <- paste(rows, collapse='\n')
    cat(full_tab)
}

