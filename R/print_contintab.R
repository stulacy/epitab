print.contintab <- function(x, ...) {
    cont <- x$content
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
    rows <- c(rows[1], header_row, rows[-1])

    # Now combine into single value with line breaks and cat it
    full_tab <- paste(rows, collapse='\n')
    cat(full_tab)
}

