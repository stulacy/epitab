#' Build a cross-referenced table
#' @export
crosstable <- function(cat_vars, cross_refs, data, functions=NULL) {
    if (length(cross_refs) == 2) {
        stop("Having 2 cross refs isn't currently supported.")
    } else if (length(cross_refs) > 2) {
        stop("Having more than 2 cross refs isn't possible.")
    } else if (length(cross_refs) == 0) {
        stop("Must specify at least one cross-reference!")
    }

    cross_ref <- cross_refs[1]

    # Calculate cross-reference freq overall
    overall <- table(data[[cross_ref]])
    overall_props <- overall / nrow(data)

    content <- setNames(lapply(cat_vars, function(var) {
        # Calculate table frequencies overall
        counts <- table(data[[var]])

        # Calculate 2x2 table frequencies with proportions
        cross_counts <- table(data[[var]], data[[cross_ref]])
        cross_props <- apply(cross_counts, 2, "/", counts)

        # Apply function
        func_vals <- lapply(functions, function(x) x(var, data))

        list(counts=counts,
             cross_counts=cross_counts,
             cross_proportion=cross_props,
             function_vals=func_vals)
    }), cat_vars)

    raw_obj <- list(content=content,
                    overall_counts=overall,
                    overall_proportion=overall_props,
                    cat_vars=cat_vars,
                    cross_refs=cross_refs,
                    funcs=names(functions))

    mat <- convert_list_to_matrix(raw_obj)

    obj <- list(content=mat,
                overall_counts=overall,
                overall_proportion=overall_props,
                cat_vars=cat_vars,
                cross_refs=cross_refs,
                ncrossrefs=length(cross_refs),
                funcs=names(functions)
                )
    class(obj) <- c('crosstab', class(obj))
    obj
}

convert_list_to_matrix <- function(x) {
    spaces <- function(n) paste(rep(" ", n), collapse='')
    cont <- x$content
    cat_vars <- names(cont)
    cross <- x$cross_refs
    funcs <- x$funcs
    nfuncs <- length(funcs)
    num_cross_levels <- length(x$overall_counts)
    cross_level_labels <- colnames(x$content[[1]]$cross_counts)

    # Setup empty matrix to hold the table
    ncols <- 3 + num_cross_levels + nfuncs
    nrows <- 4 + sum(sapply(cat_vars, function(var) length(cont[[var]]$counts) + 1))
    tab <- matrix("", nrow=nrows, ncol=ncols)

    # Add first row and line
    tab[1, 3] <- 'All'
    tab[1, 4] <- cross
    for (i in 1:nfuncs) {
        tab[1, 3 + num_cross_levels + i] <- funcs[i]
    }
    tab[2, ] <- "-"

    # First content row is the cross reference variable levels
    for (i in 1:num_cross_levels) {
        tab[3, 3+i] <- cross_level_labels[i]
    }

    # Followed by the overall counts
    tab[4, 2] <- "Total"
    tab[4, 3] <- sum(x$overall_counts)
    for (i in 1:num_cross_levels) {
        tab[4, 3+i] <- paste0(x$overall_counts[i], " (", round(x$overall_proportion[i], 2), ")")
    }

    curr_row_num <- 5

    # Then add the content split by variable
    for (cat_num in seq_along(cat_vars)) {
        var <- cat_vars[cat_num]
        varcont <- cont[[var]]
        num_levs <- length(varcont$counts)
        curr_row_num <- curr_row_num + 1  # Add a blank line between variables
        for (i in 1:num_levs) {
            if (i == 1) {
                tab[curr_row_num, 1] <- var
            }

            tab[curr_row_num, 2] <- names(varcont$counts)[i]  # name of level
            tab[curr_row_num, 3] <- varcont$counts[i]         # overall count
            for (j in 1:num_cross_levels) {              # add count per level of cross-ref var (with proportion in brackets)
                tab[curr_row_num, 3 + j] <- paste0(varcont$cross_counts[i, j], " (", round(varcont$cross_proportion[i, j], 2), ")")
            }
            # Add function vars
            for (j in seq_along(funcs)) {
                tab[curr_row_num, 3+num_cross_levels+j] <- varcont$function_vals[[funcs[j]]][i]
            }

            curr_row_num <- curr_row_num + 1
        }
    }
    tab
}