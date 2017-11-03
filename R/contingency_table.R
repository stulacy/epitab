#' Builds a contingency table
#'
#' A contingency table summarises a data set with multiple categorical variables
#' in many ways. There is generally an outcome variable of interest, and the
#' independent variables will be cross-tabulated with this outcome. Summary statistics
#' are also often provided, along with the results of models fitting a relationship
#' between the covariates and the outcome.
#'
#' This function builds up a contingency table to summarise a data set, which
#' can later be exported in a publication friendly format.
#'
#' @param independents A named list of independent variables, which will
#'   be distributed down the table's rows. The variables must be specified
#'   by strings, with the item name used as the column header.
#' @param outcomes The variables to cross-tabulate by. These will be
#'   distributed across the table's columns. Specified as a named list of strings.
#'   Must correspond to factor or character variables.
#' @param data The data set that contains the columns specified in
#'   \code{cat_vars} and \code{outcome}.
#' @param frequency Whether to include the counts of each level of \code{cat_vars}.
#' @param row_funcs A list of functions that are applied row-wise to the table,
#'   one independent variable at a time, providing a value for each level of
#'   the factors specified in \code{independents}.
#'   See the vignette for a description of how to specify this.
#'   Two functions: \code{odds_ratio} and \code{hazard_ratio} come
#'   provided with the package.
#'
#' @return An S3 object of class \code{contintab}, that provides the cell contents
#'   as a matrix of strings.
#'
#' @examples
#'
#' # This example uses a dummy data set of whether an individual was treated or not
#' treat_df <- data.frame(age=factor(sample(c("0-20", "21-60", ">61"), 100, replace=TRUE),
#'                                   levels=c('0-20', '21-60', '>61')),
#'                        sex=factor(sample(c("M", "F"), 100, replace=TRUE),
#'                                   levels=c('F', 'M')),
#'                        treated=factor(sample(c("Yes", "No"), 100, replace=TRUE),
#'                                       levels=c('Yes', 'No')))
#'
#'  contingency_table(list("Age at diagnosis"='age', "Sex"='sex'),
#'                    data=treat_df)
#'
#'  contingency_table(list("Age at diagnosis"='age', "Sex"='sex'),
#'                    treat_df,
#'                    outcomes=list('Treated'='treated'))
#'
#'  contingency_table(list("Age at diagnosis"='age', "Sex"='sex'),
#'                    treat_df,
#'                    outcomes=list('Treated'='treated'),
#'                    row_funcs=list("Odds ratio"=odds_ratio('treated')))
#'
#'  contingency_table(list("Age at diagnosis"='age', "Sex"='sex'),
#'                    treat_df,
#'                    outcomes=list('Treated'='treated'),
#'                    row_funcs=list("Odds ratio"=odds_ratio('treated'),
#'                                "Adjusted odds ratio"=odds_ratio('treated', adjusted=TRUE)))
#'
#'
#' @export
contingency_table <- function(independents, data, outcomes=NULL,
                              row_funcs=NULL,
                              col_funcs=NULL,
                              frequency=TRUE) {
    if (length(outcomes) > 2) {
        stop("Having more than 2 outcomes isn't possible.")
    }

    for (cat in independents) {
        if (!is.factor(data[[cat]]) & typeof(data[[cat]]) != 'character') {
            stop("Error: ", cat, " variable isn't a factor or character. Please reencode it as such.")
        }
    }

    raw_content <- lapply(outcomes, function(outcome_val) {
        # Calculate cross-reference freq overall
        overall <- table(data[[outcome_val]])
        overall_props <- overall / nrow(data)

        content <- lapply(independents, function(var) {
            # Calculate table frequencies overall
            counts <- table(data[[var]])
            # Calculate 2x2 table frequencies with proportions
            cross_counts <- table(data[[var]], data[[outcome_val]])
            cross_props <- apply(cross_counts, 2, "/", counts)

            list(cross_counts=cross_counts,
                 cross_proportion=cross_props)
        })
        list(overall=overall, overall_props=overall_props,
             dependent_freqs=content)
    })

    cat_counts <- lapply(independents, function(var) {
        table(data[[var]])
    })

    row_func_vals <- lapply(independents, function(var) {
        lapply(row_funcs, function(x) x(var, independents, data))
    })

    col_func_vals <- lapply(col_funcs, function(func) {
        lapply(outcomes, function(out) {
            func(out, data)
        })
    })

    if (!is.null(outcomes)) {
        outcome_levels <- lapply(outcomes, function(var) levels(data[[var]]))
    } else {
        outcome_levels <- list(c())
    }

    # Calculate regression coefficients
    raw_obj <- list(content=raw_content,
                    row_func_labels=names(row_funcs),
                    row_func_vals=row_func_vals,
                    col_func_labels=names(col_funcs),
                    col_func_vals=col_func_vals,
                    cat_vars=independents,
                    cat_counts=cat_counts,
                    outcomes=outcomes,
                    cat_levels=lapply(independents, function(var) levels(data[[var]])),
                    outcome_levels=outcome_levels,
                    frequency=frequency,
                    has_outcome=!is.null(outcomes),
                    num_obs=nrow(data),
                    num_headers=1 + as.numeric(!is.null(outcomes)))

    mat <- convert_list_to_matrix(raw_obj)
    raw_obj$mat <- mat
    class(raw_obj) <- c('contintab', class(raw_obj))
    raw_obj
}


#' Converts a table with summary values saved as a list to a matrix.
#'
#' @keywords internal
#'
#' @param list Input list
#'
#' @return A string matrix containing the content of each cell
#'
#' Internal helper function
#'
convert_list_to_matrix <- function(x) {

    row_funcs <- x$row_func_labels
    nrowfuncs <- if (is.null(row_funcs)) 0 else length(row_funcs)

    col_funcs <- x$col_func_labels
    ncolfuncs <- if (is.null(col_funcs)) 0 else length(col_funcs)

    num_cross_levels <- sum(sapply(x$outcome_levels, length))

    # Setup empty matrix to hold the table
    ncols <- 2 + as.numeric(x$frequency) + num_cross_levels + nrowfuncs
    nrows <- sum(sapply(x$cat_levels, function(var) length(var)+1)) + x$num_headers + 2 * ncolfuncs
    if (x$frequency | x$has_outcome) {
        nrows <- nrows + 2
    }

    tab <- matrix("", nrow=nrows, ncol=ncols)

    # Row to hold model column names and 'All'
    header_row <- x$num_headers

    # Add first row
    if (x$frequency) {
        tab[header_row, 3] <- 'All'
        col_num <- 4
    } else {
        col_num <- 3
    }

    for (outcome in names(x$outcome_levels)) {
        tab[1, col_num] <- outcome
        col_num <- col_num + length(x$outcome_levels[[outcome]])
    }

    if (nrowfuncs > 0) {
        for (i in 1:nrowfuncs) {
            tab[header_row, col_num - 1 + i] <- row_funcs[i]
        }
    }

    # First content row is the outcome variable levels.
    col_num <- 3 + as.numeric(x$frequency)
    for (outcome in x$outcome_levels) {
        for (var in outcome) {
            tab[2, col_num] <- var
            col_num <- col_num + 1
        }
    }

    curr_row_num <- header_row + 1

    if (x$has_outcome | x$frequency) {
        curr_row_num <- curr_row_num + 1
        # First row is overall counts if required
        tab[curr_row_num, 2] <- "Total"
        if (x$frequency) {
            tab[curr_row_num, 3] <- x$num_obs
            col_num <- 4
        } else {
            col_num <- 3
        }

        for (outcome in x$content) {
            for (i in seq_along(outcome$overall)) {
                tab[curr_row_num, col_num] <- paste0(outcome$overall[i], " (", round(outcome$overall_props[i], 2), ")")
                col_num <- col_num + 1
            }
        }

        curr_row_num <- curr_row_num + 1
    }

    # Then add the content split by variable
    for (var in names(x$cat_vars)) {
        var_start_row <- curr_row_num + 1  # Add a blank line between variables
        var_levels <- x$cat_levels[[var]]

        # Display categorical variable and level names
        for (i in seq_along(var_levels)) {
            if (i == 1) {
                tab[var_start_row+i-1, 1] <- var
            }
            tab[var_start_row+i-1, 2] <- var_levels[i]  # name of level
            if (x$frequency)
                tab[var_start_row+i-1, 3] <- x$cat_counts[[var]][i] # overall count
        }
        if (x$frequency) {
            starting_crosstab_col <- 4
        } else {
            starting_crosstab_col <- 3
        }

        # Display cross-tab
        for (i in seq_along(x$cat_levels[[var]])) {
            col_num <- starting_crosstab_col
            for (outcome in names(x$content)) {
            varcont <- x$content[[outcome]]$dependent_freqs[[var]]
                for (j in seq_along(x$outcome_levels[[outcome]])) { # add count per level of cross-ref var (with proportion in brackets)
                    tab[var_start_row+i-1, col_num] <- paste0(varcont$cross_counts[i, j], " (", round(varcont$cross_proportion[i, j], 2), ")")
                    col_num <- col_num + 1
                }
            }
        }

        # Add function vars
        func_starting_col <- starting_crosstab_col + num_cross_levels
        for (i in seq_along(x$cat_levels[[var]])) {
            for (j in seq_along(row_funcs)) {
                tab[var_start_row+i-1, func_starting_col+j-1] <- x$row_func_vals[[var]][[j]][i]
            }
        }
        curr_row_num <- var_start_row + length(x$cat_levels[[var]])
    }

    # Now add col functions
    for (i in seq_along(x$col_func_vals)) {
        curr_row_num <- curr_row_num + 1
        tab[curr_row_num, 1] <- col_funcs[i]
        outcomes <- x$col_func_vals[[i]]
        if (x$frequency) {
            start_col_num <- 4
        } else {
            start_col_num <- 3
        }

        col_num <- start_col_num

        for (outcome in outcomes) {
            for (val in outcome) {
                tab[curr_row_num, col_num] <- val
                col_num <- col_num + 1
            }
        }
        curr_row_num <- curr_row_num + 1
    }
    tab
}