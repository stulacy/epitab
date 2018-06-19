#' Builds a contingency table
#'
#' A contingency table provides cross-tabulated frequencies between an outcome
#' of interest and one or more independent variables. This function extends
#' contingency tables to include summary statistics formed both column-wise
#' and row-wise, looking at outcomes and covariates respectively in isolation.
#' This allows for a large amount of flexibility and tables can be drawn for
#' a variety of situations. By default, the \code{print} method fits these
#' tables to standard R console output, but publication quality tables
#' can be produced using the \code{neat_table} function. See the vignette
#' for further guidance.
#'
#' @param independents A named list of independent variables, which will
#'   be distributed down the table's rows. The variables must be specified
#'   by strings, with the item name used as the column header.
#' @param outcomes The variables to cross-tabulate by. These will be
#'   distributed across the table's columns. Specified as a named list of strings.
#'   Must correspond to factor or character variables.
#' @param data The data set that contains the columns specified in
#'   \code{cat_vars} and \code{outcome}.
#' @param crosstab_funcs A list of functions that are applied to every cross-tabulation
#'   permutation of \code{independents} and \code{outcomes}. The most common
#'   function, the frequency, is provided with the package in function \code{freq}.
#'   See the vignette for further guidance.
#' @param row_funcs A list of functions that are applied row-wise to the table,
#'   one independent variable at a time, providing a value for each level of
#'   the factors specified in \code{independents}.
#'   Two functions: \code{odds_ratio} and \code{hazard_ratio} come
#'   provided with the package.
#'   See the vignette for further guidance.
#' @param col_funcs A list of functions that are applied column-wise to the table,
#'   to every outcome separate from the independent variables.
#'   Examples provided with the package included \code{summary_mean} and
#'   \code{summary_median}, which calculate the mean and median value of a
#'   specified continuous variable for each level of the outcome.
#'   See the vignette for further guidance.
#' @param marginal Whether to include the counts of each level of \code{cat_vars}, the marginal frequency.
#'
#' @return An S3 object of class \code{contintab}, that provides the cell contents
#'   as a matrix of strings.
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
#' # Displays a standard contingency table
#' contingency_table(list("Age"='agebin', "Sex"='sex'),
#'                   outcomes=list('Treated'='treated'),
#'                   crosstab_funcs=list(freq()),
#'                   data=treat)
#'
#' # Continuous variables can be summarised with respect to the outcome
#' # by using col_funcs
#' contingency_table(list("Age"='agebin', "Sex"='sex'),
#'                   outcomes=list('Treated'='treated'),
#'                   crosstab_funcs=list(freq()),
#'                   col_funcs=list("Mean age"=summary_mean('age')),
#'                   data=treat)
#'
#' # Regression coefficients can be added using row_funcs
#' contingency_table(list("Age"='agebin', "Sex"='sex'),
#'                    treat,
#'                    outcomes=list('Treated'='treated'),
#'                    crosstab_funcs=list(freq()),
#'                    row_funcs=list("Odds ratio"=odds_ratio('treated'),
#'                                   "Adjusted odds ratio"=odds_ratio('treated', adjusted=TRUE)))
#'
#' @export
contingency_table <- function(independents, data, outcomes=NULL,
                              crosstab_funcs=NULL,
                              row_funcs=NULL,
                              col_funcs=NULL,
                              marginal=TRUE) {

    for (cat in independents) {
        if (!is.factor(data[[cat]]) & typeof(data[[cat]]) != 'character') {
            stop("Error: ", cat, " variable isn't a factor or character. Please reencode it as such.")
        }
    }

    if (xor(is.null(crosstab_funcs), is.null(outcomes))) {
        stop("if one of crosstab_funcs or outcomes is provided, then the other must be as well.")
    }

    raw_content <- lapply(outcomes, function(outcome_val) {
        # Apply cross-tab functions to column totals
        overall_crosstab <- lapply(levels(data[[outcome_val]]), function(lev) {
            sapply(crosstab_funcs, function(func) {
                func(data, outcome_level=lev, outcome_name=outcome_val)
            })
        })

        # Apply cross-tab functions to every combination of outcome and independent
        crosstabs <- lapply(independents, function(ind_var) {
            lapply(levels(data[[ind_var]]), function(ind_lev) {
                lapply(levels(data[[outcome_val]]), function(out_lev) {
                    sapply(crosstab_funcs, function(func) {
                        func(data, out_lev, outcome_val, ind_lev, ind_var)
                    })
                })
            })
        })
        list(overall_crosstab=overall_crosstab, independent_crosstab=crosstabs)
    })

    cat_counts <- lapply(independents, function(ind_var) {
            lapply(levels(data[[ind_var]]), function(ind_lev) {
                    sapply(crosstab_funcs, function(func) {
                        func(data, outcome_level=NULL, outcome_name=NULL,
                             independent_level=ind_lev, independent_name=ind_var)
                    })
            })
    })

    row_func_vals <- lapply(independents, function(var) {
        lapply(row_funcs, function(x) x(data, var, independents))
    })
    # Add on marginal values
    row_func_vals <- c(row_func_vals, list('Marginal'=lapply(row_funcs, function(x) x(data))))

    col_func_vals <- lapply(col_funcs, function(func) {
        c(list('Marginal'=func(data)),
          lapply(outcomes, function(out) {
            lapply(levels(data[[out]]), function(lev) {
                func(data, lev, out)
            })
        }))
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
                    crosstab_funcs=crosstab_funcs,
                    cat_vars=independents,
                    cat_counts=cat_counts,
                    outcomes=outcomes,
                    cat_levels=lapply(independents, function(var) levels(data[[var]])),
                    outcome_levels=outcome_levels,
                    frequency=!is.null(crosstab_funcs) && marginal,
                    has_outcome=!is.null(outcomes),
                    num_obs=nrow(data),
                    num_headers=1 + as.numeric(!is.null(outcomes)))

    mat <- convert_list_to_matrix(raw_obj)
    raw_obj$mat <- mat
    class(raw_obj) <- c('contintab', class(raw_obj))
    raw_obj
}


# Converts a table with summary values saved as a list to a matrix.
#
# list Input list
#
# return A string matrix containing the content of each cell
#
# Internal helper function
#
convert_list_to_matrix <- function(x) {

    row_funcs <- x$row_func_labels
    nrowfuncs <- if (is.null(row_funcs)) 0 else length(row_funcs)

    col_funcs <- x$col_func_labels
    ncolfuncs <- if (is.null(col_funcs)) 0 else length(col_funcs)

    crosstab_funcs <- x$crosstab_funcs
    ncrosstabfuncs <- if(is.null(crosstab_funcs)) 0 else length(crosstab_funcs)

    num_cross_levels <- sum(sapply(x$outcome_levels, length)) * ncrosstabfuncs

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

    # Outcome variable labels
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

        # Overall cross-tabulation values
        for (outcome in x$content) {
            for (val in outcome$overall_crosstab) {
                tab[curr_row_num, col_num] <- val
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
                tab[var_start_row+i-1, 3] <- x$cat_counts[[var]][[i]] # overall count
        }
        if (x$frequency) {
            starting_crosstab_col <- 4
        } else {
            starting_crosstab_col <- 3
        }

        # Display cross-tab
        for (i in seq_along(x$cat_levels[[var]])) {
            col_num <- starting_crosstab_col
            for (outcome in x$content) {
                varcont <- outcome$independent_crosstab[[var]][[i]]
                for (j in seq_along(varcont)) {
                    tab[var_start_row+i-1, col_num] <- varcont[[j]]
                    col_num <- col_num + 1
                }
            }
        }

        # Add row function values
        func_starting_col <- starting_crosstab_col + num_cross_levels

        for (j in seq_along(row_funcs)) {
            tab[4, func_starting_col+j-1] <- x$row_func_vals[['Marginal']][[j]]
            for (i in seq_along(x$cat_levels[[var]])) {
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
        start_col_num <- 3
        if (x$frequency) {
            tab[curr_row_num, start_col_num] <- outcomes[['Marginal']]
            start_col_num <- start_col_num + 1
        }

        col_num <- start_col_num

        for (outcome in outcomes[setdiff(names(outcomes), 'Marginal')]) {
            for (val in outcome) {
                tab[curr_row_num, col_num] <- val
                col_num <- col_num + 1
            }
        }
        curr_row_num <- curr_row_num + 1
    }
    tab
}