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
#' @param cat_vars A named list of independent variables, where the names are
#'   used as the column headers. The variables must be specified by strings.
#' @param strata The variables to cross-tabulate by, provided as a named list of strings.
#'   The specified variables must be factors. Currently only one strata is allowed.
#' @param data The data set that contains the columns specified in
#'   \code{cat_vars} and \code{outcome}.
#' @param frequency Whether to include the counts of each level of \code{cat_vars}.
#' @param models An optional list of functions that apply a model to the
#'   data, providing a value for each level of the factors specified in
#'   \code{cat_vars}. See the vignette for a description of how to specify this.
#'   One function \code{odds_ratio} comes provided with the package.
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
#'                    strata=list('Treated'='treated'))
#'
#'  contingency_table(list("Age at diagnosis"='age', "Sex"='sex'),
#'                    treat_df,
#'                    strata=list('Treated'='treated'),
#'                    models=list("Odds ratio"=odds_ratio()))
#'
#'  contingency_table(list("Age at diagnosis"='age', "Sex"='sex'),
#'                    treat_df,
#'                    strata=list('Treated'='treated'),
#'                    models=list("Odds ratio"=odds_ratio(),
#'                                "Adjusted odds ratio"=odds_ratio(adjusted=TRUE)))
#'
#'
#' @export
contingency_table <- function(cat_vars, data, strata=NULL, models=NULL,
                              frequency=TRUE) {
    if (length(strata) > 2) {
        stop("Having more than 2 strata isn't possible.")
    }

    if (!is.null(strata)) {
        outcome_val <- strata[[1]]
    }

    for (cat in cat_vars) {
        if (!is.factor(data[[cat]]) & typeof(data[[cat]]) != 'character') {
            stop("Error: ", cat, " variable isn't a factor or character. Please reencode it as such.")
        }
    }

    # Calculate cross-reference freq overall
    if (!is.null(strata)) {
        overall <- table(data[[outcome_val]])
        overall_props <- overall / nrow(data)
    } else {
        overall <- NULL
        overall_props <- NULL
    }

    content <- lapply(cat_vars, function(var) {
        # Calculate table frequencies overall
        counts <- table(data[[var]])
        if (!is.null(strata)) {
            # Calculate 2x2 table frequencies with proportions
            cross_counts <- table(data[[var]], data[[outcome_val]])
            cross_props <- apply(cross_counts, 2, "/", counts)
        } else {
            cross_counts <- NULL
            cross_props <- NULL
        }
        # Apply function
        func_vals <- lapply(models, function(x) x(var, cat_vars, outcome_val, data))

        list(counts=counts,
             cross_counts=cross_counts,
             cross_proportion=cross_props,
             function_vals=func_vals,
             levels=levels(data[[var]]))
    })

    raw_obj <- list(content=content,
                    overall_counts=overall,
                    overall_proportion=overall_props,
                    outcome_label=names(strata)[1],
                    funcs=names(models),
                    frequency=frequency,
                    has_outcome=!is.null(strata),
                    num_obs=nrow(data),
                    num_headers=1 + length(strata))

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
    cont <- x$content
    cat_vars <- names(cont)
    funcs <- x$funcs
    nfuncs <- if (is.null(funcs)) 0 else length(funcs)
    num_cross_levels <- length(x$overall_counts)
    if (num_cross_levels > 0) {
        cross_level_labels <- colnames(x$content[[1]]$cross_counts)
    }

    # Setup empty matrix to hold the table
    ncols <- 2 + as.numeric(x$frequency) + num_cross_levels + nfuncs
    nrows <- sum(sapply(cat_vars, function(var) length(cont[[var]]$levels) + 1)) + x$num_headers
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

    if (x$has_outcome) {
        # Outcome labels are always first row
        tab[1, col_num] <- x$outcome_label
        col_num <- col_num + num_cross_levels
    }

    if (nfuncs > 0) {
        for (i in 1:nfuncs) {
            tab[header_row, col_num - 1 + i] <- funcs[i]
        }
    }

    # First content row is the outcome variable levels.
    if (x$has_outcome) {
        for (i in seq_len(num_cross_levels)) {
            tab[2, 2 + as.numeric(x$frequency) + i] <- cross_level_labels[i]
        }
    }

    curr_row_num <- header_row + 1

    if (x$has_outcome | x$frequency) {
        curr_row_num <- curr_row_num + 1
        # First row is overall counts if required
        tab[curr_row_num, 2] <- "Total"
        if (x$frequency) {
            tab[curr_row_num, 3] <- x$num_obs
            col_num <- 3
        } else {
            col_num <- 2
        }

        for (i in seq_len(num_cross_levels)) {
            tab[curr_row_num, col_num+i] <- paste0(x$overall_counts[i], " (", round(x$overall_proportion[i], 2), ")")
        }

        curr_row_num <- curr_row_num + 1
    }

    # Then add the content split by variable
    for (cat_num in seq_along(cat_vars)) {
        curr_row_num <- curr_row_num + 1  # Add a blank line between variables

        var <- cat_vars[cat_num]
        varcont <- cont[[var]]

        num_levs <- length(varcont$levels)
        for (i in 1:num_levs) {
            if (i == 1) {
                tab[curr_row_num, 1] <- var
            }

            tab[curr_row_num, 2] <- varcont$levels[i]  # name of level
            if (x$frequency) {
                tab[curr_row_num, 3] <- varcont$counts[i]         # overall count
                col_num <- 3
            } else {
                col_num <- 2
            }
            for (j in seq_len(num_cross_levels)) {              # add count per level of cross-ref var (with proportion in brackets)
                tab[curr_row_num, col_num + j] <- paste0(varcont$cross_counts[i, j], " (", round(varcont$cross_proportion[i, j], 2), ")")
            }
            # Add function vars
            for (j in seq_along(funcs)) {
                tab[curr_row_num, col_num+num_cross_levels+j] <- varcont$function_vals[[funcs[j]]][i]
            }

            curr_row_num <- curr_row_num + 1
        }
    }
    tab
}