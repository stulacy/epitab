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
#' @param outcome The outcome variable of interest, provided as a named list.
#'   Must be a factor. The outcome must be specified as a string.
#' @param data The data set that contains the columns specified in
#'   \code{cat_vars} and \code{outcome}.
#' @param functions An optional list of functions that apply a model to the
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
#'  #tab <- contingency_table(list("Age at diagnosis"='age', "Sex"='sex'),
#'  #                        list('Treated'='treated'),
#'  #                        treat_df,
#'  #                        list("Odds ratio"="odds_ratio"))
#'  #tab
#'
#' @export
contingency_table <- function(cat_vars, outcome, data, functions=NULL) {
    if (length(outcome) == 2) {
        stop("Having 2 cross refs isn't currently supported.")
    } else if (length(outcome) > 2) {
        stop("Having more than 2 cross refs isn't possible.")
    } else if (length(outcome) == 0) {
        stop("Must specify at least one cross-reference!")
    }

    outcome_val <- outcome[[1]]

    full_funcs <- list()
    for (fn in names(functions)) {
        f <- functions[[fn]]
        if (class(f) == "function") {
            # Check has 2 arguments
            full_funcs[[fn]] <- f
        } else if (class(f) == "character") {
            if (!f %in% c('odds_ratio', 'adj_odds_ratio')) {
                stop("Error: function type '", f, "' unknown. Options are 'odds_ratio' and 'adj_odds_ratio'")
            }

            # Run closure and generate function
            if (f == 'odds_ratio') {
                full_funcs[[fn]] <- odds_ratio(outcome_val)
            }
        } else {
            stop("List entries in argument 'functions' must either be functions or strings.")
        }
    }

    # Calculate cross-reference freq overall
    overall <- table(data[[outcome_val]])
    overall_props <- overall / nrow(data)

    content <- lapply(cat_vars, function(var) {
        # Calculate table frequencies overall
        counts <- table(data[[var]])

        # Calculate 2x2 table frequencies with proportions
        cross_counts <- table(data[[var]], data[[outcome_val]])
        cross_props <- apply(cross_counts, 2, "/", counts)

        # Apply function
        func_vals <- lapply(full_funcs, function(x) x(var, data))

        list(counts=counts,
             cross_counts=cross_counts,
             cross_proportion=cross_props,
             function_vals=func_vals)
    })

    raw_obj <- list(content=content,
                    overall_counts=overall,
                    overall_proportion=overall_props,
                    outcome_label=names(outcome),
                    funcs=names(full_funcs))

    mat <- convert_list_to_matrix(raw_obj)

    obj <- list(content=mat,
                overall_counts=overall,
                overall_proportion=overall_props,
                cat_vars=unlist(cat_vars),
                outcome=outcome_val,
                noutcomes=length(outcome),
                funcs=names(full_funcs)
                )
    class(obj) <- c('contintab', class(obj))
    obj
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
    spaces <- function(n) paste(rep(" ", n), collapse='')
    cont <- x$content
    cat_vars <- names(cont)
    funcs <- x$funcs
    nfuncs <- length(funcs)
    num_cross_levels <- length(x$overall_counts)
    cross_level_labels <- colnames(x$content[[1]]$cross_counts)

    # Setup empty matrix to hold the table
    ncols <- 3 + num_cross_levels + nfuncs
    nrows <- 3 + sum(sapply(cat_vars, function(var) length(cont[[var]]$counts) + 1))
    tab <- matrix("", nrow=nrows, ncol=ncols)

    # Add first row
    tab[1, 3] <- 'All'
    tab[1, 4] <- x$outcome_label
    for (i in 1:nfuncs) {
        tab[1, 3 + num_cross_levels + i] <- funcs[i]
    }

    # First content row is the cross reference variable levels
    for (i in 1:num_cross_levels) {
        tab[2, 3+i] <- cross_level_labels[i]
    }

    # Followed by the overall counts
    tab[3, 2] <- "Total"
    tab[3, 3] <- sum(x$overall_counts)
    for (i in 1:num_cross_levels) {
        tab[3, 3+i] <- paste0(x$overall_counts[i], " (", round(x$overall_proportion[i], 2), ")")
    }

    curr_row_num <- 4

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