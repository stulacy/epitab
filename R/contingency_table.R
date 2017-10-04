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
#' @param models An optional list of functions that apply a model to the
#'   data, providing a value for each level of the factors specified in
#'   \code{cat_vars}. See the vignette for a description of how to specify this.
#'   One function \code{odds_ratio} comes provided with the package.
#' @param cox_outcome A survival object representing the survival outcome of the
#'   Cox model.
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
#'                    outcome=list('Treated'='treated'))
#'
#'  contingency_table(list("Age at diagnosis"='age', "Sex"='sex'),
#'                    treat_df,
#'                    outcome=list('Treated'='treated'),
#'                    models=list("Odds ratio"="odds_ratio"))
#'
#'
#'  contingency_table(list("Age at diagnosis"='age', "Sex"='sex'),
#'                    treat_df,
#'                    outcome=list('Treated'='treated'),
#'                    models=list("Odds ratio"="odds_ratio",
#'                                "Adjusted odds ratio"="adj_odds_ratio"))
#'
#'
#' @export
contingency_table <- function(cat_vars, data, outcome=NULL, models=NULL, cox_outcome=NULL,
                              frequency=TRUE, custom_functions=NULL) {
    if (length(outcome) == 2) {
        stop("Having 2 cross refs isn't currently supported.")
    } else if (length(outcome) > 2) {
        stop("Having more than 2 cross refs isn't possible.")
    }

    if (!is.null(custom_functions)) {
        stop("Error: currently do not have custom functions functionality supported.")
    }

    func_options <- c('odds_ratio', 'adj_odds_ratio', 'hazard_ratio', 'adj_hazard_ratio')

    if (is.null(outcome) & "odds_ratio" %in% models) {
        stop("Error: Cannot calculate odds or adjusted odds ratio when outcome is null.")
    }

    if (!is.null(outcome)) {
        outcome_val <- outcome[[1]]
    }

    for (cat in cat_vars) {
        if (!is.factor(data[[cat]]) & typeof(data[[cat]]) != 'character') {
            stop("Error: ", cat, " variable isn't a factor or character. Please reencode it as such.")
        }
    }

    # TODO Add separate list for custom functions
    full_funcs <- list()
    for (fn in names(models)) {
        f <- models[[fn]]
        if (!f %in% func_options) {
            stop("Error: function type '", f, "' unknown. Options are ", paste(func_options, collapse=', '))
        }

        # Run closure and generate function
        if (f == 'odds_ratio') {
            full_funcs[[fn]] <- build_or(outcome_val)
        } else if (f == "adj_odds_ratio") {
            full_funcs[[fn]] <- build_or(outcome_val, unlist(cat_vars))
        } else if (grepl('hazard', f)) {
            if (is.null(cox_outcome)) {
                stop("Error: Please provide a survival object in 'cox_outcome' when trying to display hazard ratios.")
            }

            if (f == 'hazard_ratio') {
                full_funcs[[fn]] <- build_cox(cox_outcome)

            } else if (f == "adj_hazard_ratio") {
                full_funcs[[fn]] <- build_cox(cox_outcome, unlist(cat_vars))
            }
        }
    }
    for (fn in custom_functions) {
        f <- custom_functions[[fn]]
        if (class(f) == "function") {
            # Check has 2 arguments
            full_funcs[[fn]] <- f
        } else {
            stop("Error: custom functions must be of type function")
        }
    }

    # Calculate cross-reference freq overall
    if (!is.null(outcome)) {
        overall <- table(data[[outcome_val]])
        overall_props <- round((overall / nrow(data)) * 100)
    } else {
        overall <- NULL
        overall_props <- NULL
    }

    content <- lapply(cat_vars, function(var) {
        # Calculate table frequencies overall
        counts <- table(data[[var]])
        if (!is.null(outcome)) {
            # Calculate 2x2 table frequencies with proportions
            cross_counts <- table(data[[var]], data[[outcome_val]])
            cross_props <- apply(cross_counts, 2, "/", counts)
            cross_props <- round(cross_props * 100)
        } else {
            cross_counts <- NULL
            cross_props <- NULL
        }
        # Apply function
        func_vals <- lapply(full_funcs, function(x) x(var, data))

        list(counts=counts,
             cross_counts=cross_counts,
             cross_proportion=cross_props,
             function_vals=func_vals,
             levels=levels(data[[var]]))
    })

    raw_obj <- list(content=content,
                    overall_counts=overall,
                    overall_proportion=overall_props,
                    outcome_label=names(outcome),
                    funcs=names(full_funcs),
                    frequency=frequency,
                    has_outcome=!is.null(outcome),
                    num_obs=nrow(data))

    mat <- convert_list_to_matrix(raw_obj)
    class(mat) <- c('contintab', class(mat))
    mat
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
    nfuncs <- if (is.null(funcs)) 0 else length(funcs)
    num_cross_levels <- length(x$overall_counts)
    if (num_cross_levels > 0) {
        cross_level_labels <- colnames(x$content[[1]]$cross_counts)
    }

    # Setup empty matrix to hold the table
    ncols <- 2 + as.numeric(x$frequency) + num_cross_levels + nfuncs
    nrows <- 2 + sum(sapply(cat_vars, function(var) length(cont[[var]]$levels) + 1))
    tab <- matrix("", nrow=nrows, ncol=ncols)

    # Add first row
    header <- character(ncols)
    if (x$frequency) {
        header[3] <- 'All'
        col_num <- 4
    } else {
        col_num <- 3
    }

    if (x$has_outcome) {
        header[col_num] <- x$outcome_label
        col_num <- col_num + 1
    }

    if (nfuncs > 0) {
        for (i in 1:nfuncs) {
            header[col_num - 2 + num_cross_levels + i] <- funcs[i]
        }
    }

    colnames(tab) <- header

    # First content row is the outcome variable levels
    for (i in seq_len(num_cross_levels)) {
        tab[1, 3+i] <- cross_level_labels[i]
    }

    # Followed by the overall counts
    if (x$frequency) {
        tab[2, 2] <- "Total"
        tab[2, 3] <- x$num_obs
        col_num <- 3
    } else {
        col_num <- 2
    }

    for (i in seq_len(num_cross_levels)) {
        tab[2, col_num+i] <- paste0(x$overall_counts[i], " (", round(x$overall_proportion[i], 2), ")")
    }

    curr_row_num <- 3

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