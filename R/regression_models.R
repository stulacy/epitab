#' Builds a function used to calculate hazard ratios.
#'
#' Builds a function to build a Cox model on the
#' outcome survival object, with a single covariate and
#' returns a list of the odds ratios for each level of the factor.
#'
#' @param outcome The dependent variable, specifies a \code{Surv} object
#'   as a string.
#' @param adjusted Whether to adjust for the other covariates, specified by
#'   \code{cat_vars} argument to \code{contingency_table}.
#' @param relevel_baseline Whether to use the largest level as the baseline.
#'
#' @return A function that is used to calculate hazard ratios.
hazard_ratio <- function(outcome, adjusted=FALSE, relevel_baseline=TRUE) {

    build_regression_model(outcome, adjusted, relevel_baseline, extract_coefs=function(formula, data) {
        survival::coxph(formula, data)
    })
}

#' Builds a function used to calculate odds ratios.
#'
#' Builds a function to calculate logistic regression on the
#' outcome variable of interest with a varying number of covariates and
#' returns a list of the odds ratios for each level of the factor.
#'
#' @param outcome The dependent variable, defaults to the first \code{strata}
#'   supplied to \code{contingency_table}.
#' @param adjusted Whether to adjust for the other covariates, specified by
#'   \code{cat_vars} argument to \code{contingency_table}.
#' @param relevel_baseline Whether to use the largest level as the baseline.
#'
#' @return A function that is used to calculate odds ratios.
#'
odds_ratio <- function(outcome=NULL, adjusted=FALSE, relevel_baseline=TRUE) {

    build_regression_model(outcome, adjusted, relevel_baseline, extract_coefs = function(formula, data) {
        stats::glm(formula, data, family=stats::binomial())
    })
}

build_regression_model <- function(outcome, adjusted, relevel_baseline, extract_coefs) {

    function(var, all_vars, first_strata, data) {
        # Used when no covariates are specified, thereby indicating
        # a univariate model
        if (adjusted) {
            covars <- all_vars
        } else {
            covars <- var
        }

        if (is.null(outcome)) {
            outcome <- first_strata
        }

        # Set baseline as largest group
        orig_levels <- levels(data[[var]])
        if (relevel_baseline) {
            baseline <- levels(data[[var]])[which.max(table(data[[var]]))]
            data[[var]] <- stats::relevel(data[[var]], ref=baseline)
        } else {
            baseline <- levels(data[[var]])[1]
        }

        form <- stats::as.formula(paste(outcome, "~", paste(covars, collapse='+')))

        # Extract the required coefficients, in glm they are output as
        # <varname><level>. Also add a dummy coefficient = 1 for baseline
        mod <- extract_coefs(form, data)
        raw_coefs <- exp(stats::coef(mod))
        coefs <- c(raw_coefs, 1)
        baseline_label <- paste0(var, baseline)
        names(coefs)[length(coefs)] <- baseline_label

        # Reorder coefs back into original levels
        coef_labels <- paste0(var, orig_levels)
        modelled_levels <- setdiff(coef_labels, baseline_label)
        estimates <- coefs[coef_labels]

        # Then round
        estimates[modelled_levels] <- sapply(estimates[modelled_levels], function(x) sprintf("%0.2f", x))

        # Now get CIs
        raw_ci <- suppressMessages(exp(stats::confint(mod)))
        # Firstly get the (p-1) CIs
        ci <- raw_ci[modelled_levels, ]
        # If have a single modelled level then a vector gets returned rather than a matrix
        if (is.null(dim(ci))) {
            dim(ci) <- c(1, 2)
            row.names(ci) <- modelled_levels
        }
        ci_str <- apply(ci, 1, function(row) {
            sprintf("(%.2f - %.2f)", row[1], row[2])
        })
        # Add empty val for baseline
        ci_str <- c(ci_str, "")
        names(ci_str)[length(ci_str)] <- baseline_label

        # And combine into formatted string
        paste(estimates, ci_str[coef_labels])
    }
}