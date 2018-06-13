#' @importFrom stats confint
#' @importFrom xml2 as_list
build_regression_model <- function(outcome, adjusted, relevel_baseline, extract_coefs, digits=2, ci=TRUE) {

    function(data, var=NULL, all_vars=NULL) {
        if (is.null(var) || is.null(all_vars)) {
            return("")
        }

        # Used when no covariates are specified, thereby indicating
        # a univariate model
        if (adjusted) {
            covars <- all_vars
        } else {
            covars <- var
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
        estimates[modelled_levels] <- sapply(estimates[modelled_levels], function(x) sprintf("%.*f", digits, x))

        # Now get CIs
        if (ci) {
            raw_ci <- suppressMessages(exp(confint(mod)))
            # Firstly get the (p-1) CIs
            ci <- raw_ci[modelled_levels, ]
            # If have a single modelled level then a vector gets returned rather than a matrix
            if (is.null(dim(ci))) {
                dim(ci) <- c(1, 2)
                row.names(ci) <- modelled_levels
            }
            ci_str <- apply(ci, 1, function(row) {
                sprintf("(%.*f - %.*f)", digits, row[1], digits, row[2])
            })
            # Add empty val for baseline
            ci_str <- c(ci_str, "")
            names(ci_str)[length(ci_str)] <- baseline_label

            # And combine into formatted string
            output <- paste(estimates, ci_str[coef_labels])
        } else {
            output <- estimates
        }
        output
    }
}

#' Builds a function used to calculate odds ratios.
#'
#' Builds a function to run logistic regression on the
#' outcome variable of interest and return the odds ratios
#' for each covariate level.
#'
#' See documentation for \code{contingency_table} and vignette
#' for usage.
#'
#' @param outcome The dependent variable as a string.
#' @param adjusted Whether to adjust for the other covariates, specified by
#'   \code{independents} argument to \code{contingency_table}.
#' @param relevel_baseline Whether to use the largest level as the baseline.
#' @param digits The number of digits to display.
#' @param ci Whether to include a confidence interval in parentheses after the estimate.
#'
#' @return A function that is used to calculate odds ratios.
#'
#' @export
#' @import MASS
odds_ratio <- function(outcome, adjusted=FALSE,
                       relevel_baseline=FALSE, digits=2, ci=TRUE) {

    build_regression_model(outcome, adjusted, relevel_baseline, function(formula, data) {
        stats::glm(formula, data, family=stats::binomial())
    }, digits=digits, ci=ci)
}

#' Builds a function used to calculate hazard ratios.
#'
#' Builds a function to fit a Cox model to the
#' outcome survival object and returns the hazard ratios for
#' each covariate level.
#'
#' See documentation for \code{contingency_table} and vignette
#' for usage.
#'
#' @inheritParams odds_ratio
#' @param outcome The dependent variable, specifies a \code{Surv} object
#'   as a string. For example, \code{hazard_ratio("Surv(time, status)")}.
#'
#' @return A function that is used to calculate hazard ratios.
#'
#' @export
hazard_ratio <- function(outcome, adjusted=FALSE, relevel_baseline=FALSE, digits=2, ci=TRUE) {

    build_regression_model(outcome, adjusted, relevel_baseline, function(formula, data) {
        survival::coxph(formula, data)
    }, digits=digits, ci=ci)
}


