#' Builds a function used to calculate odds ratios.
#'
#' Builds a function to calculate logistic regression on the
#' outcome variable of interest with a varying number of covariates and
#' returns a list of the odds ratios for each level of the factor.
#'
#' @keywords internal
#'
#' @param var The dependent variable
#' @param data The data frame.
#'
#' @return A vector of strings with the hazard ratios.
#'
build_or <- function(outcome, covars=NULL) {

    function(var, data) {
        # Used when no covariates are specified, thereby indicating
        # a univariate model
        if (is.null(covars)) {
            covars <- var
        }
        # Set baseline as largest group
        orig_levels <- levels(data[[var]])
        max_group <- levels(data[[var]])[which.max(table(data[[var]]))]
        data[[var]] <- stats::relevel(data[[var]], ref=max_group)

        form <- stats::as.formula(paste(outcome, "~", paste(covars, collapse='+')))
        mod <- stats::glm(form, data, family=stats::binomial())

        # Extract the required coefficients, in glm they are output as
        # <varname><level>. Also add a dummy coefficient = 1 for baseline
        coefs <- c(exp(stats::coef(mod)[-1]), 1)
        names(coefs)[length(coefs)] <- paste0(var, max_group)

        # Reorder coefs back into original levels
        coef_labels <- paste0(var, orig_levels)
        round(coefs[coef_labels], 2)
    }
}

#' Builds a function used to calculate hazard ratios.
#'
#' Builds a function to build a Cox model on the
#' outcome survival object, with a single covariate and
#' returns a list of the odds ratios for each level of the factor.
#'
#' @keywords internal
#'
#' @param var The dependent variable
#' @param data The data frame.
#'
#' @return A vector of strings with the hazard ratios.
#'
build_cox <- function(outcome, covars=NULL) {

    function(var, data) {
        # Used when no covariates are specified, thereby indicating
        # a univariate model
        if (is.null(covars)) {
            covars <- var
        }

        # Set baseline as largest group
        orig_levels <- levels(data[[var]])
        max_group <- levels(data[[var]])[which.max(table(data[[var]]))]
        data[[var]] <- stats::relevel(data[[var]], ref=max_group)

        form <- stats::as.formula(paste(outcome, "~", paste(covars, collapse='+')))
        mod <- survival::coxph(form, data)

        # Extract the required coefficients, in glm they are output as
        # <varname><level>. Also add a dummy coefficient = 1 for baseline
        coefs <- c(exp(stats::coef(mod)), 1)
        names(coefs)[length(coefs)] <- paste0(var, max_group)

        # Reorder coefs back into original levels
        coef_labels <- paste0(var, orig_levels)
        round(coefs[coef_labels], 2)
    }
}