#' Builds a function used to calculate odds ratios.
#'
#' Builds a function to calculate logistic regression on the
#' outcome variable of interest with a single covariate and
#' returns a list of the odds ratios for each level of the factor.
#'
#' @keywords internal
#'
#' @param var The dependent variable
#' @param data The data frame.
#'
#' @return A vector of strings with the hazard ratios.
#'
odds_ratio <- function(outcome) {
    function(var, data) {
        # Set baseline as largest group
        orig_levels <- levels(data[[var]])
        max_group <- levels(data[[var]])[which.max(table(data[[var]]))]
        data[[var]] <- stats::relevel(data[[var]], ref=max_group)

        form <- stats::as.formula(paste(outcome, "~", var))
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