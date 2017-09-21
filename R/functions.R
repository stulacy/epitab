odds_ratio <- function(var, data) {
    # Set baseline as largest group
    orig_levels <- levels(data[[var]])
    max_group <- levels(data[[var]])[which.max(table(data[[var]]))]
    data[[var]] <- relevel(data[[var]], ref=max_group)

    form <- as.formula(paste("treated ~", var))
    mod <- glm(form, data, family=binomial())

    # Extract the required coefficients, in glm they are output as
    # <varname><level>. Also add a dummy coefficient = 1 for baseline
    coefs <- c(exp(coef(mod)[-1]), 1)
    names(coefs)[length(coefs)] <- paste0(var, max_group)

    # Reorder coefs back into original levels
    coef_labels <- paste0(var, orig_levels)
    coefs[coef_labels]
}
