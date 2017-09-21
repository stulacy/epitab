crosstable <- function(cat_vars, cross_refs, data, functions=NULL) {
    if (length(cross_refs) == 2) {
        stop("Having 2 cross refs isn't currently supported.")
    } else if (length(cross_refs) > 2) {
        stop("Having more than 2 cross refs isn't possible.")
    } else if (length(cross_refs) == 0) {
        stop("Must specify at least one cross-reference!")
    }

    cross_ref <- cross_refs[1]

    # Calculate cross-reference freq overall
    overall <- table(data[[cross_ref]])
    overall_props <- overall / nrow(data)

    content <- setNames(lapply(cat_vars, function(var) {
        # Calculate table frequencies overall
        counts <- table(data[[var]])

        # Calculate 2x2 table frequencies with proportions
        cross_counts <- table(data[[var]], data[[cross_ref]])
        cross_props <- apply(cross_counts, 2, "/", counts)

        # Apply function
        func_vals <- lapply(functions, function(x) x(var, data))

        list(counts=counts,
             cross_counts=cross_counts,
             cross_proportion=cross_props,
             function_vals=func_vals)
    }), cat_vars)

    obj <- list(content=content,
                overall_counts=overall,
                overall_proportion=overall_props,
                ncrossrefs=length(cross_refs),
                cat_vars=cat_vars,
                cross_refs=cross_refs,
                funcs=names(functions))
    class(obj) <- c('crosstab', class(obj))
    obj
}


