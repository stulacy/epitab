foo_df <- data.frame(age=factor(sample(c("0-20", "21-60", ">61"), 100, replace=T), levels=c('0-20', '21-60', '>61')),
                     sex=factor(sample(c("M", "F"), 100, replace=T), levels=c('F', 'M')),
                     treated=factor(sample(c("Yes", "No"), 100, replace=T), levels=c('Yes', 'No')))

obj <- crosstable(c('age', 'sex'), 'treated', foo_df, list("Odds ratio"=odds_ratio))

