library(data.table)
library(rpart)

feature_importance <- function(data, dv, features, popped=c(), master_vi=data.table(), all_splits=FALSE) {

    fitData <- data[, paste(c(dv, features)), with=FALSE]
    formula <- paste(dv, "~ .")
    fit <- rpart(as.formula(formula), method="anova", data=fitData, cp=0.005)

    vi <- fit$variable.importance
    vi <- round((vi / sum(vi)) * 100, 2)
    vi_dt <- setDT(data.frame(as.list(vi)))

    if (length(popped) > 0) {
        vi_dt[, popped] <- NA
    }

    master_vi <- rbind(master_vi, vi_dt)

    abv_avg <- vi > (100/length(vi))
    popping <- names(vi[abv_avg]) # Pop features above average
    features <- names(vi[!abv_avg]) # Keep features below (or equal to) average

    if (length(features) > 1 & length(popping) > 0) {
        master_vi <- feature_importance(data, dv, features, popped=append(popped, popping), master_vi=master_vi, all_splits=all_splits)
    }

    if (all_splits == TRUE & length(popping) > 1 & length(features) > 0) {
        master_vi <- feature_importance(data, dv, popping, popped=append(popped, features), master_vi=master_vi, all_splits=all_splits)
    }

    master_vi
}
