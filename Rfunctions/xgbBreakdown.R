xgbBreakdown <- function(model, explainer, data, coef) {
    library(data.table)
    library(xgboostExplainer)

    if (class(data) == "data.table" | class(data) == "data.frame") {
        data <- xgb.DMatrix(data.matrix(data), missing=NA)
    }

    breakdown <- explainPredictions(model, explainer, data)
    setDT(breakdown)
    breakdown <- breakdown[, lapply(.SD, function(col) { return(col * coef) })]
    return(breakdown)
}
