xgbExplainer <- function(model, train, type, baseScore) {
    library(xgboostExplainer)

    explainer = buildExplainer(model, train, type=type, base_score=baseScore)
    return(explainer)
}
