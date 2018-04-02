xgbImportance <- function(model, col_names, file) {
    library(xgboost)

    imp = xgb.importance(col_names, model)
    png(file)
    xgb.plot.importance(imp)
    dev.off()
}
