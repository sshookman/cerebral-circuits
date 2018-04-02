library("data.table")
library("rpart")

optimal_cp <- function(data, dv, features, target=27, threshold=5, max_iter=10) {
    cp <- length(features) / 2500
    optimal_cp_loop(data, dv, features, target, cp, threshold, max_iter)
}


optimal_cp_loop <- function(data, dv, features, target, cp, threshold, max_iter) {
    
    fit_data <- data[, paste(c(dv, features)), with=FALSE]
    fit <- rpart(as.formula(paste(dv, "~ .")), data=fit_data, cp=cp)
    predicted <- predict(fit, fit_data)
    num_pred <- length(unique(predicted))

    print(num_pred)
    print(cp)

    if (max_iter > 0) {
        if (num_pred < (target - threshold) | num_pred > (target + threshold)) {
            #calc_cp <- (cp - ((target - num_pred) * step))
            calc_cp <- cp - (cp * ((target - num_pred) / target))
            cp <- optimal_cp_loop(data, dv, features, target, calc_cp, threshold, (max_iter - 1))
        }
    }

    return(cp)
}
