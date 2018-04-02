nullToZero <- function(data, features) {
    data[is.na(get(features)) | is.null(get(features)), features] <- 0
    return(data)
}
