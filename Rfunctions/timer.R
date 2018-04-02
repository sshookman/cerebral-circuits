timer <- function(func, active=TRUE, ...) {
    if (active) {
        start <- Sys.time()
    }

    response <- func(...)

    if (active) {
        dtime <- difftime(Sys.time(), start)
        print(paste("Executed in", as.double(dtime), units(dtime)))
    }

    return(response)
}
