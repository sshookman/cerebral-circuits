# Generates a Quantile Graph
quantile_graph <- function(df, column, dspName = column, filename = NULL) {
    quant <- quantile(df[[column]], seq(0, 1, by=.01))
    
    if (is.null(filename)) {
        plot(quant, type = 'l', xlab = "Quantile", ylab = dspName, main = paste("Quantile for", dspName))
    } else {
        png(filename = paste(filename, ".png", sep = ""))
        plot(quant, type = 'l', xlab = "Quantile", ylab = dspName, main = paste("Quantile for", dspName))
        dev.off()
    }
    
    quant
}

