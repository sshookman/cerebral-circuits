# Get matrices for correlation and covariance
corcov <- function(dataset) {
    nums <- sapply(dataset, is.numeric)
    nums <- dataset[, nums, with=FALSE]
    round(cor(nums, use = "pairwise.complete.obs", method = "spearman"), digits = 2)
    round(cov(nums, use = "complete.obs", method = "spearman"), digits = 2)
}

