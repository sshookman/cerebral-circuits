library("data.table")
library("rpart")

feature_importance <- function(data, dv, features, file=NULL) {
    popped <- c()
    master_vi = data.table()
    master_ap = data.table()
    
    while(length(features) > 1) {
        fitData <- data[, paste(c(dv, features)), with=FALSE]
        formula <- paste(dv, "~", paste(features, collapse=" + "))
        fit <- rpart(as.formula(formula), method="anova", data=fitData)
        
        vi <- fit$variable.importance
        ap <- importance_rpart(fit)
        master_vi <- aggregate_fi(master_vi, vi, popped)
        master_ap <- aggregate_fi(master_ap, ap, popped)
        
        popped <- append(popped, features[1])
        features <- features[-1]
    }
    
    if(!is.null(file)) {
        write.csv(master_vi, file=paste(file, "vi.csv", sep="_"), row.names=FALSE)
        write.csv(master_ap, file=paste(file, "ap.csv", sep="_"), row.names=FALSE)
    }
    
    master_vi
}

aggregate_fi <- function(master_table, table, columns) {
    table <- round((table / sum(table)) * 100, 2)
    table <- setDT(data.frame(as.list(table)))

    if (length(columns) > 0) {
        table[, columns] <- NA            
    }
    
    master_table <- rbind(master_table, table)
    master_table
}

importance_rpart <- function(fit) {
  if(is.null(fit)) {
    stop('Please provide correct rpart model object, returning NULL')
  }
  if(class(fit) != 'rpart'){
    stop('Please provide correct rpart model object, returning NULL')
  }
  ff <- fit$frame
  fpri <- which(ff$var != "<leaf>")  # points to primary splits in ff
  spri <- 1 + cumsum(c(0, 1 + ff$ncompete[fpri] + ff$nsurrogate[fpri]))
  spri <- spri[seq_along(fpri)] # points to primaries in the splits matrix
  nsurr <- ff$nsurrogate[fpri]  # number of surrogates each has
  sname <- vector("list", length(fpri))
  sval <- sname
    ## The importance for primary splits needs to be scaled
    ## It was a printout choice for the anova method to list % improvement in
    ##  the sum of squares, an importance calculation needs the total SS.
    ## All the other methods report an unscaled change.
  scaled.imp <- if (fit$method == "anova")
  fit$splits[spri, "improve"] * ff$dev[fpri]
  else fit$splits[spri, "improve"]

  sdim <- rownames(fit$splits)
  for (i in seq_along(fpri)) {
    ## points to surrogates
    if (nsurr[i] > 0L) {
      indx <- spri[i] + ff$ncompete[fpri[i]] + seq_len(nsurr[i])
      sname[[i]] <- sdim[indx]
      sval[[i]] <- scaled.imp[i] * fit$splits[indx, "adj"]
    }
  }

    import <- tapply(c(scaled.imp, unlist(sval)),
                     c(as.character(ff$var[fpri]), unlist(sname)),
                     sum)
    sort(c(import), decreasing = TRUE) # a named vector
}
