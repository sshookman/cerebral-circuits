# Plot graphs from make down to mmyt
mmyt_hists <- function(df, make, model, year, trim) {
    df_m    <- df[df$MAKE_NAME == make,]
    df_mm   <- df_m[df_m$MODEL_NAME == model,]
    df_mmy  <- df_mm[df_mm$MODEL_YEAR == year,]
    df_mmyt <- df_mmy[df_mmy$TRIM_NAME == trim,]
    
    hist(df_m$PRICE,    main=paste("Histogram of", make, "Prices"))
    hist(df_mm$PRICE,   main=paste("Histogram of", make, model, "Prices"))
    hist(df_mmy$PRICE,  main=paste("Histogram of", make, model, year, "Prices"))
    hist(df_mmyt$PRICE, main=paste("Histogram of", make, model, year, trim, "Prices"))
}

