
  
  
############################
rf_ts <- function (y, xreg = NULL, maxlag = max(8, 2 * frequency(y)), 
                lambda = 1, verbose = FALSE, seas_method = c("dummies",                                                                            "decompose", "fourier", "none"), K = max(1, min(round(f/4 - 1), 10))) 
{
    seas_method = match.arg(seas_method)
    # trend_method = match.arg(trend_method)
    
#### TREAT DATA
    
    # 1. Treat xreg  
    if (!is.null(xreg)) {
        if (class(xreg) == "ts" | "data.frame" %in% class(xreg)) {
            message("Converting  -> matrix")
            xreg <- as.matrix(xreg)
        }
        if (!is.numeric(xreg) | !is.matrix(xreg)) {
            stop("only matrix")
        }
    }


    # 2. Treat Y
    if (!"ts" %in% class(y)) {
        y <- ts(y,frequency = 7)
        #stop("y must be a univariate time series")
    }
    if (length(y) < 4) {
        stop("time-series is short")
    }


    f <- stats::frequency(y)
    untransformedy <- y
    orign <- length(y)
    origy <- JDMod(y, lambda = lambda)

    
#### SEASON_METHOD 
    #### 1. decompose
    if (seas_method == "decompose") {
        decomp <- decompose(origy, type = 'multiplicative')
        origy <- seasadj(decomp)
    }

    # #### 2. differencing
    diffs <- 0
    # if (trend_method == "differencing") {
    #     alpha = 0.05
    #     dodiff <- TRUE
    #     while (dodiff) {
    #         suppressWarnings(dodiff <- tseries::kpss.test(origy)$p.value < 
    #                              alpha)
    #         if (dodiff) {
    #             diffs <- diffs + 1
    #             origy <- ts(c(0, diff(origy)), start = start(origy), 
    #                         frequency = f)
    #         }
    #     }
    # }


    #### 3. dummies
    if (maxlag < f & seas_method == "dummies") {
        stop("At least one full period of lags needed when seas_method = dummies.")
    }
  
    
    if (maxlag > (orign - f - round(f/4))) {
        warning(paste("y is too short for", maxlag, "to be the value of maxlag.  Reducing maxlags to", 
                      orign - f - round(f/4), "instead."))
        maxlag <- orign - f - round(f/4)
    }
    
    if (maxlag != round(maxlag)) {
        maxlag <- ceiling(maxlag)
        if (verbose) {
            message(paste("Rounding maxlag up to", maxlag))
        }
    }
    


    origxreg <- xreg
    n <- orign - maxlag
    
    #### 
    y2 <- ts(origy[-(1:(maxlag))], start = time(origy)[maxlag + 
                                                           1], frequency = f)
    

    #### 
    # if (nrounds_method == "cv" & n < 15) {
    #     warning("y is too short for cross-validation.  Will validate on the most recent 20 per cent instead.")
    #     nrounds_method <- "v"
    # }
    # 
    
    if (seas_method == "dummies" & f > 1) {
        ncolx <- maxlag + f - 1
    }
    
    
    if (seas_method == "decompose") {
        ncolx <- maxlag
    }
    
    
    if (seas_method == "fourier" & f > 1) {
        ncolx <- maxlag + K * 2
    }
    
    
    if (seas_method == "none" | f == 1) {
        ncolx <- maxlag
    }
    
    x <- matrix(0, nrow = n, ncol = ncolx)
    
    x[, 1:maxlag] <- lagv(origy, maxlag, keeporig = FALSE)
    if (f > 1 & seas_method == "dummies") {
        tmp <- data.frame(y = 1, x = as.character(rep_len(1:f, 
                                                          n)))
        seasons <- model.matrix(y ~ x, data = tmp)[, -1]
        x[, maxlag + 1:(f - 1)] <- seasons
        colnames(x) <- c(paste0("lag", 1:maxlag), paste0("season", 
                                                         2:f))
    }
    
    
    if (f > 1 & seas_method == "fourier") {
        fx <- fourier(y2, K = K)
        x[, (maxlag + 1):ncolx] <- fx
        colnames(x) <- c(paste0("lag", 1:maxlag), colnames(fx))
    }
    
    
    if (f == 1 || seas_method == "decompose" || seas_method == 
        "none") {
        colnames(x) <- c(paste0("lag", 1:maxlag))
    }
    
    
    if (!is.null(xreg)) {
        xreg <- lagvm(xreg, maxlag = maxlag)
        x <- cbind(x, xreg[, , drop = FALSE])
    }
    
    
    # if (nrounds_method == "cv") {
    #     if (verbose) {
    #         message("Starting cross-validation")
    #     }
    #     
        # cv <- xgb.cv(data = x, label = y2, nrounds = nrounds, 
        #              nfold = nfold, early_stopping_rounds = 5, maximize = FALSE, 
        #              verbose = verbose, ...)
        # nrounds_use <- cv$best_iteration
    # } else {
    #     if (nrounds_method == "v") {
    #         nrounds_use <- validate_xgbar(y, xreg = xreg, ...)$best_nrounds
    #     }
    #     else {
    #         nrounds_use <- nrounds
    #     }
    # }
    
    if (verbose) {
        message("Fitting random forest _time seires ")
    }
    
    model <- randomForest(x= x,   y = y2  )
    fitted <- ts(c(rep(NA, maxlag), predict(model, newdata = x)), 
                 frequency = f, start = min(time(origy)))
    
    # if (trend_method == "differencing") {
    #     for (i in 1:diffs) {
    #         fitted[!is.na(fitted)] <- ts(cumsum(fitted[!is.na(fitted)]), 
    #                                      start = start(origy), frequency = f)
    #     }
    #     fitted <- fitted + JDMod(untransformedy[maxlag + 1], 
    #                              lambda = lambda)
    # }
    
    if (seas_method == "decompose") {
        fitted <- fitted * decomp$seasonal %>% round()
    }
    
    
    fitted <- InvJDMod(fitted, lambda = lambda)
    method <- paste0("xgbar(", maxlag, ", ", diffs, ", ")
    if (f == 1 | seas_method == "none") {
        method <- paste0(method, "'non-seasonal')")
    }
    
    else {
        method <- paste0('randomforest', "'", seas_method, "')")
    }
    
    output <- list(y = untransformedy, y2 = y2, x = x, model = model, 
                   fitted = fitted, maxlag = maxlag, seas_method = seas_method, 
                   diffs = diffs, lambda = lambda, method = method)
    if (seas_method == "decompose") {
        output$decomp <- decomp
    }
    
    if (seas_method == "fourier" & f != 1) {
        output$fx <- fx
        output$K <- K
    }
    
    if (!is.null(xreg)) {
        output$origxreg = origxreg
        output$ncolxreg <- ncol(origxreg)
    }
    
    class(output) <- "xgbar"
    return(output)
}
