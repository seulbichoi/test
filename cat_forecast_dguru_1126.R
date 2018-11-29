cat.forecast.ap <- function (y, xreg = NULL, pred_xreg = NULL, h = 42) {

    #### Checking Data
    
    if (!"ts" %in% class(y)) {
      stop("y class check : y need to ts class")
    }
    
    if (length(y))
      if (!is.null(xreg)) {
        xreg <- as.matrix(xreg)
        if (!is.numeric(xreg) | !is.matrix(xreg)) {
          stop("xreg check : class is numeric vector or a matrix")
        }
      }
    
    #### 데이터 전처리
    
    lambda = BoxCox.lambda(y)  #- 아웃라이어 보정을 위한 Lambda 값 산출
    y <- tsclean(y, lambda) #- 아웃라이어 보정
    origxreg <- xreg
    
    f <- stats::frequency(y) # 7로 고정  
    orig_n <- length(y) # 종속변수 갯수 
    
    if (orig_n < 46) {
      stop("must be data length > 45")
    } 
    
    target_y <- y
    
    
    n <- orig_n - 42 # 42개 일자를 활용하여 총길이 중 앞단 42일 제외하기 위함  (총길이 - 42)
    y2 <- target_y[-(1:(42))] # 종속변수 최초 42일 제외처리 
    
    #--- 독립변수(일자 + 더미) 추가 생성
    
    ncolx <- 48 # 42일 변수(42) + 더미 갯수(7) -1 
    
    x <- matrix(0, nrow = n, ncol = ncolx) 
    
    x[, 1:42] <- lag_y(target_y, 42) # -- 42일 데이터 만들기 
    
    #---- 더미 변수 생성을 위한 처리
    tmp <- data.frame(y = 0, x = as.character(rep_len(1:7, n))) 
    seasons <- stats::model.matrix(y ~ x, data = tmp)[,-1]
    x[, 42 + 1:6] <- seasons
    
    #---- 컬럼명 지정 
    colnames(x) <- c(paste0("lag", 1:42), paste0("season",  2:7))
    #
    
    #---- 제공된 추가 독립변수와의 바인딩
    if (!is.null(xreg)) {
      x <- cbind(x, origxreg[-c(1:42),])
    }
    
    
    
    ##### 모델링 관련  #####
    
    #-. Catboost 모델링 파라미터
    params <- list(
      iterations = 100,
      depth = 8,
      learning_rate = 0.01,
      l2_leaf_reg = 2,
      bagging_temperature = 2,
      random_strength = 1.5,
      od_type = 'Iter',
      thread_count = 8,
      random_seed = 1
      # , task_type = 'GPU' #- GPU 지원시 별로의 세팅 필요 (ex., R의경우 Cuda 필요) 
    )
    
    
    # cat_dt <- catboost::catboost.load_pool(data = x, label = y2)
    
    n <- length(y2)
    split_n <- round(0.632 * n) #--- Validation (2/3 - Train | 1/3 - Test)
    
    test_x <- x[1:split_n,] %>% as.matrix
    test_y2 <- y2[1:split_n] %>% as.matrix
    cat_test <- catboost::catboost.load_pool(data = test_x,
                                             label = test_y2)
    
    valid_x <- x[(split_n + 1):n,] %>% as.matrix
    valid_y2 <- y2[(split_n + 1):n] %>% as.matrix
    cat_valid <- catboost::catboost.load_pool(data = valid_x,
                                              label = valid_y2)
    model <- catboost::catboost.train(cat_test, cat_valid,
                                      params = params)
    
    
    if (!is.null(pred_xreg)) {
      xreg3 <- as.matrix(pred_xreg)
    } else {
      xreg3 <- NULL
    }
    
    
    #---- 예측 일자 (외부변수제공시 해당 Row 수 / 미제공시 제공된 h 값 )
    if (!is.null(pred_xreg)) {
      h = nrow(pred_xreg)
      if (is.null(h)) {
        h = length(pred_xreg)
      }  else {
        h = h
      }
    }
    
    
    rollup_cat <- function(x = x,
                           y = y,
                           model,
                           xregpred,
                           i) {
      newrow <- c(y[length(y)], x[nrow(x),-42])[c(1:42)]
      newrow <-
        c(newrow, x[(nrow(x) - 6 ), c((42 + 1):(42 + 6))])
      if (!is.null(xregpred)) {
        newrow <- c(newrow, xregpred)
      }
      newrow <- matrix(newrow, nrow = 1)
      colnames(newrow) <- colnames(x)
      pred_pool <- catboost.load_pool(newrow)
      pred <- catboost.predict(model, pred_pool)
      return(list(x = rbind(x, newrow), y = c(y, pred)))
    }
    
    y <- y2
    
    #- 위에 생성된 Function을 통해 h일자에 대한 예측 수행
    for (i in 1:h) {
      tmp <- rollup_cat(
        x,
        y,
        model = model,
        xregpred = xreg3[i,],
        i = i
      )
      x <- tmp$x
      y <- tmp$y
    }
    
    # 예측된 데이터 Select
    pred_y <- y[-(1:length(y2))]
    output <- pred_y
    return(output)
  }
