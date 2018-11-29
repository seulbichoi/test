{
  require(catboost)
  require(data.table) #rbindlist()
  require(dplyr) # select
  require(tibbletime)
  require(reshape2)
  require(ggplot2)
  require(timetk)
  require(readxl)
  require(forecast)
  require(readr)
  require(forecastxgb)
  require(randomForest)
  require(tidyr)
  require(bsts)# fill
}
{
  options(scipen=999)
}



#1 = def, 2 = depth, 3 = poisson, 4 = not tsclean
{parameter1 <- list(iterations = 500,
                    depth = 6,
                    #eval_metric = 'Poisson'
                    # subsample = 'Poisson',
                    learning_rate = 0.02,
                    l2_leaf_reg = 2,
                    bagging_temperature = 0.9,
                    random_strength = 1,
                    nan_mode = 'Min',
                    od_type = 'Iter',
                    thread_count = 8,
                    random_seed = 1)
}

###########################################################################
################### target cd select

target_cd <- readClipboard()

new_master$cat2 %>% unique() 

target_cd <- new_master %>% dplyr::filter(prod_cd %in% target_cd) %>%
  dplyr::filter(cat2 == "스킨케어") %>% .$prod_cd %>% unique()


###########################################################################
################### Data Load

target_cd <- readClipboard() #45개
lux_data <- readRDS("C:/Users/Choiseulbi/Desktop/AP/hera_51prod_xgbar/28_soldout_yn_test/lux_data.RDS")
lux_data2 <- lux_data %>% dplyr::filter(prod_cd %in% target_cd)
lux_data2$prod_cd %>% unique() %>% length()

###########################################################################
################### time_tk factor change - int
time_tk_date$week.iso <- as.numeric(time_tk_date$week.iso)
time_tk_date$month <- as.numeric(time_tk_date$month)
time_tk_date$day <- as.numeric(time_tk_date$day)

time_tk_date %>% summary()

###########################################################################
################### time_tk factor change - int

ap_xreg_data <- function(date = date){
  xreg_data2 <- time_tk_date %>%
    dplyr::filter(ymd <= as.Date(date)+120 & ymd > date) %>%
    dplyr::select(week.iso) %>% as.matrix()
  return(xreg_data2)
}



ap_cbmodel1 <- function(target_cd, date = ymd) {
  tryCatch({
    print(target_cd)
    
    
    # target cd / data.frame 
    prod_df <- lux_data2 %>% dplyr::filter(prod_cd == target_cd)
    
    
    # ts
    prod_ts <- ts(prod_df %>% 
                    dplyr::filter(ymd <= date) %>%
                    dplyr::select(sell_in_cnt),
                  frequency = 7,
                  start = 1)
    
    # variable add
    # prod_df$non_sale_yn <- ifelse(prod_df$sell_in_cnt == 0 , 1 , 0)
    # 
    # 
    # for ( i in 1 : nrow(prod_df) ){
    #   
    #   if ( i  < 15 ) {
    #     prod_df[i, 'non_sale_days'] <- sum(prod_df[c(1 : i), ]$non_sale_yn , na.rm =T)
    #   } else {
    #     prod_df[i, 'non_sale_days'] <- sum(prod_df[c((i - 14) : i), ]$non_sale_yn , na.rm =T)
    #   }
    #   
    # }
    
    
    # xreg data
    xreg_data <- prod_df %>% left_join(time_tk_date %>% dplyr::select(ymd, week.iso)) %>%
      dplyr::filter(prod_cd == target_cd & ymd <= date) %>%
      dplyr::select(week.iso) %>% as.matrix()
    
    
    # xreg_data2 <- prod_df %>% left_join(time_tk_date %>% dplyr::select(ymd, week.iso)) %>%
    #   dplyr::filter(prod_cd == target_cd) %>% dplyr::filter(ymd <= as.Date(date)+120 & ymd > date) %>%
    #   dplyr::select(week.iso, non_sale_days) %>% as.matrix()
    
    
    # forecast
    prod_model <- cat.forecast(
      prod_ts, 
      maxlag = 42,  
      params = parameter1, 
      xreg = xreg_data,
      pred_xreg = ap_xreg_data(date),
      season_type = c("dummies"))
    
    return(prod_model)}, 
    error = function(e){
      return(target_cd)
    }
  )
}


####################################################################################

# test - 3 repeat

stand_ymd = c("2018-04-15", "2018-05-15")


system.time({
  for (i in stand_ymd){
    print(Sys.time())
    tmp_rf1 <- lapply(target_cd, ap_cbmodel1, date = i)
    saveRDS(get(ls(pattern="tmp_rf1")), paste0(substr(i,6,7),"_hera_skin_raw.RDS"))
    rm(list=ls(pattern = "tmp_rf1"))
    print(i);print(Sys.time())
  }
})



hera_skin_4 <- readRDS(dir(pattern = "skin_15")[1])
hera_skin_5 <- readRDS(dir(pattern = "skin_15")[2])


tmp_index <- which(lapply(hera_skin_5, function(x){
  length(x)}) %>% unlist == 120)



###############################################################################################
################### daily product - catforecast test - real, pred value 

real_value <- daily_sell_in %>% dplyr::filter(prod_cd %in% target_cd) %>%
  group_by(prod_cd, month = month(ymd)) %>% summarise(real = sum(sell_fix_cnt, na.rm = T))

pred_value <- list()


for(i in 1:length(stand_ymd)){
  tmp_list <- list()
  pred <- rep3_result[[i]] #readRDS(paste0("D:/project/calculate_acc_month/",dir,lf[i]))
  tmp_m <- month(stand_ymd[i])
  tmp_index <- which(lapply(pred, function(x){
    length(x)}) %>% unlist == 120)
  
  # not_act <- which(lapply(pred, function(x){
  #   length(x)}) %>% unlist != 120)
  
  for(j in tmp_index){ # target_cd[tmp_index] - data frame select - grouping
    tmp_list[[j]] <- data.frame(value = pred[[j]]) %>% 
      mutate(ymd = seq(as.Date(stand_ymd[i]) + 1, as.Date(stand_ymd[i])+ 120, by = 1)) %>%
      group_by(month = month(ymd)) %>%
      summarise(pred = sum(value, na.rm = T)) %>%
      filter(month > tmp_m & month < (tmp_m+4))
  }
  
  pred_value[[i]] <- tmp_list %>% rbindlist %>% mutate(prod_cd = rep(target_cd[tmp_index], each = 3)) %>%
    left_join(real_value) %>% select(prod_cd, month, real, pred)
  
  
  pred_value[[i]]$acc <- acc_cal(pred_value[[i]])
  pred_value[[i]][pred_value[[i]]$real == 0 & pred_value[[i]]$pred == 0, "acc"] <- 1
  print(i)
}


for(i in 1:length(stand_ymd)){
  pred <- rep3_result[[i]] #readRDS(paste0("D:/project/calculate_acc_month/",dir,lf[i]))
  tmp_index <- which(lapply(pred, function(x){
    length(x)}) %>% unlist == 120)
  print(paste0("act : " ,length(tmp_index)))
  print(paste0("not-act : " , 720 - length(tmp_index)))
}

# [1] "act : 448"
# [1] "not-act : 272"
# [1] "act : 513"
# [1] "not-act : 207"
# [1] "act : 583"
# [1] "not-act : 137"


pred_value[[1]] %>% group_by(prod_cd) %>% summarise(acc = mean(acc)) %>% summary()
pred_value[[2]] %>% group_by(prod_cd) %>% summarise(acc = mean(acc)) %>% summary()
pred_value[[3]] %>% group_by(prod_cd) %>% summarise(acc = mean(acc)) %>% summary()


# 안돌아가는 애들 중에서 R등급 확인해보기
not_act <- list()

for(i in 1 : length(rep3_result)){
  pred <- rep3_result[[i]] #readRDS(paste0("D:/project/calculate_acc_month/",dir,lf[i]))
  not_act[[i]] <- which(lapply(pred, function(x){
    length(x)}) %>% unlist != 120)
}

not_act_list <- lapply(not_act, function(x) daily_rlist %>% dplyr::filter(prod_cd %in% target_cd[x]))

for ( i in 1: length(not_act_list)){
  
  print(not_act_list[[i]] %>% group_by(prod_grd) %>% summarise(cnt = n()))
  
}

# not_act_list는 r13, r14, r15만 해당
daily_rlist %>% group_by(prod_grd) %>% summarise(cnt = n())

# prod_grd   cnt
# R11         59
# R12         94
# R13        436
# R14         57
# R15         84

#not_act_list별로 학습 기간 전부터 90일 이상이고, 0의 개수 비율이 0.7이상인 애들 분석 진행
require(bsts)

##########################################################################################################
case_cd_check <- function(stand_ymd) {
  
  min_max <- daily_sell_in %>% group_by(prod_cd) %>% summarise(min_y = min(ymd), max_y = max(ymd)) %>% ungroup()
  
  case1 <- not_act_list[[i]] %>% left_join(min_max) %>%
    dplyr::filter(as.Date(stand_ymd) - 90 > as.Date(min_y)) %>% .$prod_cd %>% unique()
  
  case2 <- daily_sell_in %>% dplyr::filter(prod_cd %in% case1) %>%
    dplyr::filter(ymd <= stand_ymd) %>% group_by(prod_cd) %>% summarise(stand_day = n()) %>% ungroup()
  
  case_cd <- left_join(case2, daily_sell_in %>% dplyr::filter(prod_cd %in% case1) %>%
                         dplyr::filter(ymd <= stand_ymd) %>%
                         dplyr::filter(sell_fix_cnt == 0) %>%
                         group_by(prod_cd) %>%
                         summarise(count_0 = n())) %>%
    mutate(per = count_0 / stand_day) %>%
    dplyr::filter(per >= 0.75) %>% .$prod_cd %>% unique()
  
  return(case_cd)
  
}


bsts_model <- function(temp, date = last_date){
  
  model_set <- daily_sell_in %>% dplyr::filter(prod_cd %in% temp) %>%
    dplyr::filter(ymd <= date) %>%
    group_by(year = substr(ymd, 1, 4), month = month(ymd)) %>%
    summarise(sell_fix_cnt = sum(sell_fix_cnt))
  
  model_y <- ts(model_set$sell_fix_cnt, frequency = 7)
  ss1 <- AddLocalLinearTrend(list(), model_y)
  ss2 <- AddSeasonal(ss1, model_y, nseasons = 12)
  model <- bsts(model_y, state.specification = ss2, niter = 300)
  pred <- predict(model, h = 4)
  
  return(pred$mean)
  
}

##########################################################################################################


last_date <- c("2018-04-30", "2018-05-31", "2018-06-30")

for (i in last_date){
  
  temp <- lapply(stand_ymd, case_cd_check)
  bsts_result <- lapply(temp, bsts_model, date = i)
  
}






acc_cal <- function(data, real = "real", pred = "pred"){
  data <- data %>% na.omit %>% as.data.frame()
  tmp_real <- data[,real]
  tmp_pred <- data[,pred]
  tmp_acc <- NULL
  for(i in 1:nrow(data)){
    if(abs(tmp_pred[i] - tmp_real[i]) > tmp_real[i])
      tmp_acc[i] <- 0
    else tmp_acc[i] <- 1-(abs(tmp_real[i]-tmp_pred[i]) / (tmp_real[i]+1))
  }
  return(tmp_acc)
}







# 90일 이상 - min_date 필요 
# 0의 개수 비율 0.7 이상 
not_act_list[[i]] %>% View()

