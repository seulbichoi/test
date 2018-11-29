sell_in_total <- readRDS("C:\\Users\\Choiseulbi\\Desktop\\AP\\hera_51prod_xgbar\\new_sell_in_total.RDS")
# lux_cd <- readRDS("C:\\Users\\Choiseulbi\\Desktop\\AP\\hera_51prod_xgbar\\cd.RDS")
lux_all_cd <- readRDS("C:\\Users\\Choiseulbi\\Desktop\\AP\\hera_51prod_xgbar\\lux_all_rgrade_cd.RDS")
lux_all_cd <- lux_all_cd %>% unique() # 전체 코드 789개 

##################################################################################################
lux_data <- sell_in_total %>% dplyr::filter(prod_cd %in% lux_all_cd)

stand_ymd <- c("2018-04-15", "2018-05-15", "2018-06-15")

lux_cd_check <- lapply(stand_ymd, case_cd_check)

##################################################################################################
case_cd_check <- function(stand_ymd) {
  
  min_max <- lux_data %>% group_by(prod_cd) %>% summarise(min_y = min(ymd), max_y = max(ymd)) %>% ungroup()
  
  case1 <- lux_data %>% left_join(min_max) %>%
    dplyr::filter(as.Date(stand_ymd) - 90 > as.Date(min_y)) %>% .$prod_cd %>% unique()
  
  case2 <- lux_data %>% dplyr::filter(prod_cd %in% case1) %>%
    dplyr::filter(ymd <= stand_ymd) %>% group_by(prod_cd) %>% summarise(stand_day = n()) %>% ungroup()
  
  case_cd <- left_join(case2, lux_data %>% dplyr::filter(prod_cd %in% case1) %>%
                         dplyr::filter(ymd <= stand_ymd) %>%
                         dplyr::filter(sell_in_cnt == 0) %>%
                         group_by(prod_cd) %>%
                         summarise(count_0 = n())) %>%
    mutate(per = count_0 / stand_day) %>%
    dplyr::filter(per >= 0.75) %>% .$prod_cd %>% unique()
  
  return(case_cd)
  
}


##################################################################################################
# 명석씨가 보내준 코드들 중, 기간이 짧아서 안돌아가는 제품들 제외

lux_month_error <- list()
lux_min_max <- list()
lux_case1_cd <- list()

bsts_daily <- list()
daily_y <- list()
model <- list()
pred <- list()
real <- list()
real1 <- list()


# month_real1 - acc
for (i in 1 : 3) {
  
  lux_month_error[[i]] <- lux_data %>% dplyr::filter(prod_cd %in% lux_error_cd[,i])
  lux_min_max[[i]] <- lux_month_error[[i]] %>% group_by(prod_cd) %>% summarise(min_y = min(ymd), max_y = max(ymd))
  lux_case1_cd[[i]] <- lux_month_error[[i]] %>% left_join(lux_min_max[[i]]) %>%
    dplyr::filter(as.Date(stand_ymd[i]) - 90 > as.Date(min_y)) %>% .$prod_cd %>% unique() %>% as.character()
  
  stand_month <- month(stand_ymd[i])
  
  for ( j in 1 : length(lux_case1_cd[[i]])) {
    bsts_daily[[j]] <- lux_month_error[[i]] %>% dplyr::filter(prod_cd == lux_case1_cd[[i]][j]) %>%
      dplyr::filter(ymd <= last_date[i]) %>%
      group_by(year = substr(ymd, 1, 4), month = month(ymd)) %>%
      summarise(sell_fix_cnt = sum(sell_in_cnt))
    
    daily_y[[j]] <- ts(bsts_daily[[j]]$sell_fix_cnt, frequency = 7)
    ss1 <- AddLocalLinearTrend(list(), daily_y[[j]])
    ss2 <- AddSeasonal(ss1, daily_y[[j]], nseasons = 12)
    model[[j]] <- bsts(daily_y[[j]], state.specification = ss2, niter = 300)
    pred[[j]] <- predict(model[[j]], h=3)
    
    real[[j]] <- lux_month_error[[i]] %>% dplyr::filter(prod_cd == lux_case1_cd[[i]][j]) %>%
      dplyr::filter(ymd > last_date[i]) %>%
      group_by(year = substr(ymd, 1, 4), month = month(ymd)) %>%
      summarise(sell_fix_cnt = sum(sell_in_cnt)) %>% dplyr::filter(month > month(stand_ymd[i]), month <= stand_month+3)
    real[[j]]$prod_cd <- lux_case1_cd[[i]][j]
    real[[j]]$pred <- round(pred[[j]]$mean)
    real[[j]]$pred <- ifelse(real[[j]]$pred < 0 , 0 , real[[j]]$pred)
    real[[j]]$acc <- acc_cal(real[[j]], real = "sell_fix_cnt")
    
  }
  
  real1[[i]] <- real
  
}

saveRDS(real1, "month_real1.RDS")

##########################################################################################################

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

##########################################################################################################

for ( i in 1 : length(day_real2)){
  
  print(bind_rows(day_real2[[2]]) %>% group_by(month) %>%
          summarise(acc= mean(acc)))
  
}

##########################################################################################################
# day_real1 - acc
for (i in 1 : 3) {
  
  lux_month_error[[i]] <- lux_data %>% dplyr::filter(prod_cd %in% lux_error_cd[,i])
  lux_min_max[[i]] <- lux_month_error[[i]] %>% group_by(prod_cd) %>% summarise(min_y = min(ymd), max_y = max(ymd))
  lux_case1_cd[[i]] <- lux_month_error[[i]] %>% left_join(lux_min_max[[i]]) %>%
    dplyr::filter(as.Date(stand_ymd[i]) - 90 > as.Date(min_y)) %>% .$prod_cd %>% unique() %>% as.character()
  
  stand_month <- month(stand_ymd[i])

  for ( j in 1 : length(lux_case1_cd[[i]])) {
    bsts_daily[[j]] <- lux_month_error[[i]] %>% dplyr::filter(prod_cd == lux_case1_cd[[i]][j]) %>%
      dplyr::filter(ymd <= stand_ymd[i])

    daily_y[[j]] <- ts(bsts_daily[[j]]$sell_in_cnt, frequency = 7)
    ss1 <- AddLocalLinearTrend(list(), daily_y[[j]])
    ss2 <- AddSeasonal(ss1, daily_y[[j]], nseasons = 12)
    model[[j]] <- bsts(daily_y[[j]], state.specification = ss2, niter = 300)
    pred[[j]] <- predict(model[[j]], h=120)

    real[[j]] <- lux_month_error[[i]] %>% dplyr::filter(prod_cd == lux_case1_cd[[i]][j]) %>%
      dplyr::filter(ymd > last_date[i] , month(ymd) <= stand_month+3)

    row_count <- nrow(real[[j]])

    real[[j]]$prod_cd <- lux_case1_cd[[i]][j]
    real[[j]]$pred <- round(pred[[j]]$mean)[1:row_count]
    # real[[j]] <- real[[j]] %>% group_by(prod_cd, month) %>%
    #   summarise(sell_in_cnt = sum(sell_in_cnt), pred = sum(pred))

    real[[j]]$pred <- ifelse(real[[j]]$pred < 0 , 0 , real[[j]]$pred)
    real[[j]]$acc <- acc_cal(real[[j]], real = "sell_in_cnt")

  }

  real1[[i]] <- real
  
}

##########################################################################################################
# 90일 이상 day_real2 - acc

bsts_daily <- list()
daily_y <- list()
model <- list()
pred <- list()
real <- list()
# real1 <- list()



# for (i in 1 : 3) {
  
  # lux_month_error[[i]] <- lux_data %>% dplyr::filter(prod_cd %in% lux_error_cd[,i])
  # lux_min_max[[i]] <- lux_month_error[[i]] %>% group_by(prod_cd) %>% summarise(min_y = min(ymd), max_y = max(ymd))
  # lux_case1_cd[[i]] <- lux_month_error[[i]] %>% left_join(lux_min_max[[i]]) %>%
  #   dplyr::filter(as.Date(stand_ymd[i]) - 90 <= as.Date(min_y)) %>%
  #   dplyr::filter(min_y < stand_ymd[i]) %>%
  #   .$prod_cd %>% unique() %>% as.character()
  
  i=3
  for ( j in 1 : length(lux_case1_cd[[i]])) {
    bsts_daily[[j]] <- lux_month_error[[i]] %>% dplyr::filter(prod_cd == lux_case1_cd[[i]][j]) %>%
      dplyr::filter(ymd <= stand_ymd[i])
    
    real[[j]] <- lux_month_error[[i]] %>% dplyr::filter(prod_cd == lux_case1_cd[[i]][j]) %>%
      dplyr::filter(ymd > stand_ymd[i])
    
    row_count <- nrow(real[[j]])
    
    daily_y[[j]] <- ts(bsts_daily[[j]]$sell_in_cnt, frequency = 7)
    ss1 <- AddLocalLinearTrend(list(), daily_y[[j]])
    ss2 <- AddSeasonal(ss1, daily_y[[j]], nseasons = 12)
    model[[j]] <- bsts(daily_y[[j]], state.specification = ss2, niter = 300)
    pred[[j]] <- predict(model[[j]], h= row_count)
    
    real[[j]]$prod_cd <- lux_case1_cd[[i]][j]
    real[[j]]$pred <- round(pred[[j]]$mean)
    
    real[[j]]$pred <- ifelse(real[[j]]$pred < 0 , 0 , real[[j]]$pred)
    real[[j]]$acc <- acc_cal(real[[j]], real = "sell_in_cnt")
    
  }
  
   real1[[3]] <- real
  
# }

real1 <- list()

# 기준일 보다 제품이 주문 안된 경우 - 빼고 시작하기

setwd("C:\\Users\\Choiseulbi\\Desktop\\AP\\hera_51prod_xgbar")

saveRDS(real1, "month_real1.RDS")
month_real1 <- readRDS(paste0(getwd(), "/", "month_real1.RDS"))

saveRDS(real1, "day_real1.RDS")
day_real1 <- readRDS(paste0(getwd(), "/", "day_real1.RDS"))

saveRDS(real1, "day_real2.RDS")
day_real2 <- readRDS(paste0(getwd(), "/", "day_real2.RDS"))


##########################################################################################################
# 90일 이전 / 0비율 check



case2 <- lux_data %>% dplyr::filter(prod_cd %in% lux_case1_cd[[3]]) %>%
  dplyr::filter(ymd <= stand_ymd[3]) %>% group_by(prod_cd) %>% summarise(stand_day = n()) %>% ungroup()

case_cd <- left_join(case2, lux_data %>% dplyr::filter(prod_cd %in% lux_case1_cd[[3]]) %>%
                       dplyr::filter(ymd <= stand_ymd[3]) %>%
                       dplyr::filter(sell_in_cnt == 0) %>%
                       group_by(prod_cd) %>%
                       summarise(count_0 = n())) %>%
  mutate(per = count_0 / stand_day)
#  %>% dplyr::filter(per >= 0.75) %>% .$prod_cd %>% unique()

case_cd %>% summary()




