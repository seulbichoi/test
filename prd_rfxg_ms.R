require(tibbletime)
require(data.table) #rbindlist()
require(plyr)
require(dplyr) # select
require(reshape2)
require(ggplot2)
require(timetk)
require(readxl)
require(forecast)
require(readr)
require(forecastxgb)
require(randomForest)
require(tidyr) # fill

options(scipen=999)
load("D:/2018_Rdata/sell_in_0730/sell_in_total.Rdata")
target_cd <- read_excel("D:/2018_Rdata/level_price_0629.xlsx") %>% select(2,3)
colnames(target_cd) <- c("brd", "prod_cd")
load("D:/2018_Rdata/time_tk/time_tk_0614.RData")
time_tk_date <- time_tk_date %>% mutate(week.iso = as.numeric(week.iso))


ap_ts <- function(target_cd, ymd) {
  prod_ts <- ts(sell_in_total %>% 
                  filter(prod_cd == target_cd & ymd <= ymd) %>%
                  select(sell_in_cnt),
                frequency = 7,
                start = 1)
  return(prod_ts)
}
ap_xgmodel <- function(data, maxlag = length.week * 7, nrounds_method = 'manual') {
  prod_model <- xgbar(data, seas_method = 'decompose', maxlag = maxlag, nrounds_method = nrounds_method)
  return(prod_model)
}
ap_rfmodel <- function(target_cd, ymd = ymd, maxlag = length.week * 7){
  prod_model <- rf_ts(sell_in_total %>% filter(prod_cd == target_cd & ymd <= ymd) %>%
                        select(sell_in_cnt), maxlag = maxlag)
}
ap_check_modelxg <- function(model, date=ymd, length.week = 6) { 
  week.iso = time_tk_date[time_tk_date$ymd == date, 8] + 1
  check_model <- data.frame(pred = forecast(model, 
                                            length.week * 7)$mean) %>%
    mutate(week = rep(week.iso:(week.iso + length.week - 1) , 
                      each = 7)) %>%
    group_by(week) %>%
    summarise(xg_pred = sum(pred))
  return(check_model)
}
ap_check_modelrf <- function(model, date=ymd, length.week = 6) { 
  week.iso = time_tk_date[time_tk_date$ymd == date, 8] + 1
  check_model <- data.frame(forecast(model, 
                                     h = length.week * 7)) %>%
    mutate(week = rep(week.iso:(week.iso + length.week - 1) , 
                      each = 7)) %>%
    group_by(week) %>%
    summarise(rf_pred = sum(Point.Forecast))
  return(check_model)
}

########################## setting ##########################
# ymd = "2018-07-29" 
# prod_cd = target_cd %>% filter(brd == "헤라") %>% .$prod_cd 
# length.week = 20
brd = c("헤라") #"설화수", "프리메라", "아모레퍼시픽")
ymd <- seq(as.Date('2018-03-18'), length.out = 11, by = 7)
#############################################################

for (i in brd){
  for (j in ymd){
    tmp_xgts <- lapply(prod_cd, ap_ts, ymd = ymd)
    tmp_xgmodel <- lapply(tmp1, ap_xgmodel)
    tmp_rfmodel <- lapply(prod_cd, ap_rfmodel)
    tmp_xgpred <- lapply(tmp2, ap_check_modelxg) %>% rbindlist() %>% mutate(prod_cd = rep(prod_cd, each = length.week))
    tmp_rfpred <- lapply(tmp3, ap_check_modelrf) %>% rbindlist() %>% mutate(prod_cd = rep(prod_cd, each = length.week))
    final <- tmp_xgpred %>% left_join(tmp_rfpred, by = c("week", "prod_cd")) %>% select(prod_cd, week, everything())
  }
}

