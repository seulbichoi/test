#calculate_acc for ap
{
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
}
{
  options(scipen=999)
  sell_in_total <- readRDS("D:/2018_proj/pms/rds/sell_in_total.RDS")
  target_cd <- readRDS("D:/2018_proj/pms/rds/target_cd.RDS")
  target_cd2 <- sell_in_total %>% filter(prod_cd %in% target_cd$prod_cd & ymd <= "2018-03-18") %>% 
    group_by(prod_cd) %>% 
    summarise(cnt = n()) %>% 
    filter(cnt > 200) %>% left_join(target_cd)
  time_tk_date <- readRDS("D:/2018_proj/pms/rds/time_tk.RDS")
}
ap_check_modelrf <- function(model, date = ymd, length.week = 6) { 
  week.iso = time_tk_date[time_tk_date$ymd == date, 2] + 1
  check_model <- data.frame(forecast(model, h = length.week*7)) %>%
    mutate(week = rep(week.iso:(week.iso + length.week-1), each = 7)) %>%
    group_by(week) %>%
    summarise(rf_pred = sum(Point.Forecast))
  return(check_model)
}
ap_acc <- function(data, real, pred){
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

lf = list.files("D:/2018_proj/R_proj/model_generator/rf/")
ymd = seq(as.Date("2018-04-01"), length.out = 23, by = 7)

rf <- list()
for(i in 1:length(lf)){
  rf[[i]] <- readRDS(paste0("D:/2018_proj/R_proj/model_generator/rf/",lf[i])) %>% 
    lapply(ap_check_modelrf, date= ymd[i]) %>% 
    rbindlist() %>% mutate(prod_cd = rep(target_cd2$prod_cd, each = 6))
}

ap_workacc <- function(df){
  df2 <- df[[1]] %>% 
    left_join(df[[2]], by = c("prod_cd", "week")) %>% 
    left_join(df[[3]], by = c("prod_cd", "week")) %>% 
    left_join(df[[4]], by = c("prod_cd", "week")) %>%
    select(prod_cd, everything())
  colnames(df2)[3:6] <- c("w-5","w-4","w-3","w-2")
  real <- sell_in_total %>% 
    filter(year.iso == 2018 & week.iso == df2[6,2]) %>% 
    mutate(week = week.iso) %>% group_by(prod_cd, week) %>% 
    summarise(real = sum(sell_in_cnt))
  df3 <- df2 %>% 
    mutate(pred = 0.4*`w-5`+0.3*`w-4`+0.2*`w-3`+0.1*`w-2`) %>% 
    left_join(real) %>% na.omit
  return(df3)
}
acc_df <- list()
for(i in 1:14){
  df <- list(rf[[i]],rf[[i+1]],rf[[i+2]],rf[[i+3]])
  acc_df[[i]] <- ap_workacc(df) %>% mutate(pred = round(pred)) %>%
    select(prod_cd, week, pred, real)
  acc = ap_acc(acc_df[[i]],4,3)
  acc_df[[i]] <- cbind(acc_df[[i]], acc) %>% left_join(lp)
  write.csv(acc_df[[i]], file = paste0("row_week_",i+18,".csv"), row.names = F)
  tmp <- acc_df[[i]] %>% group_by(brd, grade) %>% summarise(mean_acc = mean(acc))
  write.csv(tmp, file = paste0("acc_mean_week",i+18,".csv"), row.names = F)
}


lp <- read_excel("D:/2018_proj/R_proj/calculate_acc/level_price_0629.xlsx") %>% select(3,5) %>%  mutate(brd = substr(prod_cd, 1, 5))
lp[lp$grade == "R13",2] <- "R1345"
lp[lp$grade == "R14",2] <- "R1345"
lp[lp$grade == "R15",2] <- "R1345"


