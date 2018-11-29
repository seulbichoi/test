#################################################################################################
#################################################################################################

require(dplyr)
require(lubridate)
require(data.table)
require(forecast)
require(forecastxgb)
require(randomForest)


#################################################################################################
#################################################################################################
# 헤라 제품에 대한 7월 8월 예측 TEST 진행

colnames(r_list) <- c("brd", "prod_cd", "prod_nm", "level")

h_test <- sell_in_total %>% select(prod_cd, ymd, cnt = sell_in_cnt, yy = year.iso, ww = week.iso) %>%
  dplyr::filter(substr(prod_cd, 1, 5) == "11107")

s_test <- sell_in_total %>% select(prod_cd, ymd, cnt = sell_in_cnt, yy = year.iso, ww = week.iso) %>%
  dplyr::filter(substr(prod_cd, 1, 5) == "11117")

p_test <- sell_in_total %>% select(prod_cd, ymd, cnt = sell_in_cnt, yy = year.iso, ww = week.iso) %>%
  dplyr::filter(substr(prod_cd, 1, 5) == "11064")

ap_test <- sell_in_total %>% select(prod_cd, ymd, cnt = sell_in_cnt, yy = year.iso, ww = week.iso) %>%
  dplyr::filter(substr(prod_cd, 1, 5) == "11138")


###################
# ap_data_merge <- function(df,x){
#   df %>% select(prod_cd, ymd, cnt = sell_in_cnt, yy = year.iso, ww = week.iso) %>%
#   dplyr::filter(substr(prod_cd, 1, 5) == x )
# }
# 
# brand_cd <- c('11107','11117','11064','11138')
# 
# test_df <- lapply( brand_cd, 
#                    function(x) ap_data_merge(sell_in_total, x)
#                    )
# 
# test_df[[2]] %>% View()
#######################

 
target_cd <- r_list %>% dplyr::filter(brd =="아모레퍼시픽") %>% .$prod_cd


# target_cd <-
#   h_test %>% dplyr::filter(cnt > 0) %>% group_by(prod_cd) %>%
#   summarise(min_dt = min(ymd), max_dt = max(ymd)) %>%
#   left_join(h_test, by = 'prod_cd') %>% 
#   dplyr::filter(ymd > min_dt - 1 & ymd <= max_dt)%>% 
#   dplyr::filter(year(min_dt) < 2018) %>% group_by(prod_cd) %>% summarise(count = n()) %>%
#   dplyr::filter(count > 180) %>% inner_join(r_list, by = 'prod_cd') %>% .$prod_cd
  
  
#################################################################################################
#################################################################################################
# 헤라 제품에 대한 7월 8월 예측 TEST 진행


# h_test 에서 targetcd 애들 가져오기

test_list <- lapply(target_cd, function(x) ap_test %>% dplyr::filter(prod_cd == x))


# 31주차(7월 30일부터) 6주(36주차까지) 예측 (수행 완료) - prd에 저장 
# 29주차(7월 16일부터) 6주(34주차까지) 예측 - 3,4,5,6의 합을 구해야함 (수행 완료) - prd2에 저장

fc_day = "2018-07-30"
fc_end = "2018-09-09"

fc_day = "2018-07-16"
fc_end = "2018-08-26"

temp <- lapply(test_list, function(x) x %>% dplyr::filter(ymd < fc_day))

############################################################################
#rf_ts model
result <- list()

system.time (

for (i in 1:length(target_cd)){

  result[[i]] <- rf_ts(temp[[i]]$cnt, maxlag=42)

})



############################################################################
# model forecast

prd1 <- list()
prd2 <- list()
# require(forecastxgb)

for ( i in 1:length(target_cd)){
  # prd1[[i]] <- forecast(result[[i]], h=42) %>% data.frame()
  prd2[[i]] <- forecast(result[[i]], h=42) %>% data.frame()
}


prd1[[80]] %>% View()
prd2[[6]] %>% View()

for (i in 1:length(prd2)){
  
  # colnames(prd1[[i]]) <- c("pred1")
  # prd1[[i]]$pred1 <- round(prd1[[i]]$pred1)
  # prd1[[i]]$prod_cd <- target_cd[i]
  # prd1[[i]]$ymd <- seq(as.Date(fc_day), as.Date(fc_end), by = "day")
  # prd1[[i]] <- as.data.frame(prd1[[i]])
  # prd1[[i]] <- prd1[[i]] %>% select(prod_cd, ymd, pred1)
  # 
  colnames(prd2[[i]]) <- c("pred2")
  prd2[[i]]$pred2 <- round(prd2[[i]]$pred2)
  prd2[[i]]$prod_cd <- target_cd[i]
  prd2[[i]]$ymd <- seq(as.Date(fc_day), as.Date(fc_end), by = "day")
  prd2[[i]] <- as.data.frame(prd2[[i]])
  prd2[[i]] <- prd2[[i]] %>% select(prod_cd, ymd, pred2)

}

prd2[[80]] %>% View()

prd1 <- rbindlist(prd1)
prd2 <- rbindlist(prd2)

final_pred <- full_join(prd2, prd1, by = c('ymd', 'prod_cd')) %>%
  left_join(time_tk_date %>% select(ymd, week.iso), by = 'ymd')

save(final_pred, file="final_pred_ap.Rdata")


############################################################################
# 그 이후에 전처리 할 부분 

# for ( i in 1:length(target_cd)){
#   
#   real <- h_test %>% dplyr::filter(prod_cd == target_cd[i]) %>%
#     dplyr::filter(ymd >= fc_day) %>%
#     dplyr::filter(ymd <= fc_end)
#   
#   f_list[[i]] <- cbind(real, prd)
# }
# 

# # rbindlist하기
# rp <- list()
# 
# for (i in 1:7){
#   
#   rp[[i]] <- real_pred[[i]] %>% group_by(prod_cd, ww=week.iso) %>%
#     summarise(real=sum(sell_in_cnt), pred=sum(Point.Forecast)) %>%
#     mutate(stand = 12+i)
#   
# }
# 
# rp <- rbindlist(rp)
