######### 평균 주문수량 어느정도 있는 제품들 대상으로 xgbar
######### 람다값도 바꿔보기 

colnames(r_list) <- c("staff", "brd", "prod_cd", "prod_nm", "level")

#헤라 설화수 프리메라 대상 
xg_11 <- sell_in_total %>% dplyr::filter(prod_cd %in% r_list$prod_cd) %>% left_join(time_tk_date %>% select(ymd, week.iso, year.iso, day = wday.lbl, holi, sat_sun), by = 'ymd')

# 연평균 주문수량이 30개 이상인 제품 선정 
xg_11cd <- xg_11 %>% group_by(prod_cd, year.iso) %>% summarise(cnt = mean(sell_in_cnt)) %>% 
  group_by(prod_cd) %>% summarise(cnt = mean(cnt)) %>% dplyr::filter(cnt > 29) %>% select(prod_cd) %>% unique() %>% .$prod_cd

xg_11 <- xg_11 %>% dplyr::filter(prod_cd %in% xg_11cd) %>% mutate(brd = substr(prod_cd, 1, 5))

#################################################################################################################

colnames(all_brand_master)

# 카테고리 별 평균 주문수량 df 만들기
cat3_cnt <- xg_11 %>% group_by(cat3, ymd) %>%
  summarise(cnt = sum(sell_in_cnt))


xg_11 <- left_join(xg_11, all_brand_master %>%
                     select(prod_cd, cat3), by = 'prod_cd') %>% mutate(last_dat = as.Date(ymd) - 365)
  
xg_11 <- xg_11 %>% inner_join(cat3_cnt %>% select(cat3, last_dat = ymd, cnt),by = c('cat3', 'last_dat')) %>% left_join(chodo)

xg_11$last_cnt <- log(xg_11$cnt+1)
xg_11_final <- inner_join(xg_11, date_list %>% select(ymd, chodo_p), by = c("ymd"))

# # 프리메라 58개, 헤라 161개, 설화수 126개
# day_cd 조건을 통해서 헤라 78개, 설화수 89개 프리메라 31개 
xg_11_final %>% dplyr::filter(prod_cd %in% day_cd) %>% select(prod_cd, brd) %>% unique() %>% group_by(brd) %>% summarise(n())

# 판매기간이 365일 이상인 제품 선정  
day_cd <- xg_11_final %>% select(prod_cd, ymd) %>% group_by(prod_cd) %>% summarise(count = n()) %>% dplyr::filter(count > 364) %>% select(prod_cd) %>% unique() %>% .$prod_cd

xg_11_final <- xg_11_final %>% dplyr::filter(prod_cd %in% day_cd)
left_join(xg_11_final %>% select(prod_cd) %>% unique(), all_brand_cd_nm, by = 'prod_cd') %>% View()

#################################################################################################################

# 브랜드 별로 list 만들기
xg_11_final$brd %>% unique()
brd <- c("11107", "11117", "11064")
# 헤라, 설화수, 프리메라 순으로 생성

brd_xg_11 <- lapply(brd, function(x) xg_11_final %>%
                      dplyr::filter(xg_11_final$brd %in% x))


brd_xg_11[[1]] %>% View()
#################################################################################################################

hera_xg_11 <- lapply(brd_xg_11[[1]]$prod_cd %>% unique() ,
                     function(x)
                       dplyr::filter(
                         brd_xg_11[[1]] %>%
                           dplyr::select(
                             prod_cd,
                             ymd,
                             sell_in_cnt,
                             chodo,
                             week.iso,
                             year.iso,
                             holi,
                             sat_sun,
                             last_cnt,
                             chodo_p
                           ),
                         prod_cd == x
                       ) %>% mutate(max_date = max(brd_xg_11[[1]]$ymd)))




sulwha_xg_11 <- lapply(brd_xg_11[[2]]$prod_cd %>% unique() ,
                     function(x)
                       dplyr::filter(
                         brd_xg_11[[2]] %>%
                           dplyr::select(
                             prod_cd,
                             ymd,
                             sell_in_cnt,
                             chodo,
                             week.iso,
                             year.iso,
                             holi,
                             sat_sun,
                             last_cnt,
                             chodo_p
                           ),
                         prod_cd == x
                       ) %>% mutate(max_date = max(brd_xg_11[[2]]$ymd)))


primera_xg_11 <- lapply(brd_xg_11[[3]]$prod_cd %>% unique() ,
                     function(x)
                       dplyr::filter(
                         brd_xg_11[[3]] %>%
                           dplyr::select(
                             prod_cd,
                             ymd,
                             sell_in_cnt,
                             chodo,
                             week.iso,
                             year.iso,
                             holi,
                             sat_sun,
                             last_cnt,
                             chodo_p
                           ),
                         prod_cd == x
                       ) %>% mutate(max_date = max(brd_xg_11[[3]]$ymd)))

#################################################################################################################


xg_train <- lapply(primera_xg_11, function(x) x %>% dplyr::filter(ymd <= as.Date(max_date) - 29))
xg_test  <- lapply(primera_xg_11, function(x) x %>% dplyr::filter(as.Date(max_date) - 29 < ymd) %>%
                     dplyr::filter(ymd < as.Date(max_date)))

xg_train[[2]] %>% View()
xg_test[[2]] %>% View()


xg_train_y <- lapply(xg_train, function(x) ts(x[,3], frequency = 7))

xg_train_xreg <- lapply(xg_train, function(x) x[,c(4, 7, 8, 9, 10)] %>% as.matrix())
xg_test_xreg <- lapply(xg_test, function(x) x[,c(4, 7, 8, 9, 10)] %>% as.matrix())




xg_m <- list()
result <- list()
pred <- list()

for (i in 1 : length(xg_train_y)){
  xg_m[[i]] <- xgbar(xg_train_y[[i]], maxlag=45, nrounds = 10,  xreg=xg_train_xreg[[i]],
                     nrounds_method = "manual")
  result[[i]] <- forecast(xg_m[[i]], xreg=xg_test_xreg[[i]])
  pred[[i]] <- round(result[[i]]$mean)
  
}


pred[[9]] %>% View()

real <- lapply(xg_test, function(x) head(x, 29))

t1 <- lapply(c(1:length(real)),
             function(x) data.frame(real[[x]], test = pred[[x]]))

real_t2 <- rbindlist(t1) %>% select(prod_cd, ymd, real = sell_in_cnt, pred = test, yy = year.iso, ww = week.iso) %>% group_by(prod_cd, yy, ww) %>% summarise(real = sum(real), pred = sum(pred)) %>%
  mutate(acc = 1 - (abs(real - pred) / real))
real_t2$acc <- ifelse((abs(real_t2$real-real_t2$pred) > real_t2$real) , 0 , real_t2$acc)

all_test[[3]] <- real_t2

################################################################################################################
# 람다 값 수정

xg_train <- lapply(primera_xg_11, function(x) x %>% dplyr::filter(ymd <= as.Date(max_date) - 29))
xg_test  <- lapply(primera_xg_11, function(x) x %>% dplyr::filter(as.Date(max_date) - 29 < ymd) %>%
                     dplyr::filter(ymd < as.Date(max_date)))

# xg_train[[2]] %>% View()
# xg_test[[2]] %>% View()


xg_train_y <- lapply(xg_train, function(x) ts(x[,3], frequency = 7))

xg_train_xreg <- lapply(xg_train, function(x) x[,c(4, 7, 8, 9, 10)] %>% as.matrix())
xg_test_xreg <- lapply(xg_test, function(x) x[,c(4, 7, 8, 9, 10)] %>% as.matrix())




xg_m <- list()
result <- list()
pred <- list()

for (i in 1 : length(xg_train_y)){
  xg_m[[i]] <- xgbar(xg_train_y[[i]], maxlag=45, nrounds = 10, lambda = 0.8,  xreg=xg_train_xreg[[i]],
                     nrounds_method = "manual")
  result[[i]] <- forecast(xg_m[[i]], xreg=xg_test_xreg[[i]])
  pred[[i]] <- round(result[[i]]$mean)
  
}


# pred[[9]] %>% View()

real <- lapply(xg_test, function(x) head(x, 29))

t1 <- lapply(c(1:length(real)),
             function(x) data.frame(real[[x]], test = pred[[x]]))

real_t2 <- rbindlist(t1) %>% select(prod_cd, ymd, real = sell_in_cnt, pred = test, yy = year.iso, ww = week.iso) %>% group_by(prod_cd, yy, ww) %>% summarise(real = sum(real), pred = sum(pred)) %>%
  mutate(acc = 1 - (abs(real - pred) / real))
real_t2$acc <- ifelse((abs(real_t2$real-real_t2$pred) > real_t2$real) , 0 , real_t2$acc)


all_test[[28]] <- real_t2
all_test[[29]] <- real_t2
all_test[[30]] <- real_t2


all_test[[2]] %>% View()
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################


all_test_nm <- lapply(all_test, function(x) left_join(x, all_brand_cd_nm, by = 'prod_cd'))
all_test_nm[[28]] %>% View()

# all_test[[1]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[2]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[3]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[4]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[5]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[6]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[7]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[8]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[9]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[10]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[11]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[12]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[13]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[14]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[15]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[16]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[17]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[18]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[19]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[20]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[21]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[22]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[23]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[24]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[25]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[26]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[27]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[28]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[29]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()
# all_test[[30]] %>% group_by(prod_cd) %>% summarise(acc= mean(acc)) %>% summary()


