setwd("C:/Users/ms/Desktop/primera(ver3)")
load("D:/amore_data/ver3/total_brand.RData")

# hera, sulwha, primera, vb, lr <- Raw Data
# _2 <- cd == nm Data
# 2 <- date join & 2018 filter data
#total_list <- hera2, sulwha2, primera2, vb2, lr2 total list

require(readr) #read_csv
require(data.table) #rbindlist()
require(plyr)
require(dplyr) # select
require(reshape2)
require(ggplot2)
getwd()

lf <- list.files()
total_list <- list()

for( i in 1:length(lf)){
  total_list[[i]] <- read_csv(lf[i])
}
total <- rbindlist(total_list)
colnames(total) <- c("prod_cd", "prod_nm", "prod_use1", "prod_use2", "date", "order_pos", "order_sap", "order_total")
total2 <- total %>% mutate(prod_brd = substr(prod_cd,1,5)) %>% select(-c(3,4))

brd <- total2 %>% select(prod_brd) %>% unique()
brd2 <- brd[c(2,4,5,6,7,12,13),]

hera <- total2 %>% filter(prod_brd == brd2[1] & !grepl("11107",prod_nm) ) %>% select(1:6)
sulwha <- total2 %>% filter(prod_brd == brd2[5] & !grepl("11117",prod_nm) ) %>% select(1:6)
primera <- total2 %>% filter(prod_brd == brd2[4] & !grepl("11064",prod_nm) ) %>% select(1:6)
vb <- union(
  total2 %>% filter(prod_brd == brd2[6] & !grepl("11077",prod_nm) ) %>% select(1:6),
  total2 %>% filter(prod_brd == brd2[7] & !grepl("11077",prod_nm) ) %>% select(1:6)
  )
lr <- union(
  total2 %>% filter(prod_brd == brd2[2] & !grepl("22001",prod_nm) ) %>% select(1:6),
  total2 %>% filter(prod_brd == brd2[3] & !grepl("22001",prod_nm) ) %>% select(1:6)
)

hera_2 <- total2 %>% filter(grepl("11107",prod_nm))
sulwha_2 <- total2 %>% filter(grepl("11117",prod_nm))
primera_2 <- total2 %>% filter(grepl("11064",prod_nm))
vb_2 <- total2 %>% filter(grepl("11077",prod_nm))
lr_2 <- total2 %>% filter(grepl("22001",prod_nm))


total_list <- list()
total_list[[1]] <- hera  #11107
total_list[[2]] <- sulwha #11117
total_list[[3]] <- primera #11064
total_list[[4]] <- vb #11077
total_list[[5]] <- lr #22001



# st <- list()
# for(i in 1:5){
#   st[[i]] <- total_list[[i]] %>% select(prod_nm) %>% unique()
#   st[[i]] <- substr(st[[i]][,1],1,5) %>% unique()
#   print(total_list[[i]] %>% select(prod_nm) %>% unique() ) #%>% nrow())
#   print(sum(grepl("",total_list[[i]]$prod_nm)))
# }

for(i in 1:length(total_list)){
  prod <- total_list[[i]] %>% group_by(prod_cd) %>% summarise(min_dt = min(date),
                                                   max_dt = max(date))  
  total_list[[i]] <- left_join(hera, prod) %>% filter(max_dt > as.Date("2017-12-31"))
}

hera2 <- total_list[[1]]
sulwha2 <- total_list[[2]]
primera2 <- total_list[[3]]
vb2 <- total_list[[4]]
lr2 <- total_list[[5]]




# hera 150개 project

hera$prod_cd <- as.character(hera$prod_cd)

fc_test <- hera %>% filter(prod_cd %in% fc_list$prod_cd)


install.packages("timetk")
require(timetk)

fc_test2 <- fc_test %>% select(date) %>% unique()
idx <- tk_index(fc_test2)

fc_test3 <- tk_get_timeseries_signature(idx)


fc_test1 <- left_join(fc_test, idx, by = 'date')

setnames(fc_test3, colnames(fc_test3)[1], "date")
names(fc_test3)[1] <- "date"
fc_test_join <- fc_test %>% left_join(fc_test3)


min(fc_test_join$date)
max(fc_test_join$date)


fc_test_final$dayofweek <-
  ifelse(fc_test_final$wday.lbl == '토요일' |
           fc_test_final$wday.lbl == '일요일',
         1,
         0)

summary(fc_test_final)
fc_test_final$chodo <- as.numeric(fc_test_final$chodo)
fc_test_final$holiday <- as.numeric(fc_test_final$holiday)

fc_test_final$holiday[is.na(fc_test_final$holiday)] <- '0'


fc_test_final %>% select(dayofweek) %>% filter(dayofweek == 1) %>% nrow()


setnames(fc_test3, colnames(fc_test3)[1], "date")
names(fc_test3)[1] <- "date"


fc_test_final %>% filter(date == '2014-12-31') %>% View()

colnames(fc_test_final)

fc_test_final2 <- fc_test_final %>%
  select(
    prod_cd,
    prod_nm,
    date,
    order_total,
    year,
    year.iso,
    month,
    day,
    week.iso,
    holiday,
    chodo,
    dayofweek
  )


prod_list <- unique(fc_test_final2$prod_cd)
fc_list_final <- list()

for (i in 1:length(prod_list)) {
  fc_list_final[[i]] <-
    fc_test_final2 %>% filter(prod_cd %in% prod_list[i])
}


# prod_list <- hera 114개 제품 리스트
# fc_df <- 최종 df
# fc_list_final <- 114개 제품별 리스트
# fc_test_join <- timetk와의 조인된 df


? save.image

save(
  list = c("fc_df", "fc_list_final", "fc_test_join", "prod_list"),
  file = "fc.RData"
)
ls()[c(1, 2, 3)]
rm(list=ls()[10])

ls()



ggplot(hera, aes(x=date, y=order_total)) + geom_point()








hera %>% select(date, order_total) %>% group_by(date) %>% summarise(cnt=sum(order_total,na.rm = T)) %>% ggplot(aes(x=date,y=cnt)) + geom_line()

sulwha %>% select(date, order_total) %>% group_by(date) %>% summarise(cnt=sum(order_total,na.rm = T)) %>% ggplot(aes(x=date,y=cnt)) + geom_line()


p1<-fc_df %>% select(prod_cd) %>% unique()


p2=p1[23:33,]


?facet_grid



fc_df %>% 
  filter(prod_cd %in% p2) %>%
  select(prod_cd, date, order_total) %>% 
  group_by(prod_cd,date) %>% 
  summarise(
    s=sum(order_total)
    ) %>% 
  ggplot(
    aes(
      x=date,
      y=s,
      group=prod_cd))+
      geom_line()+facet_grid(prod_cd~., scales='free')



ls()

rm(list=ls()[c(1,2,6,7)])














