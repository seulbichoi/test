require(plyr)
require(dplyr)
require(lubridate)
colnames(hera)
head(hera)
total_list <- list()
total_list[[1]] <- hera ; total_list[[2]] <- sulwha ; total_list[[3]] <- primera ; total_list[[4]] <- vb

sub_list <- list()


# 연도별 sku
for( i in 1:4) {
  print(total_list[[i]] %>% filter(year(date)==2014) %>% select(prod_cd) %>% unique() %>% nrow())
  print(total_list[[i]] %>% filter(year(date)==2015) %>% select(prod_cd) %>% unique() %>% nrow())
  print(total_list[[i]] %>% filter(year(date)==2016) %>% select(prod_cd) %>% unique() %>% nrow())
  print(total_list[[i]] %>% filter(year(date)==2017) %>% select(prod_cd) %>% unique() %>% nrow())
  print(total_list[[i]] %>% filter(year(date)==2018) %>% select(prod_cd) %>% unique() %>% nrow())
  print("-")
}


a<-primera[-which(primera$order_vol==primera$set_vol+primera$lack_vol),]


primera %>% filter( order_vol==(set_vol+lack_vol))


# 연월별 주문, 확정 수량 (NA 제거)
for( i in 1:4) {
  sub_list[[i]] <- total_list[[i]]  %>% group_by(year(date),month(date)) %>% summarise(sum(order_vol,na.rm=T)) 
    print(sum(is.na(order_sum[[i]])))
}

# 연월별 레벨, 카테고리별 주문건수
for( i in 1:4) {
  sub_list[[i]] <- total_list[[i]]  %>% group_by(year(date),prod_cd,prod_level,cat4) %>% summarise(cnt=n())
}

# 브랜드 별 제품 레벨 
for(i in 1:4){
  print(total_list[[i]] %>% group_by(prod_level,prod_cd) %>% summarise() %>% unique() %>% group_by(prod_level) %>% summarise(cnt=n()))  
}

# 브랜드 별 div 별 주문수
for(i in 1:4){
  sub_list[[i]] <- total_list[[i]] %>% select(div,order_vol) %>% group_by(div) %>% summarise(sum(order_vol,na.rm=T))
}
sub_total <- list()
sub_total <- rbindlist(sub_list)

# 브랜드별 ad_new == " 지정되지 않음 " 수 
hera %>% filter(ad_new=="지정되지 않음") %>% select(prod_cd) %>% unique() %>% nrow() # 67
primera %>% filter(ad_new=="지정되지 않음") %>% select(prod_cd) %>% unique() %>% nrow() # 65
sulwha %>% filter(ad_new=="지정되지 않음") %>% select(prod_cd) %>% unique() %>% nrow() # 47
vb %>% filter(ad_new=="지정되지 않음") %>% select(prod_cd) %>% unique() %>% nrow() # 26

# 중복 없음
hera %>% unique() %>% nrow() - hera %>% nrow() # 0
sulwha %>% unique() %>% nrow() - sulwha %>% nrow() # 0
primera %>% unique() %>% nrow() - primera %>% nrow() # 0
vb %>% unique() %>% nrow() - vb %>% nrow() # 0


primera %>% group_by(div,order_vol) %>% sum() group_by(div) %>%  


primera %>% select(prod_level,prod_cd)  %>% unique() %>% group_by(prod_cd) %>% summarise(cnt=n()) %>% filter(cnt >1)



hera %>% select(div, cat4, order_vol) %>% group_by(div,cat4) %>% summarise() %>% group_by(div) %>% summarise(cnt=n())
sulwha %>% select(div, cat4, order_vol) %>% group_by(div,cat4) %>% summarise() %>% group_by(div) %>% summarise(cnt=n())
primera %>% select(div, cat4, order_vol) %>% group_by(div,cat4) %>% summarise() %>% group_by(div) %>% summarise(cnt=n())
vb %>% select(div, cat4, order_vol) %>% group_by(div,cat4) %>% summarise() %>% group_by(div) %>% summarise(cnt=n())

hera %>% select(div, cat4, order_vol) %>% group_by(div,cat4) %>% summarise(sum(order_vol)) %>% View()
sulwha %>% select(div, cat4, order_vol) %>% group_by(div,cat4) %>% summarise(sum(order_vol)) #%>% group_by(div) %>% summarise(cnt=n())
primera %>% select(div, cat4, order_vol) %>% group_by(div,cat4) %>% summarise(sum(order_vol)) #%>% group_by(div) %>% summarise(cnt=n())
vb %>% select(div, cat4, order_vol) %>% group_by(div,cat4) %>% summarise(sum(order_vol)) #%>% group_by(div) %>% summarise(cnt=n())

hera %>% filter(div=="지정되지 않음")  %>% select(prod_level,prod_cd) %>% unique %>% group_by(prod_level) %>% summarise(cnt=n())
primera %>% filter(div=="지정되지 않음")  %>% select(prod_level,prod_cd) %>% unique %>% group_by(prod_level) %>% summarise(cnt=n())
sulwha %>% filter(div=="지정되지 않음")  %>% select(prod_level,prod_cd) %>% unique %>% group_by(prod_level) %>% summarise(cnt=n())
vb %>% filter(div=="지정되지 않음")  %>% select(prod_level,prod_cd) %>% unique %>% group_by(prod_level) %>% summarise(cnt=n())





