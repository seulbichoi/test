require(readr) #read_csv
require(data.table) #rbindlist()
require(plyr)
require(dplyr) # select
library(readr)
ap_dt <- read_csv("C:/Users/ms/Desktop/date.csv", 
                 col_types = cols(date = col_date(format = "%F")))


###
prod <- sulwha %>% group_by(prod_cd) %>% summarise(min_dt = min(date),
                                                   max_dt = max(date))
prod_list<-list()
for(i in 1:nrow(prod))
{
  prod_list[[i]] <- sulwha %>% filter(prod_cd ==  prod$prod_cd[i])
  a <- ap_dt %>% filter(   date>=min(prod$min_dt[i]), date<=max(prod$max_dt[i]))  
  prod_list[[i]] <- right_join(prod_list[[i]],a)
  for(j in 1:6)
  {
    prod_list[[i]][,j]<-prod_list[[i]][1,j]
  }
}
s_list <- prod_list 
s_list[[1]]
#sulwha %>% group_by(prod_cd,ad_new) %>% summarise() %>% group_by(prod_cd) %>% summarise(cnt = n()) %>% filter( cnt > 1)
###


###
prod <- primera %>% group_by(prod_cd) %>% summarise(min_dt = min(date),
                                                   max_dt = max(date))
prod_list<-list()

for(i in 1:nrow(prod))
{
  prod_list[[i]] <- primera %>% filter(prod_cd ==  prod$prod_cd[i])
  a <- ap_dt %>% filter(   date>=min(prod$min_dt[i]), date<=max(prod$max_dt[i]))  
  prod_list[[i]] <- right_join(prod_list[[i]],a)
  for(j in 1:6)
  {
    prod_list[[i]][,j]<-prod_list[[i]][1,j]
  }
}
p_list <- prod_list 
###


###
prod <- vb %>% group_by(prod_cd) %>% summarise(min_dt = min(date),
                                                   max_dt = max(date))
prod_list<-list()
for(i in 1:nrow(prod))
{
  prod_list[[i]] <- vb %>% filter(prod_cd ==  prod$prod_cd[i])
  a <- ap_dt %>% filter(   date>=min(prod$min_dt[i]), date<=max(prod$max_dt[i]))  
  prod_list[[i]] <- right_join(prod_list[[i]],a)
  for(j in 1:6)
  {
    prod_list[[i]][,j]<-prod_list[[i]][1,j]
  }
}
v_list <- prod_list 
###


###
prod <- hera %>% group_by(prod_cd) %>% summarise(min_dt = min(date),
                                                   max_dt = max(date))
prod_list<-list()
for(i in 1:nrow(prod))
{
  prod_list[[i]] <- hera %>% filter(prod_cd ==  prod$prod_cd[i])
  a <- ap_dt %>% filter(   date>=min(prod$min_dt[i]), date<=max(prod$max_dt[i]))  
  prod_list[[i]] <- right_join(prod_list[[i]],a)
  for(j in 1:6)
  {
    prod_list[[i]][,j]<-prod_list[[i]][1,j]
  }
}
h_list <- prod_list 
###

setwd("C:/Users/Desktop")
save(list=c("h_i","h_list","hera"),file="hera_data0321.Rdata")
save(list=c("s_i","s_list","sulwha"),file="sulwha_data0321.Rdata")
save(list=c("p_i","p_list","primera"),file="primera_data0321.Rdata")
save(list=c("v_i","v_list","vb"),file="vb_data0321.Rdata")




