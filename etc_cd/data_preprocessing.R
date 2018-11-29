#data preprocessing

require(readr) #read_csv
require(data.table) #rbindlist()
require(plyr)
require(dplyr) # select
require(reshape2)
require(readxl)
require(tidyr) # fill
require(lubridate)
load("C:/2018_Rdata/time_tk/time_tk_0614.RData")

brd <- c("v","p","l","a") # data folder name
brd <- "s"
brd <- "h"
###############################################################################################
### div word spacing remove 
###############################################################################################
div_df <- function(bind_df){
  df <- data.frame(div = unique(bind_df$div), div2 = NA, div3 = NA ) %>% 
    mutate(div = as.character(div))
  
  for(i in 1:nrow(df)){
    df$div2[i] <- unlist(strsplit(df$div[i], "\\|"))[1]
    df$div3[i] <- unlist(strsplit(df$div[i], "\\|"))[2]
  }
  return(df)
}
###############################################################################################
#divide all, not_all
###############################################################################################
divide_div <- function(df){
  df1 <- df %>% 
    filter(grepl("전체", div)) %>% 
    left_join(tmp_div,by="div") %>% 
    select(-div) %>% 
    dcast(prod_cd+prod_nm+ymd+div2~div3, value.var = "cnt") %>% 
    select(-div2) 
  
  df2 <- df %>% 
    filter(!grepl("전체", div)) %>% 
    left_join(tmp_div,by="div") %>% 
    select(-div) %>% 
    dcast(prod_cd+prod_nm+ymd+div2~div3, value.var = "cnt")
  
  return(list(all = df1, not_all = df2))
}
###############################################################################################
#function apply
###############################################################################################
tmp_div <- rbindlist(lapply(brd_list, div_df)) %>% 
  unique() %>% 
  mutate(div2 = gsub(" ", "", div2), div3 = gsub(" ", "", div3))

brd_list2 <- lapply(brd_list, divide_div)

###############################################################################################
#adjust colnames
###############################################################################################
for(i in 1:length(brd_list2)){
  colnames(brd_list2[[i]]$all)[4:14] <- c("sell_in_cnt", 
    "etc" ,
    "prom_ass_exc",
    "prod_lack",
    "inv_lack",
    "mat_postpone",
    "order_error",
    "master_error",
    "ass_exc",
    "avail_inven", 
    "inven_cnt"
  ) 
  
  colnames(brd_list2[[i]]$not_all)[4:15] <- c("div",
    "sell_in_cnt", 
    "etc" ,
    "prom_ass_exc",
    "prod_lack",
    "inv_lack",
    "mat_postpone",
    "order_error",
    "master_error",
    "ass_exc",
    "avail_inven", 
    "inven_cnt"
  ) 
}
###############################################################################################
#fill missing value setting & divide inven
###############################################################################################
rm(brd_list)

for(i in 1){
  assign(paste0(brd[i],"_t"), brd_list2[[i]]$all %>% select(-c(avail_inven, inven_cnt))) 
  assign(paste0(brd[i],"_d"), brd_list2[[i]]$not_all %>% select(-c(avail_inven, inven_cnt)))
  assign(paste0(brd[i],"_i"),
    rbind(
      brd_list2[[i]]$all %>% 
        select(prod_cd, ymd, avail_inven, inven_cnt) %>% 
        mutate(div=NA), 
      brd_list2[[i]]$not_all %>% 
        select(prod_cd, ymd, div, avail_inven, inven_cnt)))
}

###############################################################################################
#total df fill missing value  function
###############################################################################################
total_fill_missing <- function(i,df){
  tmp <- df %>% filter(prod_cd == i) %>% mutate(ymd = as.Date(ymd))
  min_date = min(tmp$ymd) ; max_date = max(tmp$ymd)
  tmp_date <- time_tk_date %>%
    filter(ymd >= min_date , ymd <= max_date)
  tmp <- tmp %>%
    right_join(tmp_date) %>%
    fill(prod_cd) %>%
    fill(prod_nm) %>% 
    replace(., is.na(.), 0)
  return(tmp)
}


h_t <- lapply(h_t %>% filter(sell_in_cnt>0) %>% .$prod_cd %>% unique(), total_fill_missing, df = h_t)
h_t <- rbindlist(h_t)

s_t <- lapply(s_t %>% filter(sell_in_cnt>0) %>% .$prod_cd %>% unique(), total_fill_missing, df = s_t)
s_t <- rbindlist(s_t)

p_t <- lapply(p_t %>% filter(sell_in_cnt>0) %>% .$prod_cd %>% unique(), total_fill_missing, df = p_t)
p_t <- rbindlist(p_t)

l_t <- lapply(l_t %>% filter(sell_in_cnt>0) %>% .$prod_cd %>% unique(), total_fill_missing, df = l_t)
l_t <- rbindlist(l_t)

a_t <- lapply(a_t %>% filter(sell_in_cnt>0) %>% .$prod_cd %>% unique(), total_fill_missing, df = a_t)
a_t <- rbindlist(a_t)

v_t <- lapply(v_t %>% filter(sell_in_cnt>0) %>% .$prod_cd %>% unique(), total_fill_missing, df = v_t)
v_t <- rbindlist(v_t)






###############################################################################################
#div df fill missing value function
###############################################################################################
div_fill_missing <- function(i,df){
  tmp <-df %>% filter(prod_cd == i[1] , div == i[2]) %>% mutate(ymd = as.Date(ymd))
  min_date = min(tmp$ymd) ; max_date = max(tmp$ymd)
  tmp_date <- time_tk_date %>%
    filter(ymd >= min_date , ymd <= max_date)
  tmp <- tmp %>%
    right_join(tmp_date) %>%
    fill(prod_cd, prod_nm, div) %>%
    replace(., is.na(.), 0)
}
###############################################################################################
h_d <- lapply(h_d %>% 
    filter(sell_in_cnt>0) %>% 
    select(prod_cd, div) %>% 
    unique() %>% 
    t() %>%
    as.data.frame() %>%
    as.list(),
  div_fill_missing, 
  df=h_d)

h_d <- rbindlist(h_d)

s_d <- lapply(s_d %>% 
    filter(sell_in_cnt>0) %>% 
    select(prod_cd, div) %>% 
    unique() %>% 
    t() %>%
    as.data.frame() %>%
    as.list(),
  div_fill_missing, 
  df=s_d)

s_d <- rbindlist(s_d)

p_d <- lapply(p_d %>% 
    filter(sell_in_cnt>0) %>% 
    select(prod_cd, div) %>% 
    unique() %>% 
    t() %>%
    as.data.frame() %>%
    as.list(),
  div_fill_missing, 
  df=p_d)

p_d <- rbindlist(p_d)

l_d <- lapply(l_d %>% 
    filter(sell_in_cnt>0) %>% 
    select(prod_cd, div) %>% 
    unique() %>% 
    t() %>%
    as.data.frame() %>%
    as.list(),
  div_fill_missing, 
  df=l_d)

l_d <- rbindlist(l_d)

a_d <- lapply(a_d %>% 
    filter(sell_in_cnt>0) %>% 
    select(prod_cd, div) %>% 
    unique() %>% 
    t() %>%
    as.data.frame() %>%
    as.list(),
  div_fill_missing, 
  df=a_d)

a_d <- rbindlist(a_d)

v_d <- lapply(v_d %>% 
    filter(sell_in_cnt>0) %>% 
    select(prod_cd, div) %>% 
    unique() %>% 
    t() %>%
    as.data.frame() %>%
    as.list(),
  div_fill_missing, 
  df=v_d)

v_d <- rbindlist(v_d)

save(
  liricos_t,primera_t,sulwhasoo_t,ap_t,
  file="D:/2018_proj/R_proj/order_raw_org/spla_total.Rdata")

save(
  liricos_d,primera_d,sulwhasoo_d,ap_d,
  file="D:/2018_proj/R_proj/order_raw_org/spla_div.Rdata")

save(
  liricos_i,primera_i,sulwhasoo_i,hera_i,vb_i,ap_i,
  file="D:/2018_proj/R_proj/order_raw_org/all_brand_inven.Rdata")

save(
  h_t,
  file="D:/2018_proj/R_proj/order_raw_org/s_total.Rdata")

save(
  h_d,
  file="D:/2018_proj/R_proj/order_raw_org/s_div.Rdata")

save(
  h_i,
  file="D:/2018_proj/R_proj/order_raw_org/s_inven.Rdata")


