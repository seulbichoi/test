#---DESC.

# 1. div 변수 공백 제거 후 div 별 형태변환
# 2. col name 수정
# 3. div data frame, total data frame, inven data frame 분류 후 결측값 채움
# 4. 각 data frame RDS로 저장

#data preprocessing
time_tk_date <- readRDS("D:/2018_Rdata/time_tk/time_tk.RDS")

#apply function
tmp_div <- rbindlist(lapply(brd_list, div_df)) %>% unique() %>%
            mutate(div2 = gsub(" ", "", div2), 
                   div3 = gsub(" ", "", div3))

brd_list2 <- lapply(brd_list, divide_div)

#adjust colnames
col.ap <- c("sell_in_cnt", "etc",
            "prom_ass_exc", "prod_lack", 
            "inv_lack", "mat_postpone",
            "order_error", "master_error",
            "ass_exc", "avail_inven", "inven_cnt")


for (i in 1:length(brd_list2)) {
  colnames(brd_list2[[i]]$all)[4:14] <- col.ap
  colnames(brd_list2[[i]]$not_all)[4:15] <- c("div", col.ap)
}


#total(all) fill missing value  function

total_list <- list() # all_brand_sell_in_total
division_list <- list() # all_brand_division_sell_in_total
inven_list <- list() # all_brand_inven



for (i in 1:length(brd_list2)) {
  
  tmp_t <- brd_list2[[i]]$all %>% select(-c(avail_inven, inven_cnt))
  tmp_d <- brd_list2[[i]]$not_all %>% select(-c(avail_inven, inven_cnt))
  
  inven_list[[i]] <- rbind(brd_list2[[i]]$all %>% select(prod_cd, ymd, avail_inven, inven_cnt),
                           brd_list2[[i]]$not_all %>% select(prod_cd, ymd, avail_inven, inven_cnt)
  )
  
  total_list[[i]] <- lapply(tmp_t %>% filter(sell_in_cnt > 0) %>% .$prod_cd %>% unique(),
                            total_fill_missing,
                            df = tmp_t) %>% rbindlist
  
  temp_df <- tmp_d %>% filter(sell_in_cnt > 0) %>% select(prod_cd, div) %>% unique() %>% t() %>%
    as.data.frame() %>% as.list()
  
  division_list[[i]] <- lapply(temp_df,
                               div_fill_missing,
                               df = tmp_d) %>% rbindlist
}

except_cd <- c('11008', "22022", '11000', '11157')

total_list <- total_list %>% rbindlist %>%
  filter(!(substr(prod_cd, 1, 5) %in% except_cd),!grepl("수출", prod_nm))

division_list <- division_list %>% rbindlist %>%
  filter(!(substr(prod_cd, 1, 5) %in% except_cd),!grepl("수출", prod_nm))

inven_list <- inven_list %>% rbindlist

##saveRDS
saveRDS(total_list, ".RDS")
saveRDS(division_list, ".RDS")
saveRDS(inven_list, ".RDS")
