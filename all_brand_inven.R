require(plyr)
require(dplyr)
require(lubridate)
require(readr)
require(data.table) #rbindlist()
setwd("C:/Users/ms/Desktop/inven")
lf <- list.files()

total <- list()

for(i in 1:length(lf))
{
  total[[i]] <- read_csv(lf[i])
}

total_df <- rbindlist(total)

for(i in 1:57){
  print(head(total[[i]]))  
  print(i)
}

names(total_df) <- c("brand_grp", "prod_cd", "prod_nm", "div", "type", "date", "inven_cnt")

h_i <- total_df %>% filter(brand_grp == "헤라")
s_i <- total_df %>% filter(brand_grp == "설화수")
p_i <- total_df %>% filter(brand_grp == "프리메라")
v_i <- total_df %>% filter(brand_grp == "바이탈뷰티")

rm(hera_list, primera_list, sulwha_list, vb_list)

