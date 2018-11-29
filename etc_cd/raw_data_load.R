#raw data load

require(readr) #read_csv
require(data.table) #rbindlist()
require(plyr)
require(dplyr) # select
require(reshape2)
require(readxl)
require(tidyr) # fill

dir = "D:/2018_proj/order_raw/" # + /hera, /sulwhasoo, /primera, /liri, /ap folder
brd <- c("v","p","l","a") # data folder name
brd <- "s"
brd <- "h"
###############################################################################################
### fill NA ->0 
###############################################################################################
na.fill <- function(df) { 
  df <- as.data.frame(df)
  for( i in 4:ncol(df) ){
    
    df[,i] <- ifelse(is.na(df[,i]) , 0, df[,i] )
  }
  return(df)
}
###############################################################################################
#### AP change colnm
###############################################################################################
ap_load<- function(file) {
  
  ap_dt <- read_excel(file, 
    sheet = 'Sheet1',
    col_names = FALSE,
    trim_ws = T)
  if(!is.na(ap_dt[2,1])){
    ap_dt[1, which(is.na(ap_dt[1,])) ] <- "전체결과"
    colnames(ap_dt) <-
      c("prod_cd",
        "prod_nm",
        "ymd",
        ap_dt[1, -c(1:3)] %>% paste0("|",ap_dt[2, -c(1:3)]) %>% trimws())
    ap_dt <- ap_dt[-c(1:2),]  %>% na.fill()
    for(j in 4:ncol(ap_dt)){
      ap_dt[,j] <- as.numeric(ap_dt[,j]) 
    }
    ap_dt <- ap_dt %>%
      as.data.table %>% 
      melt.data.table(id.vars = c("prod_cd", "prod_nm", "ymd")) %>% 
      mutate(variable = as.character(variable))
    return(ap_dt) 
    
  } else
    ap_dt[1, which(is.na(ap_dt[1,])) ] <- "전체결과"
  colnames(ap_dt) <-
    c("prod_cd",
      "prod_nm",
      "ymd",
      ap_dt[1, -c(1:3)] %>% paste0("|",ap_dt[2, -c(1:3)]) %>% trimws())
  ap_dt <- ap_dt[-c(1:3),]  %>% na.fill() 
  for(j in 4:ncol(ap_dt)){
    ap_dt[,j] <- as.numeric(ap_dt[,j]) 
  }
  ap_dt <- ap_dt %>%
    as.data.table %>% 
    melt.data.table(id.vars = c("prod_cd", "prod_nm", "ymd")) %>% 
    mutate(variable = as.character(variable))
  return(ap_dt) 
}
###############################################################################################
brd_list <- list()
for(i in 1:length(brd)){
  lf <- paste0(dir,brd[i],"/",list.files(paste0(dir, brd[i])))
  df_list <- lapply(lf, ap_load)
  brd_list[[i]] <- rbindlist(df_list)
  rm(df_list)
  colnames(brd_list[[i]])[4:5] <- c("div","cnt")
}
save(brd_list, file="D:/2018_proj/R_proj/order_raw_org/raw_load_brd_h.Rdata")

