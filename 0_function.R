###### FUNCTION

#### 1 . raw_data_load 
#---- na 값 처리

na.fill <- function(df) { 
  df <- as.data.frame(df)
  
  for( i in 4:ncol(df) ) {
    df[,i] <- ifelse(is.na(df[,i]) , 0, df[,i] )
  }
  
  return(df)
}

#---- SAP xlsx file load, 형변환, column 정리 

ap_load <- function(file) {
  ap_dt <- read_excel(file, sheet = 'Sheet1',
                      col_names = FALSE, 
                      trim_ws = T)
  
  
  if(!is.na(ap_dt[2, 1])) {
    
    ap_dt[1, which(is.na(ap_dt[1, ])) ] <- "전체결과"
    
    colnames(ap_dt) <- c("prod_cd", "prod_nm", "ymd",
                         ap_dt[1, -c(1:3)] %>% paste0("|",ap_dt[2, -c(1:3)]) %>% trimws()
                         )
    
    ap_dt <- ap_dt[-c(1:2), ] %>% na.fill()
    
    for(j in 4:ncol(ap_dt)) {
      ap_dt[, j] <- as.numeric(ap_dt[, j])
    }
    
    ap_dt <- as.data.table(ap_dt) %>% 
              melt.data.table(id.vars = c("prod_cd", "prod_nm", "ymd")) %>%
              mutate(variable = as.character(variable))
    
    return(ap_dt) 
    
  } else {
    
    ap_dt[1, which(is.na(ap_dt[1, ])) ] <- "전체결과"
    
    colnames(ap_dt) <- c("prod_cd","prod_nm" ,"ymd",
                         ap_dt[1, -c(1:3)] %>% paste0("|",ap_dt[2, -c(1:3)]) %>% trimws()
                         )
    
    ap_dt <- ap_dt[-c(1:3), ] %>% na.fill() 
    
    for(j in 4:ncol(ap_dt)) {
      ap_dt[, j] <- as.numeric(ap_dt[, j]) 
    }
    ap_dt <- ap_dt %>%
              as.data.table %>% 
              melt.data.table(id.vars = c("prod_cd", "prod_nm", "ymd")) %>% 
              mutate(variable = as.character(variable))
    return(ap_dt) 
  }
}



#########################################
#### 2 . data_preprocessing  
#########################################

#----.div 변수 하위 레벨 분류

div_df <- function(bind_df) {
  
  df <- data.frame(div = unique(bind_df$div), 
                   div2 = NA, 
                   div3 = NA ) %>% 
         mutate(div = as.character(div))
  
  for(i in 1:nrow(df)) {
    df$div2[i] <- unlist(strsplit(df$div[i], "\\|"))[1]
    df$div3[i] <- unlist(strsplit(df$div[i], "\\|"))[2]
  }
  
  return(df)

}


#----. div 통합과 div 별 데이터 분리

divide_div <- function(df) {
  df1 <- df %>% 
    filter(grepl("전체", div)) %>% 
    left_join(tmp_div, by = "div") %>% 
    select(-div) %>% 
    dcast(prod_cd + prod_nm + ymd + div2 ~ div3,
          value.var = "cnt") %>% 
    select(-div2) 
  
  df2 <- df %>% 
    filter(!grepl("전체", div)) %>% 
    left_join(tmp_div, by = "div") %>% 
    select(-div) %>% 
    dcast(prod_cd + prod_nm + ymd + div2 ~ div3,
          value.var = "cnt")
  return(list(all = df1, not_all = df2))
}


#----. div 통합 데이터 결측값 filling

total_fill_missing <- function(i, df) {
  
  tmp <- df %>% filter(prod_cd == i) %>% mutate(ymd = as.Date(ymd))
  
  min_date = min(tmp$ymd)
  max_date = max(df$ymd)
  
  tmp_date <- time_tk_date %>% filter(ymd >= min_date, ymd <= max_date)
  
  tmp <- right_join(tmp, tmp_date) %>% fill(prod_cd) %>% fill(prod_nm) %>% replace(., is.na(.), 0)
  return(tmp)
  
}

#----. div 별 데이터 결측값 filling

div_fill_missing <- function(i, df) {
  tmp <-df %>% filter(prod_cd == i[1], div == i[2]) %>% 
    mutate(ymd = as.Date(ymd))
  
  min_date <- min(tmp$ymd)
  max_date <- max(df$ymd)
  
  tmp_date <- time_tk_date %>%
    filter(ymd >= min_date, ymd <= max_date)
  
  tmp <- tmp %>%
    right_join(tmp_date) %>%
    fill(prod_cd, prod_nm, div) %>%
    replace(., is.na(.), 0)
}
