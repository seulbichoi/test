## 초도주 만들기 ver 2

######초도주 만들기
require(dplyr)
# Time Series#########
require('timetk')
require('tidyquant')
require('tibbletime')

###########################################
# pkg_ls <- c('timetk','tibbletime', 'plyr', 'dplyr', 'tidyquant','tibble')
# if(pkg_ls %in% rownames(installed.packages()) == FALSE) {
#   install.packages(pkg_ls)
# }

chodo_day <- function( start = '2013-01-01', end = '2020-12-31') {
  
  dt_list <- c(as.Date(start):as.Date(end)) %>% as.Date(origin= '1970-01-01') 
  # dt_list <- as.data.frame(dt_list)
  
  chodo1 <-
    tibbletime::as_tbl_time(as.data.frame(dt_list), index = dt_list) %>%
    timetk::tk_augment_timeseries_signature()
  
  ###########################
  # case 1 :: 1일이 토 / 일 일때
  
  cd_holy_wk <- chodo1[chodo1$day == 1 & chodo1$wday %in% c(1, 7), ] %>% 
    mutate(week.iso = week.iso + 1) %>%
    select(year.iso, week.iso)
  
  #### W  주차
  cd_holy <- inner_join(chodo1, cd_holy_wk) %>%
    filter(wday %in% c(2:6)) %>%
    select(1) %>% mutate(chodo_weight = 2) %>% as.data.frame
  
  #### w-1 주차 : 목(5),금(6),토(7),일(1)  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
  cd_holy1 <- inner_join(chodo1, cd_holy_wk %>% mutate(week.iso = week.iso - 1)) %>%
    filter(wday %in% c(1,5:7 )) %>%
    select(1) %>% 
    mutate(chodo_weight = 1) %>% as.data.frame
  
  cd_holy_day <- full_join(cd_holy, cd_holy1) %>% arrange(dt_list)
  
  ##################################################################################################
  # case 2 : 1일이 월요일일때  #####################################################################
  #  화요일 --> 해당주 전부 + 전주 목(5) , 금(6) (7) , 1 까지
  ##################################################################################################
  
  cd_mon_wk <- chodo1 [chodo1$day == 1 & chodo1$wday == 2, c('year.iso','week.iso')] 
  
  cd_mon <- inner_join(chodo1, cd_mon_wk) %>% filter(wday %in% c(2:6)) %>%
    select(1) %>% 
    mutate(chodo_weight = 2) %>% as.data.frame
  cd_mon1 <- inner_join(chodo1, cd_mon_wk %>% mutate(week.iso = week.iso - 1)) %>% filter(wday %in% c(1,5:7 )) %>%
    select(1) %>% 
    mutate(chodo_weight = 1) %>% as.data.frame
  
  cd_mon_day <- full_join(cd_mon, cd_mon1) %>% arrange(dt_list)
 
  ################################
  # case 3 : 1일이 화요일일때    #
  # ###  1 : 화요일 --> 해당주 전부 + 전주 금 까지
  ################################
  
  cd_tue_wk <-
    chodo1[chodo1$day == 1 & chodo1$wday == 3, ] %>%
    mutate(week.iso = week.iso) %>%
    select(year.iso, week.iso) 
  
  
  cd_tue <- inner_join(chodo1, cd_tue_wk) %>% filter(wday %in% c(2:6)) %>%
    select(1) %>% 
    mutate(chodo_weight = 2) %>% as.data.frame
  
  #### w-1 주차 : 금(6),토(7),일(1)  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
  cd_tue1 <- inner_join(chodo1, cd_tue_wk %>% mutate(week.iso = week.iso - 1)) %>%
    filter(wday %in% c(1,6:7 )) %>%
    select(1) %>% 
    mutate(chodo_weight = 1) %>% as.data.frame
  
  cd_tue_day <- full_join(cd_tue, cd_tue1) %>% arrange(dt_list)
  
  
  
  
  
  ##############################
  ##### case 4 :: 평일일 때(수~금) ######
  ##############################
  cd_work_wk <- chodo1[chodo1$day == 1 & chodo1$wday %in% c(4:6), ] %>%
    select(year.iso, week.iso)
  
  cd_work_day <- inner_join(chodo1, cd_work_wk) %>%
    dplyr::filter(wday %in% c(2:6)) %>%
    arrange(dt_list) %>% select(1) %>% 
    mutate(chodo_weight = 1) %>% as.data.frame
  
  
  cd_list <- bind_rows(cd_holy_day,cd_mon_day,cd_tue_day,cd_work_day) %>% full_join(as.data.frame(dt_list)) %>% arrange(dt_list)
  cd_list[is.na(cd_list$chodo_weight),'chodo_weight'] <- 0
  # View(cd_list)
  return (cd_list)
}

chodo_day()
