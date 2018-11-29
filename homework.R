#### install package ####
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("")
# install.packages("")

library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
#### data import ####

## making directory as an object
sb_dir <- c("C:/Users/Choiseulbi/Desktop/AP/추출데이터/order_total_real/hera/")

## listing up name of files in the diretory
sb_file <- list.files(sb_dir)

## counting number of files in the directory
sb_file_cnt <- length(sb_file)

## making list
asd <- list()
sub <- list()
seoul11 <- list()
pusan26 <- list()
daegu27 <- list()
tt <- list()
name_list <- substr(sb_file, 1,9)

## read table one by one automatically, using loop program
for (i in 1:length(sb_file)) {
  asd[[i]] <-fread(paste0(sb_dir, sb_file[i]), header = T)
}

## change list, column name
names(ad) <- substr(sb_file,1,9)
column_list <- names(asd[[1]])

## selecting column
for (i in 1:8) {
  names(asd[[i]]) <- column_list
  sub[[i]] <- asd[[i]][,6:9]
}

## filtering column, create new column 'date'
for (i in 1:8) {
  sub[[i]] <- filter(sub[[i]], SIDO %in% c(11, 26, 27), DSBJT_CD == 12, FORM_CD == 3)
}

## create new column 'date'
for (i in 1:8) {
  sub[[i]]$date <- as.Date(as.character(sub[[i]]$RECU_FR_DT), format = "%Y%m%d")
}

## filtering (seoul, pusan, daegu)
for (i in 1:8) {
  seoul11[[i]] <- filter(sub[[1]], SIDO == 11)
  pusan26[[i]] <- filter(sub[[i]], SIDO == 26)
  daegu27[[i]] <- filter(sub[[i]], SIDO == 27)
}







#paste0("nhis_", substr(name_list[1], 6,9))
age<- data.frame(sub[[1]])

## grouping and summarise

nhis2008 <- group_by(nhis2008, date, SIDO)

nhis2008 <- summarise(nhis2008, count = n())



# 바이탈 뷰티 연도별 주문량 살펴보기 - 바이탈 뷰티 제품 파악하기

vb %>% select(prod_cd) %>% unique() %>% nrow() #249개

vb$prod_cd <- as.character(vb$prod_cd)
vb_yearly <- vb %>% group_by(prod_cd, prod_nm, date, order_total) %>% summarise()


summary(vb_yearly)

vb_yearly$order_total[is.na(vb_yearly$order_total)] <-0


vb_list <- list()
vb_name <- unique(vb_yearly$prod_cd)

# 나중에 시간 체크하기
system.time(for (i in 1:length(vb_name)) {
  vb_list[[i]] <- vb_yearly %>% filter(prod_cd == vb_name[i])
})
View(vb_list[[3]])

for (i in 1:length(vb_name)) {
  ggplot(data = vb_list[[i]], aes(x = date, y = order_total)) + geom_line() +
    labs(title = vb_list[[i]]$prod_nm[1])#facet_grid(year(date)~.)
  
  ggsave(
    file = paste0
    (
      "C:/Users/Choiseulbi/Desktop/AP/vb_plot/",
      "vbplot_",
      vb_list[[i]]$prod_cd[1],
      ".png"
    )
  )

}

for (i in 1: length(total_list)) {
  print(total_list[[i]] %>% select(prod_cd) %>% unique() %>% nrow())
}







