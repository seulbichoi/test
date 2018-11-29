require(readr) #read_csv
require(data.table) #rbindlist()
require(plyr)
require(dplyr) # select
setwd("C:/Users/ms/Desktop/")
colnm<-read.csv("colname.csv")


#4,5,9,10

################################################
setwd("C:/Users/ms/Desktop/primera")
primera_list <- list()
lf <- list.files()
for(i in 1:length(lf))
{
  primera_list[[i]] <- read_csv(lf[i])  
}
primera <- rbindlist(primera_list)
names(primera)<-colnames(colnm)
primera <- primera[!is.na(primera$date),]
primera <- primera %>% dplyr::select(-c(4,5,9,10)) %>% mutate(prod_cd=as.character(prod_cd))

#nrow(primera) == sum((primera$order_vol=primera$set_vol)==0)
a<-primera[-which(primera$order_vol==primera$set_vol+primera$lack_vol),]


View(a)
nrow(a)
################################################

################################################
setwd("C:/Users/ms/Desktop/hera")
hera_list <- list()
lf <- list.files()
for(i in 1:length(lf))
{
  hera_list[[i]] <- read_csv(lf[i])  
}


hera %>% select(div) %>% unique()

hera <- rbindlist(hera_list)
names(hera)<-colnames(colnm)
hera<- hera[!is.na(hera$date),]
hera <- hera %>% dplyr::select(-c(4,5,9,10)) %>% mutate(prod_cd=as.character(prod_cd))
hera$orderset_per <- as.numeric(gsub(" %", "",hera$orderset_per)) # only hera % remove 
a<-hera[-which(hera$order_vol==hera$set_vol+hera$lack_vol),]
View(a)


################################################

################################################
setwd("C:/Users/ms/Desktop/vb")
vb_list <- list()
lf <- list.files()
for(i in 1:length(lf))
{
  vb_list[[i]] <- read_csv(lf[i])  
}
vb <- rbindlist(vb_list)
names(vb)<-colnames(colnm)
vb <- vb[!is.na(vb$date),]
vb <- vb %>% dplyr::select(-c(4,5,9,10)) %>% mutate(prod_cd=as.character(prod_cd))
#nrow(vb) == sum((vb$order_vol=vb$set_vol)==0)
#a<-vb[-which(vb$order_vol==vb$set_vol+vb$gyeolpum),]
#View(a)
################################################

################################################
setwd("C:/Users/ms/Desktop/sulwha")
sulwha_list <- list() 
lf <- list.files()
for(i in 1:length(lf))
{
  sulwha_list[[i]] <- read_csv(lf[i])  
}
sulwha <- rbindlist(sulwha_list)
names(sulwha)<-colnames(colnm)
sulwha <- sulwha[!is.na(sulwha$date),]
sulwha <- sulwha %>% dplyr::select(-c(4,5,9,10)) %>% mutate(prod_cd=as.character(prod_cd))
#a<-sulwha[-which(sulwha$order_vol==sulwha$set_vol+sulwha$gyeolpum),]
#View(a)
################################################


















################################################
setwd("C:/Users/ms/Desktop/inven2")
inven_list <- list()
lf <- list.files()
for(i in 1:length(lf))
{
  inven_list[[i]] <- read_csv(lf[i])  
}

for(i in 2:length(inven_list)) {
  
  for (j in 1: ncol(inven_list[[i]])) {
    inven_list[[i]] <- inven_list[[i]] [inven_list[[i]][,j]!="°á°ú",]
    inven_list[[i]][,2] <- as.character(inven_list[[i]][,2])
  }
    
}





