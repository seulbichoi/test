head(p_list[[1]])

a <- primera %>% select(div) %>% unique()
b <- p_i %>% select(div) %>% unique()
a <- hera %>% select(div) %>% unique()
b <- h_i %>% select(div) %>% unique()




p_i %>% select(prod_cd,div) %>% group_by(prod_cd,div) %>% summarise()


a <- data.frame(a, sale_div = 1)
b <- data.frame(b, inv_div = 1)


aa <- full_join(x=a,y=b)
aa$inv_div <- ifelse(is.na(aa$inv_div), 0,1)
