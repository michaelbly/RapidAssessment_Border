aggtable <- as.data.frame(t(summarystats[1,]))
aggtable <-setDT(aggtable, keep.rownames = TRUE)[]
names(aggtable)[names(aggtable)=="rn"]<- "variable"
aggtable_varnames_rank<-read.csv("Input/aggtable/aggtable_names_rank.csv")
agg_table_merged <- merge(aggtable_varnames_rank, aggtable ,by="variable")
agg_table_full <- read.xlsx("Input/aggtable/aggregation_table.xlsx")
agg_table_full <- agg_table_full[, c(2,4)]
names(agg_table_full)[names(agg_table_full)=="X2"]<- "full_variable"
names(agg_table_full)[names(agg_table_full)=="X4"]<- "variable_num"
#agg_table_merged <- merge(agg_table_merged, agg_table_full, by="variable_num")
#write.csv(agg_table_merged, "Input/aggtable/agg_table_merged.csv")


hallo <- read.xlsx("Input/aggtable/aggregation_table.xlsx")
names(hallo)[names(hallo)=="X4"]<- "variable_num"
hallo$variable_num <- ifelse(is.na(hallo$variable_num), 999, hallo$variable_num)
hallo2 <- merge(hallo, agg_table_merged, by="variable_num", all=T)
hallo2[,c(2,3,4)]<-NULL
hallo2 <- hallo2[,c("numbers", "variable_num", "variable", "X5")]
hallo2 <- hallo2[order(hallo2$X5),]
write.csv(hallo2, "Input/aggtable/numbers_export.csv")

summarystats$reason_displace_arrival_military
