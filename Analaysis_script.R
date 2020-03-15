#SOURCE SCRIPTS INCLUDING RECODING, FUNCTIONS AND OTHER
source("setup.R")
source("Functions.R")
source("Import Datasets.R")
source("Recoding.R")

#export_names_select_multiple(dummy)

#DELETE ALL VARIABLES WITH "OTHER" AS IT DISTORTS FINAL OUTPUT
#!!!!TO BE DELETED IN FINAL SCRIPT
#raw_data_first[c(which(endsWith(names(raw_data_first), "other")))] <- NULL
raw_data_first$intent_town_week <- NULL
raw_data_first$intent_town <- NULL
raw_data_first$intent_district <- NULL
raw_data_first$priority_need <- NULL
raw_data_first$difficulies_border_cross <- NULL
raw_data_first$difficulties <- NULL
raw_data_first$item_bring <- NULL

#RECODE VULNERABLE GROUPS FROM NA TO 0 WHERE NO GROUP MEMBER AFFECTED
#AND CALCULATE GROUP SIZE
raw_data_first <- recode_raw(raw_data_first)

#CREATEA A DATAFRAME WITH CALCULATED POPULATION AVERAGES
pop_group_averages <- calc_avgs(raw_data_first)


#CREATE LIST OF RESULTS
list_of_results <-  from_analysisplan_map_to_output(raw_data_first, analysisplan = dap,
                                                    weighting = NULL,
                                                    cluster_variable_name = NULL,
                                                    questionnaire = NULL, confidence_level = 0.9)

#SUMMARY STATS LIST
summary.stats.list <- list_of_results$results %>% 
  lapply(function(x){map_to_labeled(result = x, questionnaire = questionnaire)})

#SUMMARY STATS LIST EXPORT
summary.stats.list %>% 
  resultlist_summary_statistics_as_one_table %>% 
  map_to_file(".summary_stats.csv")


#SUMMARY STATS LIST FORMATTED WITH p-VALUES
summary.stats.list %>%
  lapply((map_to_labeled),questionnaire) %>%
  lapply(result_format_numbers) %>% 
  lapply(add_p_value_to_summary_table) %>% 
  resultlist_summary_statistics_as_one_table %>%
  
write.csv('Output/summary_stats_formatted_with_pvalues.csv')
#browseURL("summary_stats_formatted_with_pvalues.csv")


#SUMMARY STATS LIST FORMATTED
summarystats <- summary.stats.list %>%
  lapply((map_to_labeled),questionnaire) %>% 
  resultlist_summary_statistics_as_one_table 

write.xlsx(summarystats,'Output/summarystats.xlsx')

#RESHAPE SUMMARY STATS
summarystats <- reshape_summstats(summarystats)
names_dummy <- as.data.frame(names(summarystats))
write.xlsx(names_dummy, "Output/cleaned_based_summarystats_names.xlsx")

#CALCULATE TIME VARIABLES (MORE THAN 8 HOURS) FOR INDESIGN
summarystats <- calc_time(summarystats)

#EXTRACT VARIABLE NAMES FOR THE AGGREGATION TABLE
#aggtable_names <- aggtable_names(summarystats)
#ranked_names_aggtable <- read.csv("output/aggtable_names_rank.csv")

#EXTRACT RELEVANT VARIABLES FOR INDESIGN
extract_indesign <- extract_indesign(summarystats)

#EXTRACT RELEVANT VARIABLES FOR AGGREGATE TABLES
#extract_aggtable <- extract_aggtable(summarystats)

#SELECT AND MERGE TOP FOUR FOR RELEVANT VARIABLES INCLUDED IN INDESIGN
district_origin<- select_top_four_character(summarystats,"district_origin_")
difficulties<- select_top_four_numeric(summarystats,"difficulties.")
transport<- select_top_four_character(summarystats,"transport_")
reasondisplace<- select_top_four_character(summarystats,"reason_displace_")
itemsbrought<- select_top_four_numeric(summarystats,"item_bring.")
priorityneeds<- select_top_four_numeric(summarystats,"priority_need")
intentiondays<- select_top_four_character(summarystats,"intent_two_days")
intentionweeks<- select_top_four_character(summarystats,"intent_two_weeks")
#MERGE ALL TOP 4 INDICATORS TOGETHER AND CHANGE THE HEADER NAMES
mergetop <-cbind(district_origin, difficulties, transport, reasondisplace, itemsbrought,
              priorityneeds, intentiondays, intentionweeks)
rownames(mergetop) <- c()
namevector<-as.vector(sapply(mergetop[1,], paste0, collapse=""))
names(mergetop)<-paste(names(mergetop[1,]), namevector, sep = "_")
mergetop<-mergetop[-1,]

#MERGE BOTH DATAFRAMES TOGETHER FOR INDESIGN
indesign_full<-cbind(pop_group_averages, extract_indesign, mergetop)
#write.csv(summarystats, "output/summarystats_final_border_2.csv")
write.csv(indesign_full, "output/indesign_full_border_2.csv")

