
#RECODE SELECT MULTIPLE VARIABLES FROM T/F TO BINARY VARIABLE
#raw_data_first[c(which(startsWith(names(raw_data_first), "stop_points")))] <- 
#  ifelse(raw_data_first[c(which(startsWith(names(raw_data_first), "stop_points")))] == "TRUE", 1, 0)

#raw_data_first[c(which(startsWith(names(raw_data_first), "difficulties")))] <- 
#  ifelse(raw_data_first[c(which(startsWith(names(raw_data_first), "difficulties")))] == "TRUE", 1, 0)

#raw_data_first[c(which(startsWith(names(raw_data_first), "item_bring")))] <- 
#  ifelse(raw_data_first[c(which(startsWith(names(raw_data_first), "item_bring")))] == "TRUE", 1, 0)

#raw_data_first[c(which(startsWith(names(raw_data_first), "priority_need")))] <- 
#  ifelse(raw_data_first[c(which(startsWith(names(raw_data_first), "priority_need")))] == "TRUE", 1, 0)


  
#RECODE RAW DATASET SO THAT NAs CHANGED TO 0 FOR VULNERABLE POPULATIONS AND BY CALCULATING
  #THE GROUP SIZE BASED ON THE NUMBER OF PEOPLE PER AGE GROUP
  recode_raw <- function(summs) {
    ifelse(is.na(summs[,c("unaccompanied_number", "disabled_number", 
                          "disabled_number", "pregnant_number")]), 0, 
           summs[,c("unaccompanied_number", "disabled_number", 
                    "disabled_number", "pregnant_number")])
    summs$male_0_17 <- as.numeric(as.character(summs$male_0_17))
    summs$female_0_17 <- as.numeric(as.character(summs$female_0_17))
    summs$male_18_60 <- as.numeric(as.character(summs$male_18_60))
    summs$female_18_60 <- as.numeric(as.character(summs$female_18_60))
    summs$male_60 <- as.numeric(as.character(summs$male_60))
    summs$female_60 <- as.numeric(as.character(summs$female_60))
    summs$group_size <- as.numeric(apply(summs[,c("male_0_17", 
                                                  "female_0_17", "male_18_60",
                                                  "female_18_60", "male_60", "female_60")], 
                                         1, sum))
    summs$female_total <- summs$female_18_60
    summs$minor_total <- as.numeric(apply(summs[,c("female_0_17", "male_0_17")], 
                                            1, sum))
    return(summs)
  }
  
#RECODING AND RESHAPING SUMMARY DATASET BY DELETING VARIABLES WITH MULTIPLE MENTIONS AND BY
  #MERGING THE NAME HEADER WITH THE FACTORS IN THE FIRST ROW
  reshape_summstats <- function(summs) {
    summs[, c("X", "independent.var", "independent.var.value", "se", 
              "repeat.var", "repeat.var.value")] <- NULL  
    summs <- as.data.frame(t(summs))
    names(summs) <- as.matrix(summs[1, ])
    summs <- summs[-1, ]
    summs[] <- lapply(summs, function(x) type.convert(as.character(x)))
    namesvar<-as.vector(sapply( summs[1,], paste0, collapse=""))
    names(summs) <- ifelse(namesvar=="NA", names(summs),
                                  paste(names(summs[1,]), namesvar, sep = "_"))
    summs<-summs[-1,]
    
    return(summs)
  }
  
  #CALCULATING AVERAGES (BASED ON GROUP SIZE) FOR THE VULNERABLE POPULATIONS AND FOR THE 
  #VARIOUS AGE GROUPS
  calc_avgs <- function(avg) {
    avg$disabled_number<-  ifelse(is.na(avg$disabled_number), 0, avg$disabled_number)
    avg$unaccompanied_number<-  ifelse(is.na(avg$unaccompanied_number), 0, avg$unaccompanied_number)
    avg$pregnant_number<-  ifelse(is.na(avg$pregnant_number), 0, avg$pregnant_number)
    avg$male_0_17<-  ifelse(is.na(avg$male_0_17), 0, avg$male_0_17)
    avg$male_18_60<-  ifelse(is.na(avg$male_18_60), 0, avg$male_18_60)
    avg$male_60<-  ifelse(is.na(avg$male_60), 0, avg$male_60)
    avg$female_0_17<-  ifelse(is.na(avg$female_0_17), 0, avg$female_0_17)
    avg$female_18_60<-  ifelse(is.na(avg$female_18_60), 0, avg$female_18_60)
    avg$female_18_60<-  ifelse(is.na(avg$female_18_60), 0, avg$female_18_60)
    avg$female_60<-  ifelse(is.na(avg$female_60), 0, avg$female_60)
   avg$members_no_id<-  ifelse(is.na(avg$members_no_id), 0, avg$members_no_id)
    
    subset_age <- avg[, c("male_0_17", 'male_18_60', "male_60", "female_0_17", 
                          "female_18_60", "female_60","disabled_number", "unaccompanied_number", 
                          "pregnant_number", "members_no_id", "group_size", "female_total", "minor_total")]
    subset_age %<>% mutate_if(is.character,as.numeric)
    subset_age <- as.data.frame(t(colSums(subset_age)))
    subset_age[, c("male_0_17", 'male_18_60', "male_60", "female_0_17", 
                   "female_18_60", "female_60","disabled_number", 
                   "members_no_id")] <- subset_age[, c("male_0_17", 'male_18_60', "male_60", "female_0_17", 
                                 "female_18_60", "female_60","disabled_number", 
                                "members_no_id")] / subset_age$group_size
    subset_age$pregnant_number <- subset_age$pregnant_number / subset_age$female_total
    subset_age$unaccompanied_number <- subset_age$unaccompanied_number / subset_age$minor_total
    subset_age$group_size <- NULL
    return(subset_age)
  } 
  
#CALCULATE RELEVANT TIME VARIABLES FOR INDESIGN FILE 
  #LINES BLANKED OUT WITH A # WERE NOT CHOSEN BY ANYONE IN THIS ITERATION OF THE ASSESSMENT
  calc_time <- function(avg) {
    avg$duration_rc_8_24_hours <- as.numeric(as.character(avg$duration_rc_8_24_hours))
    avg$duration_rc_two_days <- as.numeric(as.character(avg$duration_rc_two_days))
   # avg$duration_rc_three_days <- as.numeric(as.character(avg$duration_rc_three_days))
    avg$wait_border_8_24_hours <- as.numeric(as.character(avg$wait_border_8_24_hours))
    avg$wait_border_two_days <- as.numeric(as.character(avg$wait_border_two_days))
    avg$wait_border_three_days <- as.numeric(as.character(avg$wait_border_three_days))
   # avg$date_left_today <- as.numeric(as.character(avg$date_left_today))
    avg$date_left_yesterday <- as.numeric(as.character(avg$date_left_yesterday))
    avg$date_left_two_days <- as.numeric(as.character(avg$date_left_two_days))
    avg$date_left_three_days <- as.numeric(as.character(avg$date_left_three_days))
    avg$date_left_more_than_three <- as.numeric(as.character(avg$date_left_more_than_three))
    avg$date_left_week <- as.numeric(as.character(avg$date_left_week))
  #  avg$date_left_more_week <- as.numeric(as.character(avg$date_left_more_week))
    
    avg$syriaeight <- avg$wait_border_8_24_hours + avg$wait_border_two_days 
    + avg$wait_border_three_days
    avg$registeredeight <- avg$duration_rc_8_24_hours + avg$duration_rc_two_days 
  #  + avg$duration_rc_three_days
    avg$leftpie7 <- avg$date_left_week 
    # + avg$date_left_more_week
    avg$leftpie47 <- avg$date_left_more_than_three
    avg$leftpie13 <- avg$date_left_two_days + avg$date_left_three_days
    avg$leftpie1 <- avg$date_left_yesterday 
    #+ avg$date_left_today
    
    return(avg)
  }
  

#SELECT RELEVANT VARIABLES FOR INDESIGN FILE
  extract_indesign <- function(r) {
    r<-r[c("group_size", "memb_left_yes", "decision_timing_6_less", "syriaeight", "registeredeight", "leftpie1", "leftpie7", 
         "leftpie47", "leftpie13", "origin_from_location_yes", "intent_return_yes", 
         "relatives_kri_yes", "reason_intent_family_host", "reason_intent_no_choice", "reason_intent_no_options",
         "reason_intent_other")]
    write.csv(r, "output/extract_indesign.csv")
    
    return(r)
  }

  
#  "group_size", "male017avg", "male1860avg", "male60avg", "female017avg", 
#  "female1860avg", "female60avg", "unaccompanied_avg", "disabled_avg", "pregnant_avg",
#EXCLUDED FROM ABOVE SUBSET BECAUSE NOT MENTIONED IN THIS DAY'S ASSESSMENT
  #"leftpie1",           "reason_intent_no_options",
#TO BE CHANGE IN NEXT ANALAYSIS FROM TO
#"reason_intent_I do not have a choice (decided by the government);"  TO  "reason_intent_no_choice"
  
  
#EXTRACT VARIABLE NAMES FOR THE AGGREGATION TABLE
aggtable_names <- function(summarystats) {
    
r <- as.data.frame(t(summarystats[1,]))
r<- setDT(r, keep.rownames = TRUE)[]
write.csv(r, "output/aggtable_names.csv")
return(r)
  }
  
#SELECT RELEVANT VARIABLES FOR THE AGGREGATION TABLE
  extract_aggtable <- function(r) {
    r<-r[c("location_interview_rc_peshkhabor", "location_interview_rc_Sahila", "gender_ki_male",
           "gender_ki_female", "residency_yes", "residency_no", "residency_dont_know", "residency_refuse", 
           "residency_yes", "arrive_syria_yes", "arrive_syria_no", "reason_displace", "reason_displace_airstrikes", 
           "reason_displace_arrival_military", "reason_displace_dont_know", "reason_displace_escorted_security", 
           "reason_displace_home_destroyed", "reason_displace_other", "reason_displace_other_home_destr", 
           "reason_displace_planned_travel_return", "reason_displace_planned_travel_to_kri")]
    
    write.csv(r, "output/extract_indesign.csv")
    
    return(r)
  }

#CHARACTER VARIABLES - FUNCTION TO SELECT THE TOP FOUR MOST COMMONLY MENTIONED 
  #FACTORS FOR THE INDESIGN FILE
select_top_four_character <- function(df, x) {
    
    df<-  df[c(which(startsWith(names(df), x)))]
    df<-df[1,]            
    df <- as.data.frame(unlist(t(df)))
    df<-as.data.frame(sort(df$numbers, decreasing=T))
    df<-setDT(df, keep.rownames = TRUE)[]
    df<-as.data.frame(df[1:4,])
names(df)[2] <- x
df<-as.data.frame(t(df))
return(df)
  }

#NUMERIC VARIABLES - FUNCTION TO SELECT THE TOP FOUR MOST COMMONLY MENTIONED 
  #FACTORS FOR THE INDESIGN FILE
select_top_four_numeric <- function(df, x) {
df<-  df[c(which(startsWith(names(df), x)))]
df<-df[1,]            
df <- as.data.frame(unlist(t(df)))
df<-setDT(df, keep.rownames = T)[]
df<-df[order(-df$numbers),][1:4,]
names(df)[2] <- x
df<-as.data.frame(t(df))
return(df)
}


       
       