## mental_visit_before+after.R
## author: "Allissa Nguyen"
## date: "11/11/2021"
  
knitr::opts_chunk$set(echo = FALSE)
# Load required packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(infer))
suppressPackageStartupMessages(library(modelr))
suppressPackageStartupMessages(library(broom))


## Preprocessing

visits_mental$cat_visit_time <- with(visits_mental,  
                                     ifelse(visit_day >= '2019-01-01' & visit_day <= '2019-03-01', "pre_covid",
                                            ifelse(visit_day>'2019-05-01', "post_covid", 'during_covid')))




visits_mental_reduced <- visits_mental %>%
  filter(cat_visit_time == "pre_covid" | cat_visit_time == "post_covid")





library(dplyr)
visits_mental_joined <- visits_mental_reduced %>% 
  left_join(x = visits_mental_reduced, y = vets, by = "VID")




## Analysis

visitscount<-visits_mental_joined %>%
  group_by(cat_visit_time) %>%
  summarize(
    count = n()
  )





pre_covid_race <- visits_mental_joined %>%
  filter(cat_visit_time == 'pre_covid') %>% 
  group_by(race) %>%
  summarize(
    count = n()
  )




post_covid_race <- visits_mental_joined %>%
  filter(cat_visit_time == 'post_covid') %>% 
  group_by(race) %>%
  summarize(
    count = n()
  )




# <!-- rownames(visitscount_chi) <- visitscount_chi %>% pull(cat_visit_time) -->
#   <!-- visitscount_chi <- visitscount_chi[c(-1)] -->
#   <!-- chisq <- chisq.test(visitscount_chi) -->
#   <!-- chisq -->
#   <!-- chisq$observed -->
  
  
  
  
  
  
visits_mental$cat_visit_time <- with(visits_mental,  
                                     ifelse(visit_day >= '2019-01-01' & visit_day <= '2019-03-01', "pre_covid",
                                            ifelse(visit_day>='2019-05-01' & visit_day<= '2019-06-01', "post_covid", 'notusing')))



## 1month difference

original <- visits_mental



visits_mental <-original




visits_mental$cat_visit_time <- with(visits_mental,  
                                     ifelse(visit_day >= '2019-01-01' & visit_day <= '2019-03-01', "pre_covid",
                                            ifelse(visit_day>='2019-05-01' , "post_covid", 'notusing')))




visits_mental_reduced_one_month <- visits_mental %>%
  filter(cat_visit_time == "pre_covid" | cat_visit_time == "post_covid")
visits_mental_reduced_one_month




library(dplyr)
visits_mental_reduced_one_month <- visits_mental_reduced_one_month %>% 
  left_join(x = visits_mental_reduced_one_month, y = vets, by = "VID")
visits_mental_reduced_one_month




count_visit_day_time <- visits_mental_reduced_one_month %>% 
  group_by(cat_visit_time) %>% 
  count(visit_day)
count_visit_day_time


# 
# testing just vets
# 



mental_health_visits <- vets %>% 
  right_join(x = vets, y = visits_mental, by = 'VID')




mental_health_visits$cat_visit_time <- with(mental_health_visits,  
                                            ifelse(visit_day >= '2019-01-01' & visit_day <= '2019-03-01', "pre_covid",
                                                   ifelse(visit_day>='2019-06-01', "post_covid", 'notusing')))




mental_health_visits_cat <- mental_health_visits %>%
  filter(cat_visit_time == "pre_covid" | cat_visit_time == "post_covid")
mental_health_visits_cat



mental_health_visits_cat_filtered <- mental_health_visits_cat %>% 
  group_by(cat_visit_time) %>% 
  count(visit_day)
mental_health_visits_cat_filtered




mental_health_visits_cat_filtered %>% 
  summarize(
    count = n(),
    mean = mean(n, na.rm = TRUE),
    median = median(n, na.rm = TRUE),
    std.dev = sd(n, na.rm = TRUE),
    iqr = IQR(n, na.rm = TRUE),
    min = min(n, na.rm = TRUE),
    max = max(n, na.rm = TRUE))





mental_health_visits_cat_stat <- mental_health_visits_cat_filtered %>%
  specify(formula = n ~ cat_visit_time) %>%
  calculate(stat = "diff in means",
            order = c("post_covid", "pre_covid"))
count_visit_day_time_stat



mental_health_visits_cat_filtered_null <- mental_health_visits_cat_filtered %>%
  specify(formula = n ~ cat_visit_time) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 10000, type = "permute") %>%
  calculate(stat = "diff in means",
            order = c("post_covid", "pre_covid"))



mental_health_visits_cat_filtered_null  %>%
  get_p_value(obs_stat = mental_health_visits_cat_stat, direction = "two-sided")





mental_health_visits_cat_filtered_null %>%
  visualize() +
  shade_p_value(obs_stat = mental_health_visits_cat_stat, direction = "two_sided") +
  labs(title = "Simulated Null Distribution with P-Value", x = "Difference in mean counts of visits between pre-covid month and post-covid", y = "Count") +
  theme(axis.title.x = element_text(size = 8))


























count_visit_day_time_stat <- count_visit_day_time %>%
  specify(formula = n ~ cat_visit_time) %>%
  calculate(stat = "diff in means",
            order = c("post_covid", "pre_covid"))
count_visit_day_time_stat




count_visit_day_time_null <- count_visit_day_time %>%
  specify(formula = n ~ cat_visit_time) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 10000, type = "permute") %>%
  calculate(stat = "diff in means",
            order = c("post_covid", "pre_covid"))






count_visit_day_time_null  %>%
  get_p_value(obs_stat = count_visit_day_time_stat, direction = "two-sided")





count_visit_day_time_null %>%
  visualize() +
  shade_p_value(obs_stat = count_visit_day_time_stat, direction = "two_sided") +
  labs(title = "Simulated Null Distribution with P-Value", x = "Difference in mean counts of visits between pre-covid month and post-covid", y = "Count") +
  theme(axis.title.x = element_text(size = 8))
















# 
# visits_mental_reduced_one_month %>% 
#   mutate(
#     num_veterans_over_
#     visit_day)
# 

a


























# <!-- testing -->
#   
# visits_mental$cat_visit_time <- cut(x, breaks = "quarter")
# 
# 
# 
# 
# 
# x <- as.Date(1:1000, origin = "2000-01-01")
# x <- cut(x, breaks = "quarter") 
# 
# labs <- paste(substr(levels(x),1,4), "/", 1:4, sep="")
# x <- factor(x, labels = labs)
# 
# 
# 
# 
# 
# DF <- data.frame(matchDate = as.POSIXct(as.Date(sample(5000,100,replace=TRUE), origin="1993-01-01")))
# 
# 
# 
# 
# 
# visits_mental$cat_visit_time <- with(visits_mental,  
#                                      ifelse(visit_day >= '2019-01-01' & visit_day <= '2019-03-01', "pre_covid",
#                                             ifelse(visit_day >= '2019-05-01'& visit_day <= '2019-06-01', "post_covid", 'during_covid')))
# 
# 
# 
# 
# DF <- data.frame(matchDate = as.POSIXct(as.Date(sample(5000,100,replace=TRUE), origin="1993-01-01")))
# 
# years <- 1992:2011
# DF$season <- cut(DF$matchDate, 
#                  breaks=as.POSIXct(paste(years,"-08-01",sep="")),
#                  labels=paste(years[-length(years)],years[-length(years)]+1,sep="/"))
# 
# 
# 
# 
# 
# visits_mental$cat_visit_time
# 
# 
