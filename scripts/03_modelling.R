#### Preamble ####
# Purpose: Using the cleaned versions of data obtained from https://usa.ipums.org/usa/index.shtml and https://www.voterstudygroup.org/publication/nationscape-data-set to create models to estimate the outcome of the US 2020 Presidential Election. Check `README.md` for specific instructions.
# Author: Arshnoor Gill, Samantha Wong and Zhendong Zhang
# Data: November 2 2020
# Contact: arshnoorgill@utoronto.ca, zhendong.zhang@mail.utoronto.ca and saman.wong@mail.utoronto.ca
# Pre-requisites: 
# - Need to have downloaded the 'survey_data.csv' and 'census_data.csv' from the outputs folder in the repo:https://github.com/smwong88/US_2020_Election_Forecasting.git

library(tidyverse)
library(tidybayes)
library(brms)
library(here)
library(broom)
getwd()
setwd("~/outputs/paper")
read.csv("./outputs/paper/survey_data.csv")
# Ordinary Linear Regression model for multiple variables
survey_model <- lm(dummy_vote ~ gender + age + educd + state + race, data = survey_data)

# Generalized Linear Regression model for multiple variables

survey_model_bin <- glm(dummy_vote ~ gender + age + educd + state + race, data = survey_data, family = binomial(link = "logit"))

#Ordinary Linear Regression ummary to .csv
lm_sum <- tidy(survey_model)

#Generalized Linear Regression Summary to .csv
glm_sum <- tidy(survey_model_bin)

write.csv(lm_sum, "lm_summary.csv")
write.csv(glm_sum, "glm_summary.csv")

#Calculating the proportions of each variables on the survey data and the sample data to compare 

race_popn <- census_data %>%
  group_by(race) %>%
  summarize(Num=sum(Number)) %>%
  mutate(PROP=Num/sum(Num),TYPE='Popn',VAR='Race',CAT=race) %>%
  ungroup()

race_data <- survey_data %>%
  group_by(race) %>%
  summarise(Num=n()) %>%
  mutate(PROP=Num/sum(Num),TYPE='Sample',VAR='Race',CAT=race) %>%
  ungroup()

race <-rbind(race_data[,2:6],race_popn[,2:6])

gender_popn <- census_data%>%
  group_by(gender)%>%
  summarize(Num=sum(Number))%>%
  mutate(PROP=Num/sum(Num),TYPE='Popn',VAR='Gender',CAT=gender)%>%
  ungroup()
gender_data <- survey_data%>%
  group_by(gender)%>%
  summarise(Num=n())%>%
  mutate(PROP=Num/sum(Num),TYPE='Sample',VAR='Gender',CAT=gender)%>%
  ungroup()
gender <- rbind(gender_data[,2:6],gender_popn[,2:6] )

age_popn <- census_data%>%
  group_by(age)%>%
  summarize(Num=sum(Number))%>%
  mutate(PROP=Num/sum(Num),TYPE='Popn',VAR='Age',CAT=age)%>%
  ungroup()
age_data <- survey_data%>%
  group_by(age)%>%
  summarise(Num=n())%>%
  mutate(PROP=Num/sum(Num),TYPE='Sample',VAR='Age',CAT=age)%>%
  ungroup()
age<-rbind(age_data[,2:6],age_popn[,2:6])

edu_popn <- census_data%>%
  group_by(educd)%>%
  summarize(Num=sum(Number))%>%
  mutate(PROP=Num/sum(Num),TYPE='Popn',VAR='Education',CAT=educd)%>%
  ungroup()
edu_data <- survey_data%>%
  group_by(educd)%>%
  summarise(Num=n())%>%
  mutate(PROP=Num/sum(Num),TYPE='Sample',VAR='Education',CAT=educd)%>%
  ungroup()
educd <- rbind(edu_data[,2:6],edu_popn[,2:6])

state_popn <- census_data%>%
  group_by(state)%>%
  summarize(Num=sum(Number))%>%
  mutate(PROP=Num/sum(Num),TYPE='Popn',VAR='State',CAT=state)%>%
  ungroup()

state_plot_data <- survey_data%>%
  group_by(state)%>%
  summarise(Num=n())%>%
  mutate(PROP=Num/sum(Num),TYPE='Sample',VAR='State',CAT=state)%>%
  ungroup()

#Factoring the estimates by sample or population
state_plot_data <- rbind(state_plot_data[,2:6],state_popn[,2:6])
state_plot_data$TYPE <- factor(state_plot_data$TYPE, levels = c("Sample","Popn"))

#
plot_data <- rbind(gender,race,age,educd)
plot_data$TYPE <- factor(plot_data$TYPE, levels = c("Sample","Popn"))

save(state_plot_data, file = "./outputs/state_plot_data.rda", version = 2)
save(plot_data, file = "./outputs/plot_data.rda", version = 2)

#Counting all the numbers in each state in the Census Data
#Creating the estimates of proportion for support for Mr. Joe Biden in each state
set.seed(7310)

#Creating a new variable in census_data to account for the predicted voting outcome drawn from the survey model
census_data$estimate <-
  survey_model %>%
  predict(newdata = census_data)

#Grouping estimate of vote proportion on each state
prediction_prop <- census_data %>%
  mutate(vote_predict_prop = estimate*Proportion) %>%
  summarise(vote_predict = sum(vote_predict_prop)/sum(Proportion)) 

# Forming the bayes model
bayes_model <- brm(dummy_vote ~ (1|gender) + (1|age) + (1|state) + (1|educd) + (1|race),
                   data = survey_data,
                   family = bernoulli(),
                   file = "brms_model", control = list(adapt_delta = 0.98))
bayes_model <- read_rds("brms_model.rds")

# Calculate proportions by each variable for census data and survey data

age_prop <- census_data %>%
  ungroup() %>%
  group_by(age) %>%
  mutate(prop = Number/sum(Number)) %>%
  ungroup()

race_prop <- census_data %>%
  ungroup() %>%
  group_by(race) %>%
  mutate(prop = Number/sum(Number)) %>%
  ungroup()

edu_prop <- census_data %>%
  ungroup() %>%
  group_by(educd) %>%
  mutate(prop = Number/sum(Number)) %>%
  ungroup()

gender_prop <- census_data %>%
  ungroup() %>%
  group_by(gender) %>%
  mutate(prop = Number/sum(Number)) %>%
  ungroup()

#Post stratification estimates grouped by state
post_strat_est <-
  bayes_model %>%
  tidybayes::add_predicted_draws(newdata = census_data) %>%
  rename(vote_predict = .prediction) %>%
  mutate(vote_predict_prop = vote_predict*Proportion)%>%
  group_by(state, .draw) %>%
  summarise(vote_predict = sum(vote_predict_prop)) %>%
  group_by(state) %>%
  summarise(mean = mean(vote_predict), 
            lower = quantile(vote_predict, 0.025),
            upper = quantile(vote_predict, 0.975))


#Post stratification estimates grouped by age
post_strat_est_age <-
  bayes_model %>%
  tidybayes::add_predicted_draws(newdata = age_prop) %>%
  rename(vote_predict = .prediction) %>%
  mutate(vote_predict_prop = vote_predict*prop)%>%
  group_by(age, .draw) %>%
  summarise(vote_predict = sum(vote_predict_prop)) %>%
  group_by(age) %>%
  summarise(mean = mean(vote_predict), 
            lower = quantile(vote_predict, 0.025),
            upper = quantile(vote_predict, 0.975))

#Post etratification estimates grouped by race
post_strat_est_race <-
  bayes_model %>%
  tidybayes::add_predicted_draws(newdata = race_prop) %>%
  rename(vote_predict = .prediction) %>%
  mutate(vote_predict_prop = vote_predict*prop)%>%
  group_by(race, .draw) %>%
  summarise(vote_predict = sum(vote_predict_prop)) %>%
  group_by(race) %>%
  summarise(mean = mean(vote_predict), 
            lower = quantile(vote_predict, 0.025),
            upper = quantile(vote_predict, 0.975))
#Post Stratification estimates grouped by gender
post_strat_est_gender <-
  bayes_model %>%
  tidybayes::add_predicted_draws(newdata = gender_prop) %>%
  rename(vote_predict = .prediction) %>%
  mutate(vote_predict_prop = vote_predict*prop)%>%
  group_by(gender, .draw) %>%
  summarise(vote_predict = sum(vote_predict_prop)) %>%
  group_by(gender) %>%
  summarise(mean = mean(vote_predict), 
            lower = quantile(vote_predict, 0.025),
            upper = quantile(vote_predict, 0.975))
#Post stratification estimates grouped by level of education attained
post_strat_est_edu <-
  bayes_model %>%
  tidybayes::add_predicted_draws(newdata = edu_prop) %>%
  rename(vote_predict = .prediction) %>%
  mutate(vote_predict_prop = vote_predict*prop)%>%
  group_by(educd, .draw) %>%
  summarise(vote_predict = sum(vote_predict_prop)) %>%
  group_by(educd) %>%
  summarise(mean = mean(vote_predict), 
            lower = quantile(vote_predict, 0.025),
            upper = quantile(vote_predict, 0.975))

#writing .csv files for each of the prediction tibbles to speed up the time it takes to knit the rmarkdown
write.csv(post_strat_est, "./outputs/paper/post_strat_est.csv")
write.csv(post_strat_est_race, "./outputs/paper/post_strat_est_race.csv")
write.csv(post_strat_est_gender, "./outputs/paper/post_strat_est_gender.csv")
write.csv(post_strat_est_age, "./outputs/paper/post_strat_est_age.csv")
write.csv(post_strat_est_edu, "./outputs/paper/post_strat_est_edu.csv")

#Calculating the popular vote based on the MRP estimate from the Bayesian model

#Calculating the estimate of votes overall, or the popular vote
prop_of_state <- census_data %>%
  group_by(state) %>%
  summarize(total_state=sum(Number))%>%
  mutate(PROP=total_state/sum(total_state))

state_estimates <- left_join(post_strat_est, prop_of_state, by = "state")

pop_vote_est_brms <- state_estimates %>%
  mutate(state_predict = mean*PROP)

electoral_college <- c(9,3,11,6,55,9,7,3,3,29,16,4,4,20,11,6,6,8,8,4,10,11,16,10,6,10,3,6,4,14,5,29,15,3,18,7,7,20,4,9,3,11,38,6,3,13,12,5,10,3)
