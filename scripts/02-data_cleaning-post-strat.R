#### Preamble ####
# Purpose: Prepare and clean the census data downloaded from https://usa.ipums.org/usa/index.shtml. Check `README.md` for specific instructions.
# Author: Arshnoor Gill, Samantha Wong and Zhendong Zhang
# Data: November 2 2020
# Contact: arshnoorgill@utoronto.ca, zhendong.zhang@mail.utoronto.ca and saman.wong@mail.utoronto.ca
# Pre-requisites: 
# - Need to have downloaded the data from https://usa.ipums.org/usa/index.shtml and save the folder that you're  interested in to inputs/data 


library(haven)
library(tidyverse)
library(labelled)

# Read in raw data and ensure that your file name is correct.
raw_data <- read_dta("./Inputs/usa_00003.dta")
raw_data <- labelled::to_factor(raw_data)

# Keep relevant variables.
reduced_data <- 
  raw_data %>%
  select(age, sex, stateicp, race, educd)

reduced_data <- as.data.frame(reduced_data)

# Clean the state variable.
abbs <- tibble(stateicp = str_to_lower(state.name), state = state.abb)

reduced_data <- reduced_data %>%
  left_join(abbs)

# Clean the race variable.
mut_race <- function(x){
  for(i in 1:length(x)){
    if (x[i] == "black/african american/negro"){
      x[i] <- "black/african american"
    };
    if (x[i] == "two major races"){
      x[i] <- "other race, nec"
    };
    if (x[i] == "three or more major races"){
      x[i] <- "other race, nec"
    }
  }
  return(x)
}

reduced_data <- reduced_data %>%
  mutate(race = mut_race(as.vector(race)))

# Clean the age variable.
reduced_data <- reduced_data %>%
  filter(as.numeric(age) > 17)

mut_age <- function(x){
  for(i in 1:length(x)){
    if (x[i] < 30 & x[i] > 17){
      x[i] <- "18 to 29"
    };
    if (x[i] < 45 & x[i] > 29){
      x[i] <- "30 to 44"
    };
    if (x[i] < 60 & x[i] > 44){
      x[i] <- "45 to 55"
    };
    if (x[i] == "less than 1 year old"){
      x[i] <- "18 to 29"
    };
    if (x[i] >= 60 | x[i] == "90 (90+ in 1980 and 1990)"){
      x[i] <- "60 plus"
    }
  }
  return(x)
}

reduced_data <- reduced_data %>%
  mutate(age = mut_age(as.vector(age)))

# Clean the education variable and turn into four categories.
mut_educd <- function(x){
  for(i in 1:length(x)){
    if (x[i] == "1 or more years of college credit, no degree" | x[i] == "some college, but less than 1 year"){
      x[i] <- "Didn't graduate college"
    };
    if (x[i] == "regular high school diploma" | x[i] == "ged or alternative credential"){
      x[i] <-"Graduated high school"
    };
    if (x[i] == "bachelor's degree" | x[i] == "professional degree beyond a bachelor's degree" | x[i] == "master's degree" | x[i] == "nursery school, preschool" | x[i] == "doctoral degree" | x[i] == "associate's degree, type not specified"){
      x[i] <- "Graduated college"
    };
    
    if (x[i] == "grade 1" | x[i] == "grade 2" | x[i] == "grade 3" | x[i] == "no schooling completed" | x[i] == "kindergarten" | x[i] == "n/a" | x[i] == "grade 4" | x[i] == "grade 5" | x[i] == "grade 6" | x[i] == "grade 7" | x[i] == "grade 8" | x[i] == "12th grade, no diploma" | x[i] == "grade 10" | x[i] == "grade 11" | x[i] == "grade 9"){
      x[i] <- "Didn't graduate high school"
    }
  }
  return(x)
}


reduced_data <- reduced_data %>%
  mutate(educd = mut_educd(as.vector(educd)))

mut_state <- function(x){
  for(i in 1:length(x)){
    if ( is.na(x[i] )){
      x[i] <- "DC"
    }
  }
  return(x)
}

reduced_data <- reduced_data %>%
  mutate(state = mut_state(reduced_data$state))

census <- reduced_data %>%
  group_by(state, sex, age, race, educd) %>%
  count(name = "Number")

total <- reduced_data %>%
  group_by(state) %>%
  count()

census <- census %>%
  left_join(total)

# Add numbers and proportion to the census.
census <- census %>%
  mutate(n = Number/n) %>%
  rename(Proportion = n) %>%
  rename(gender = sex)

write.csv(census, "./outputs/paper/census_data.csv")
