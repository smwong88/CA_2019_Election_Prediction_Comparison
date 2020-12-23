#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://www.voterstudygroup.org/publication/nationscape-data-set. The data used is the most recent survey, `ns20200625.dta`.
# Author: Arshnoor Gill, Samantha Wong and Zhendong Zhang
# Data: November 2 2020
# Contact: arshnoorgill@utoronto.ca, zhendong.zhang@mail.utoronto.ca and saman.wong@mail.utoronto.ca
# Pre-requisites: 
# - Need to have downloaded the data from https://usa.ipums.org/usa/index.shtml and save the folder that you're  interested in to inputs/data 
# - Don't forget to gitignore it!

#### Workspace setup ####
library(haven)
library(tidyverse)

# Read in the raw data.
raw_data <- read_dta("./inputs/data/ns20200625.dta")

# Add the labels.
raw_data <- labelled::to_factor(raw_data)

# Keep variables of interest.
reduced_data <- 
  raw_data %>% 
  select(vote_2020,
         gender,
         race_ethnicity,
         education,
         state,
         age)

#new <- reduced_data

# Make the `vote` variable binary by removing undecided/non-voters and add a dummy variable on whether someone would vote for Biden for clarity of data analysis.
reduced_data <- reduced_data %>%
  filter(vote_2020 == "Donald Trump" | vote_2020 == "Joe Biden") %>%
  mutate(dummy_vote = ifelse(vote_2020 == "Joe Biden", 1, 0))

# Data cleaning on the state name. This includes a new column, `state_name`, which is each state with its full name. This will keep the survey data consistent with the stratification data.
names_matcher <- tibble(state_name = str_to_lower(state.name), state = state.abb)

reduced_data <- reduced_data %>%
  left_join(names_matcher)

reduced_data <- reduced_data %>%
  mutate(state_name = ifelse(is.na(state_name), "district of columbia", state_name))

# Data cleaning on age to turn it into four categories rather than as a continuous variable.
reduced_data <- reduced_data %>%
  filter(age > 17) %>%
  mutate(age = ifelse(age < 30, "18 to 29",
                      ifelse(age < 45, "30 to 44",
                             ifelse(age < 60, "45 to 55", "60 plus")
                      )
  )
  )

# Data cleaning on gender so it matches the stratification dataset.
reduced_data <- reduced_data %>%
  mutate(gender = ifelse(gender == "Male", "male", "female"))

# Data cleaning on race_ethnicity to match stratification dataset.
reduced_data <- reduced_data %>%
  mutate(race_ethnicity = ifelse(race_ethnicity == "White", "white",
                                 ifelse(race_ethnicity == "American Indian or Alaska Native", "american indian or alaska native",
                                        ifelse(race_ethnicity == "Asian (Japanese)", "japanese",
                                               ifelse(race_ethnicity == "Black, or African American", "black/african american",
                                                      ifelse(race_ethnicity == "Asian (Chinese)", "chinese",
                                                             ifelse(race_ethnicity == "Some other race", "other race, nec",
                                                                    "other asian or pacific islander")
                                                      )
                                               )
                                        )
                                 )
  )
  )


# Created smaller categories for education variable due to issues with size of cells.
reduced_data <- reduced_data %>%
  rename(educd = education)

reduced_data <- reduced_data %>%
  mutate(educd = ifelse(educd == "3rd Grade or less" | educd == "Middle School - Grades 4 - 8" | educd == "Completed some high school",
                        "Didn't graduate high school",
                        ifelse(educd == "High school graduate" | educd == "Other post high school vocational training",
                               "Graduated high school",
                               ifelse(educd == "Completed some college, but no degree",
                                      "Didn't graduate college",
                                      "Graduated college"))))

# Change variable names to maintain consistency.
reduced_data <- reduced_data %>%
  rename(race = race_ethnicity,
         stateicp = state_name)

# Save to outputs.
write.csv(reduced_data, "./outputs/paper/survey_data.csv")

