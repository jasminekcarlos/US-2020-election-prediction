#### Preamble ####
# Purpose: The purpose of this code is to conduct multilevel regression with
# post-stratification in order to predict whether Donald Trump or Joe Biden
# will win the popular in the 2020 United States Federal Election.
# Authors: Jasmine Carlos, Matthew Caringi, Haeun Choi, Mahmoud Elsheikh
# Date: 2 November 2020
# Contact: jasmine.carlos@mail.utoronto.ca, matthew.caringi@mail.utoronto.ca,
# haeun.choi@mail.utoronto.ca, mahmoud.elsheikh@mail.utoronto.ca
# Pre-requisites: You will need the clean UCLA survey data and clean ACS survey
# for the post stratification step. The script files entitled 
# 01-data_cleaning-UCLA-survey and 01-data_cleaning-ipums-census detail how the 
# raw data was cleaned. For the following code you will need the csv files 
# entitled clean_UCLA_survey_data.csv and clean_post_strat_dat.csv which are 
# the files that were output from the corresponding scripts.

#### Workspace set-up ####
library(broom)
library(here)
library(tidyverse)
library(kableExtra)

#### Prepare survey data set ####

# Read in the clean UCLA survey data which is named clean_UCLA_survey_data.csv
# and can be found in the outputs folder. Change the path in the read_dta 
# function as necessary, according to the path of the downloaded raw data set.
example_poll <- read_csv("/cloud/project/outputs/clean_UCLA_survey_data.csv")
  
# Below we are cleaning the vote_2020 variable. This is the variable we will be
# predicting so we must convert it into a binary variable. Therefore we filter 
# out the "I am not sure/don't know" and "I would not vote" variables. 
# Furthermore we convert the categorical variables of "Donald Trump" and 
# "Joe Biden" into numerical variables 0 and 1 accordingly.

example_poll_final <- example_poll %>%
  filter(!vote_2020 == "I am not sure/don't know") %>%
  filter(!vote_2020 == "I would not vote") %>%
  mutate(
    vote_2020_binary = case_when(
      vote_2020 == "Donald Trump" ~ 0,
      vote_2020 == "Joe Biden" ~ 1,
    ))

#### Make model ####

# The following uses the glm function to create a standard logistic regression
# model from the survey data. The dependent variable is vote_2020_binary which
# indicates who a respondent intends to vote for in the 2020 US Election
# The predictor variables used are race/ethnicity, education and age.

model <- glm(vote_2020_binary ~  as.factor(race_ethnicity) +
               as.factor(education) + as.factor(age_grouped), 
             data = example_poll_final, 
             family = binomial)

#### Prepare post-stratification data set ####

# Read in the clean 2018 ACS survey data which is named clean_post_strat_dat.csv
# and can be found in the outputs folder. Change the path in the read_dta 
# function as necessary, according to the path of the downloaded raw data set.

census_data <- read_csv("/cloud/project/outputs/clean_post_strat_dat.csv")

#### Post-stratification Estimates ####

# Create cell-level predictions. A new column with estimates is created

clean_census_data <- census_data %>%
  filter(!count == 0)

clean_census_data$estimate <-
  model %>%
  predict(newdata = clean_census_data)

# Reweigh estimates by multiplying by the corresponding proportions.
# Summarize by candidate to find the proportion of respondents who vote for
# each candidate.

prediction <- clean_census_data %>%
  mutate(win_pred_prop = estimate*proportion) %>%
  summarise(win_predict = sum(win_pred_prop))

prediction


