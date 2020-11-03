#### Preamble ####
# Purpose: The purpose of this code is to clean up the UCLA survey data to 
# better serve the analysis that we plan to conduct.
# Authors: Jasmine Carlos, Matthew Caringi, Haeun Choi, Mahmoud Elsheikh
# Date: 2 November 2020
# Contact: jasmine.carlos@mail.utoronto.ca, matthew.caringi@mail.utoronto.ca,
# haeun.choi@mail.utoronto.ca, mahmoud.elsheikh@mail.utoronto.ca
# Pre-requisites: In order to use the following code you will need to download
# the UCLA Nationscape Phase 2 June 25, 2020 survey. This data is not included
# in the GitHub repository as it needs to be downloaded directly from the UCLA
# database. To download this data visit 
# https://www.voterstudygroup.org/publication/nationscape-data-set and request
# access to the Democracy Fund + UCLA Nationscape 'Full Data Set'. This will 
# give you acess to a multitude of surveys. The survey used in this script is
# the Phase 2 June 25, 2020 survey entitled ns20200625.dta

#### Workspace setup ####
library(haven)
library(tidyverse)

# Read in the raw data
# Change the path in the read_dta function as necessary, according to the path 
# of the downloaded raw data set.
raw_data <- read_dta("/cloud/project/inputs/ns20200625.dta")

# Add labels to the variables
raw_data <- labelled::to_factor(raw_data)

# Here we are keeping only variables we may have interest in for the purpose of
# our analysis.

reduced_data <- 
  raw_data %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         employment,
         foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         congress_district,
         age)

# Further selecting variables of interest and filter out NA values

selected_data <- reduced_data %>%
  select(vote_2020, race_ethnicity, education, age) %>%
  filter(!vote_2020 == 'NA')

# Take the age variable and group the data into age groups that in general span
# 10 years.

grouped_data <- selected_data %>%
  mutate(
    age_grouped = case_when(
      age>=18 & age<=29 ~ "18-29",
      age>=30 & age<=39 ~ "30-39",
      age>=40 & age<=49 ~ "40-49",
      age>=50 & age<=59 ~ "50-59",
      age>=60 ~ "60+") 
  ) %>%
# Group the education variable into more succinct groups. More specifically 
# grouping into the following: "High School or Less", "Trade Certificate",
# "College Degree or In Progress", "Associate Degree or In Progress" and
# "Masters/Doctorate Degree or In Progress"
  mutate(education = case_when(
    education == "3rd Grade or less" ~ "High School or Less",
    education == "Middle School - Grades 4 - 8" ~ "High School or Less",
    education == "Completed some high school" ~ "High School or Less",
    education == "High school graduate" ~ "High School or Less",
    education == "Other post high school vocational training" ~ "Trade Certificate",
    education == "Completed some college, but no degree" ~ "College Degree or In Progress",
    education == "Associate Degree" ~ "Associate Degree or In Progress",
    education == "College Degree (such as B.A., B.S.)" ~ "College Degree or In Progress",
    education == "Completed some graduate, but no degree" ~ "Masters/Doctorate Degree or In Progress",
    education == "Masters degree" ~ "Masters/Doctorate Degree or In Progress",
    education == "Doctorate degree" ~ "Masters/Doctorate Degree or In Progress"
  )) %>%
# Group the race/ethnicity variable into more succinct groups. More specifically 
# grouping into the following: "White", "Black, or African American",
# "American Indian or Alaska Native", "Asian", "Pacific Islander" and
# "Some other race"
  mutate(
    race_ethnicity = case_when(
      race_ethnicity == "White" ~ "White",
      race_ethnicity == "Black, or African American" ~ "Black, or African American",
      race_ethnicity == "American Indian or Alaska Native" ~ "American Indian or Alaska Native",
      race_ethnicity == "Asian (Asian Indian)" ~ "Asian",
      race_ethnicity == "Asian (Chinese)" ~ "Asian",
      race_ethnicity == "Asian (Filipino)" ~ "Asian",
      race_ethnicity == "Asian (Japanese)" ~ "Asian",
      race_ethnicity == "Asian (Korean)" ~ "Asian",
      race_ethnicity == "Asian (Vietnamese)" ~ "Asian",
      race_ethnicity == "Asian (Other)" ~ "Asian",
      race_ethnicity == "Pacific Islander (Native Hawaiian)" ~ "Pacific Islander",
      race_ethnicity == "Pacific Islander (Guamanian)" ~ "Pacific Islander",
      race_ethnicity == "Pacific Islander (Samoan)" ~ "Pacific Islander",
      race_ethnicity == "Pacific Islander (Other)" ~ "Pacific Islander",
      race_ethnicity == "Some other race" ~ "Some other race"
    )
  )


# Write the above data set into a csv to more easily access the data set when
# conducting further analysis.

write_csv(grouped_data, "clean_UCLA_survey_data.csv")



