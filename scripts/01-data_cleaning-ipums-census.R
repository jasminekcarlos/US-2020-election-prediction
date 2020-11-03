#### Preamble ####
# Purpose: The purpose of this code is to clean up the 2018 1-year American
# Community Survey (ACS) from Integrated Public Use Microdata Series (IPUMS) USA.
# Authors: Jasmine Carlos, Matthew Caringi, Haeun Choi, Mahmoud Elsheikh
# Date: 2 November 2020
# Contact: jasmine.carlos@mail.utoronto.ca, matthew.caringi@mail.utoronto.ca,
# haeun.choi@mail.utoronto.ca, mahmoud.elsheikh@mail.utoronto.ca
# Pre-requisites: In order to use the following code you will need download the
# 2018 1-year ACS. This data is not included in the GitHub repository as it 
# needs to be downloaded directly from IPUMS. To download this data visit
# https://usa.ipums.org/usa/index.shtml to create an account with IPUMS. The 
# data used in this script is the 2018 1-year ACS with the variables of REGION,
# STATEICP, AGE, SEX, MARST, RACE, HISPAN, BPL, CITIZEN, EDUC, LABFORCE and INCTOT.
# This dataset was downloaded with 614347 observations formatted as a .dta file.


#### Workspace setup ####
library(haven)
library(tidyverse)

# Read in the raw data
# Change the path in the read_dta function as necessary, according to the path 
# of the downloaded raw data set.
reduced_data2 <- read_dta("/cloud/project/inputs/ipum_data.dta")

# Add labels to the variables
reduced_data2 <- labelled::to_factor(reduced_data2)

# Edit the data to become cleaner and easier to manipulate
var1 <- reduced_data2$age
withoutLessThanOne <- filter(var1, !var1 == "less than 1 year old")
var2 <- as.character(var1)
reduced_data2$ageChar <- var2  
dataNoOne <- filter(reduced_data2, !ageChar == "less than 1 year old")
dataNoOne1 <- filter(dataNoOne,!dataNoOne$ageChar == 'NA')
nums <- as.numeric(dataNoOne1$ageChar)                             
dataNoOne1$ageNums <- nums

# Further selecting variables of interest and filter out NA values
reduced_data2 <- 
  dataNoOne1 %>% 
  select(sex, 
         ageChar, 
         educd,
         raced) %>%
  filter(!ageChar < 18) %>%
  filter(!educd == 'n/a') %>%
  filter(!educd == 'NA')

# Take the age variable and group the data into age groups that in general span
# 10 years.
grouped_data2 <- reduced_data2 %>%
  mutate(
    age_grouped = case_when(
      ageChar>=18 & ageChar<=29 ~ "18-29",
      ageChar>=30 & ageChar<=39 ~ "30-39",
      ageChar>=40 & ageChar<=49 ~ "40-49",
      ageChar>=50 & ageChar<=59 ~ "50-59",
      ageChar>=60 ~ "60+") 
  ) %>%
# Group the education variable into more succinct groups. More specifically 
# grouping into the following: "High School or Less", "Trade Certificate",
# "College Degree or In Progress", "Associate Degree or In Progress" and
# "Masters/Doctorate Degree or In Progress"  
  mutate(education_grouped = case_when(
    educd == "no schooling completed" ~ "High School or Less",
    educd == "nursery school, preschool" ~ "High School or Less",
    educd == '3rd Grade or less' ~ 'High School or Less',
    educd == "regular high school diploma" ~ "High School or Less",
    educd == "Completed some high school" ~ "High School or Less",
    educd == "High school graduate" ~ "High School or Less",
    educd == 'grade 1' ~ 'High School or Less',
    educd == "grade 2" ~ "High School or Less",
    educd == "grade 3" ~ "High School or Less",
    educd == "grade 4" ~ "High School or Less",
    educd == "grade 5" ~ "High School or Less",
    educd == "grade 6" ~ "High School or Less",
    educd == "grade 7" ~ "High School or Less",
    educd == "grade 8" ~ "High School or Less",
    educd == "grade 9" ~ "High School or Less",
    educd == "grade 10" ~ "High School or Less",
    educd == "grade 11" ~ "High School or Less",
    educd == "12th grade, no diploma" ~ "High School or Less",
    educd == "ged or alternative credential" ~ "Trade Certificate",
    educd == "1 or more years of college credit, no degree" ~ "College Degree or In Progress",
    educd == "some college, but less than 1 year" ~ "College Degree or In Progress",
    educd == "bachelor's degree" ~ "College Degree or In Progress",
    educd == "Associate Degree" ~ "Associate Degree or In Progress",
    educd == "associate's degree, type not specified" ~ "Associate Degree or In Progress",
    educd == "Completed some graduate, but no degree" ~ "Masters/Doctorate Degree or In Progress",
    educd == "masters degree" ~ "Masters/Doctorate Degree or In Progress",
    educd == "Doctorate degree" ~ "Masters/Doctorate Degree or In Progress",
    educd == "professional degree beyond a bachelor's degree" ~ "Masters/Doctorate Degree or In Progress",
    educd == "doctoral degree" ~ "Masters/Doctorate Degree or In Progress",
    educd == "no schooling completed" ~ "No schooling completed"
  )) %>%
# Group the race/ethnicity variable into more succinct groups. More specifically 
# grouping into the following: "White", "Black, or African American",
# "American Indian or Alaska Native", "Asian", "Pacific Islander" and
# "Some other race"  
  mutate(
    race_grouped = case_when(
      raced == "white" ~ "White",
      raced == "white and asian" ~ "White",
      raced == "white and chinese" ~ "White",
      raced == "white and japanese" ~ "White",
      raced == "white and filipino" ~ "White",
      raced == "white and asian indian" ~ "White",
      raced == "white and korean" ~ "White",
      raced == "white and vietnamese" ~ "White",
      raced == "white and other asian race(s)" ~ "White",
      raced == "white and native hawaiian" ~ "White",
      raced == "white and guamanian" ~ "White",
      raced == "white and pi write_in" ~ "White",
      raced == "white and other race write_in" ~ "White",
      raced == "white, black, aian" ~ "White",
      raced == "white, black, asian" ~ "White",
      raced == "white, black, other race write_in" ~ "White",
      raced == "white, aian, asian" ~ "White",
      raced == "white, aian, other race write_in" ~ "White",
      raced == "white, chinese, hawaiian" ~ "White",
      raced == "white, chinese, filipino, hawaiian (2000 1%)" ~ "White",
      raced == "white, japanese, hawaiian (2000 1%)" ~ "White",
      raced == "white, aian and filipino" ~ "White",
      raced == "white, black, aian, asian, pi, other race write_in" ~ "White",
      raced == "white, asian, pi, other race write_in" ~ "White",
      raced == "black/african american/negro" ~ "Black, or African American",
      raced == "white and black" ~ "Black, or African American",
      raced == "black/african american/negro" ~ "Black, or African American",
      raced == "black and aian" ~ "Black, or African American",
      raced == "black and chinese" ~ "Black, or African American",
      raced == "black and filipino" ~ "Black, or African American",
      raced == "black and japanese" ~ "Black, or African American",
      raced == "black and asian indian" ~ "Black, or African American",
      raced == "black and korean" ~ "Black, or African American",
      raced == "black and asian write_in" ~ "Black, or African American",
      raced == "black and other asian race(s)" ~ "Black, or African American",
      raced == "black and other pi race(s)" ~ "Black, or African American",
      raced == "black and other race write_in" ~ "Black, or African American",
      raced == "blackfoot" ~ "American Indian or Alaska Native",
      raced == "apache" ~ "American Indian or Alaska Native",
      raced == "cherokee" ~ "American Indian or Alaska Native",
      raced == "cheyenne" ~ "American Indian or Alaska Native",
      raced == "chickasaw" ~ "American Indian or Alaska Native",
      raced == "chippewa" ~ "American Indian or Alaska Native",
      raced == "choctaw" ~ "American Indian or Alaska Native",
      raced == "creek" ~ "American Indian or Alaska Native",
      raced == "crow" ~ "American Indian or Alaska Native",
      raced == "iroquois" ~ "American Indian or Alaska Native",
      raced == "lumbee" ~ "American Indian or Alaska Native",
      raced == "navajo" ~ "American Indian or Alaska Native",
      raced == "potawatomi" ~ "American Indian or Alaska Native",
      raced == "pueblo" ~ "American Indian or Alaska Native",
      raced == "seminole" ~ "American Indian or Alaska Native",
      raced == "sioux" ~ "American Indian or Alaska Native",
      raced == "tohono o odham" ~ "American Indian or Alaska Native",
      raced == "hopi" ~ "American Indian or Alaska Native",
      raced == "puget sound salish" ~ "American Indian or Alaska Native",
      raced == "yaqui" ~ "American Indian or Alaska Native",
      raced == "south american indian" ~ "American Indian or Alaska Native",
      raced == "mexican american indian" ~ "American Indian or Alaska Native",
      raced == "south american indian" ~ "American Indian or Alaska Native",
      raced == "other amer. indian tribe (2000,acs)" ~ "American Indian or Alaska Native",
      raced == "2+ amer. indian tribes (2000,acs)" ~ "American Indian or Alaska Native",
      raced == "inupiat" ~ "American Indian or Alaska Native",
      raced == "yup'ik" ~ "American Indian or Alaska Native",
      raced == "other alaska native tribe(s) (2000,acs)" ~ "American Indian or Alaska Native",
      raced == "tribe not specified" ~ "American Indian or Alaska Native",
      raced == "native hawaiian or pi other race(s)" ~ "American Indian or Alaska Native",
      raced == "chinese" ~ "Asian",
      raced == "filipino" ~ "Asian",
      raced == "taiwanese" ~ "Asian",
      raced == "japanese" ~ "Asian",
      raced == "filipino" ~ "Asian",
      raced == "asian indian (hindu 1920_1940)" ~ "Asian",
      raced == "korean" ~ "Asian",
      raced == "vietnamese" ~ "Asian",
      raced == "sri lankan" ~ "Asian",
      raced == "pakistani" ~ "Asian",
      raced == "other asian, n.e.c" ~ "Asian",
      raced == "chinese and japanese" ~ "Asian",
      raced == "chinese and filipino" ~ "Asian",
      raced == "chinese and vietnamese" ~ "Asian",
      raced == "japanese and filipino" ~ "Asian",
      raced == "asian indian and asian write_in" ~ "Asian",
      raced == "other asian race combinations" ~ "Asian",
      raced == "aian and filipino (2000 1%)" ~ "Asian",
      raced == "aian and other race write_in" ~ "Asian",
      raced == "chinese and hawaiian" ~ "Asian",
      raced == "filipino and hawaiian" ~ "Asian",
      raced == "filipino and pi write_in" ~ "Asian",
      raced == "filipino and other race write_in" ~ "Asian",
      raced == "asian indian and other race write_in" ~ "Asian",
      raced == "chinese and korean" ~ "Asian",
      raced == "samoan" ~ "Pacific Islander",
      raced == "guamanian/chamorro" ~ "Pacific Islander",
      raced == "1+ other micronesian races (2000,acs)" ~ "Pacific Islander",
      raced == "fijian" ~ "Pacific Islander",
      raced == "pacific islander, n.s" ~ "Pacific Islander",
      raced == "other race, n.e.c" ~ "Some other race",
      raced == "Some other race" ~ "Some other race",
      raced == "Some other race" ~ "Some other race",
    )
  )

# The following code is used to create cell level predictions
# We use for loops in order to iterate through the data and create a matrix with
# all possible combinations for each variable.

ages <- unique(grouped_data2$age_grouped)
counti <- length(ages)
races <- unique(grouped_data2$race_grouped)
countj <- length(races)
educations <- unique(grouped_data2$education_grouped)
county <- length(educations) 
final <- matrix(0,150,4)
z <- 1
for(i in 1:counti) {
  for(j in 1:countj ){
    for(y in 1:county) {
      toCount <- filter(grouped_data2, grouped_data2$age_grouped == ages[i], 
                        grouped_data2$race_grouped == races[j], 
                        grouped_data2$education_grouped == educations[y])
      final[z,1] = ages[i]
      final[z,2] = races[j]
      final[z,3] = educations[y]
      final[z,4] <- nrow(toCount)
      z <- z+1
    }
  }
}

# Count how many of each iteration there are and create proportions 

counter <- as.numeric(final[,4])
final
counter
total <- sum(counter)
proportion <- counter/total
proportion

# Convert the matrix into a data frame
matrix_table <- as.data.frame(final)

# Construct a data frame that includes the variables of interest and the 
# corresponding counts and proportions for the given cell
post_strat_dat <- tibble(age_grouped = matrix_table$V1, 
                         race_ethnicity = matrix_table$V2,
                         education = matrix_table$V3, 
                         count = matrix_table$V4, proportion)

# Write the above data set into a csv to more easily access the data set when
# conducting further analysis
write_csv(post_strat_dat, "clean_post_strat_dat.csv")