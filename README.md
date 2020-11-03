# US-2020-election-prediction
The following is an analysis that uses multilevel regression with post-stratification to predict that Joe Biden will win 10.2% of popular vote in 2020 US election.

Analysis was done with the help of two datsets. Namely the UCLA Nationscape Phase 2 June 25, 2020 survey and 2018 1-year ACS from IPUMS. These datasets are not included in the GitHub repository as it needs to be downloaded directly from the source.

To download the UCLA survey data visit https://www.voterstudygroup.org/publication/nationscape-data-set and request access to the Democracy Fund + UCLA Nationscape 'Full Data Set'. This will give you acess to a multitude of surveys. The survey used in the corresponding script (01-data_cleaning-UCLA-survey.R) is the Phase 2 June 25, 2020 survey entitled ns20200625.dta

To download the 2018 1-year ACS visit https://usa.ipums.org/usa/index.shtml to create an account with IPUMS. The  data used in the corresponding script (01-data_cleaning-ipums-census.R) is the 2018 1-year ACS with the variables of REGION, STATEICP, AGE, SEX, MARST, RACE, HISPAN, BPL, CITIZEN, EDUC, LABFORCE and INCTOT. This dataset was downloaded with 614347 observations, formatted as a .dta file.

Once these datasets are downloaded you are able to proceed. Code for cleaning the datasets and preparing post-stratification data are included. Also included is the corresponding report analysing the results of the model.