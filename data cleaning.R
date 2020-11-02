#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://www.voters
# tudygroup.org/
# Author: Hanyang Liao
# Data: 30 October 2020
# Contact: hanyang.liao@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


###Clean up survey data
library(haven)
library(tidyverse)
setwd("C:/Users/18729/Desktop/ns20200625")
# Read in the raw data
raw_data_survey <- read_dta("ns20200625.dta")
# Add the labels
raw_data_survey <- labelled::to_factor(raw_data_survey)
# Just keep some variables
data_survey <- 
  raw_data_survey %>% 
  select(registration,
         vote_intention,
         vote_2020,
         employment,
         gender,
         census_region,
         race_ethnicity,
         household_income,
         education,
         state,
         age)
# check data type
str(data_survey)
# filter the survey data by registeration of respondents
data_survey$age <- as.numeric(data_survey$age)
data_survey <- data_survey %>%
  filter(registration=="Registered"& (vote_2020=="Donald Trump"|vote_2020=="Joe Biden"))
# remove NA data
data_survey <- na.omit(data_survey)

###Clean up census data
# Read in the raw data
raw_data_census <- read_dta("usa_00003.dta.gz")
# Add the labels
raw_data_census <- labelled::to_factor(raw_data_census)
# Just keep some variables that may be of interest
data_census <- 
  raw_data_census %>% 
  select(perwt,
         region,
         stateicp,
         sex, 
         age, 
         race, 
         citizen,
         educd,
         empstat,
         hhincome)
# filter observations(only citizen more than 18 years old can vote)
unique(data_census$citizen)
data_census$age <- as.numeric(data_census$age)
data_census <- data_census %>%
  filter(age >= 18 & (citizen=="naturalized citizen"|
            citizen=="born abroad of american parents"))
# adjust NA values
data_census$hhincome <- ifelse(data_census$hhincome==9999999,NaN,
                               data_census$hhincome)
# remove NA values
data_census <- na.omit(data_census)
# remove raw data
rm(raw_data_census)
rm(raw_data_survey)


###map data between data_survey and data_census
##map region variable
#check variables
unique(data_survey$census_region)
unique(data_census$region)
#mapping
region.midwest <- c("east north central div","west north central div")
region.south <- c("south atlantic division","east south central div","west south central div")
region.west <- c("mountain division","pacific division")
region.northeast <- c("new england division","middle atlantic division")
data_census <- data_census %>%
  mutate(region1 = case_when(region %in% region.midwest ~ "Midwest",
                             region %in% region.south ~ "South",
                             region %in% region.west ~ "West",
                             region %in% region.northeast ~ "Northeast"))
data_census$region1 <- as.factor(data_census$region1)
#check variables again
unique(data_survey$census_region)
unique(data_census$region1)
data_census$region <- NULL


##map state variable
#check variables
unique(data_survey$state)
unique(data_census$stateicp)
#mapping
data_census<-data_census %>% 
  mutate(state = case_when(stateicp=="alabama"~"AL",
                           stateicp=="alaska"~"AK",
                           stateicp=="arizona"~"AZ",
                           stateicp=="arkansas"~"AR",
                           stateicp=="california"~"CA",
                           stateicp=="colorado"~"CO",
                           stateicp=="connecticut"~"CT",
                           stateicp=="delaware"~"DE",
                           stateicp=="florida"~"FL",
                           stateicp=="georgia"~"GA",
                           stateicp=="hawaii"~"HI",
                           stateicp=="idaho"~"ID",
                           stateicp=="illinois"~"IL",
                           stateicp=="indiana"~"IN",
                           stateicp=="iowa"~"IA",
                           stateicp=="kansas"~"KS",
                           stateicp=="kentucky"~"KY",
                           stateicp=="louisiana"~"LA",
                           stateicp=="maine"~"ME",
                           stateicp=="maryland"~"MD",
                           stateicp=="massachusetts"~"MA",
                           stateicp=="michigan"~"MI",
                           stateicp=="minnesota"~"MN",
                           stateicp=="mississippi"~"MS",
                           stateicp=="missouri"~"MO",
                           stateicp=="montana"~"MT",
                           stateicp=="nebraska"~"NE",
                           stateicp=="nevada"~"NV",
                           stateicp=="new hampshire"~"NH",
                           stateicp=="new jersey"~"NJ",
                           stateicp=="new mexico"~"NM",
                           stateicp=="new york"~"NY",
                           stateicp=="north carolina"~"NC",
                           stateicp=="north dakota"~"ND",
                           stateicp=="ohio"~"OH",
                           stateicp=="oklahoma"~"OK",
                           stateicp=="oregon"~"OR",
                           stateicp=="pennsylvania"~"PA",
                           stateicp=="rhode island"~"RI",
                           stateicp=="south carolina"~"SC",
                           stateicp=="south dakota"~"SD",
                           stateicp=="tennessee"~"TN",
                           stateicp=="texas"~"TX",
                           stateicp=="utah"~"UT",
                           stateicp=="vermont"~"VT",
                           stateicp=="virginia"~"VA",
                           stateicp=="washington"~"WA",
                           stateicp=="west virginia"~"WV",
                           stateicp=="wisconsin"~"WI",
                           stateicp=="wyoming"~"WY",
                           stateicp=="district of columbia"~"DC"))
#check variables again
unique(data_survey$state)
unique(data_census$state)
data_census$stateicp <- NULL

##map sex variable
#check variables
unique(data_survey$gender)
unique(data_census$sex)
#mapping
data_census<-data_census %>% 
  mutate(gender = case_when(sex=="male"~'Male',
                            sex=="female"~'Female'))
data_census$gender<- as.factor(data_census$gender)
#check variables again
unique(data_survey$gender)
unique(data_census$gender)
data_census$sex <- NULL

##map race variable
#check variables
unique(data_survey$race_ethnicity)
unique(data_census$race)
#mapping
race.white <- c("White","white")
race.black <- c("Black, or African American","black/african american/negro")
race.asian.pacific <- c("Asian (Asian Indian)","Asian (Vietnamese)","Asian (Chinese)",
                        "Asian (Korean)","Asian (Japanese)","Asian (Filipino)","Asian (Other)",
                        "Pacific Islander (Native Hawaiian)","Pacific Islander (Other)",
                        "Pacific Islander (Samoan)","Pacific Islander (Guamanian)","chinese",
                        "other asian or pacific islander","japanese")
race.other <- c("Some other race","American Indian or Alaska Native","two major races",
                "other race, nec","three or more major races","american indian or alaska native")
data_survey<-data_survey %>%
  mutate(race = case_when(race_ethnicity %in% race.white ~ "White",
                          race_ethnicity %in% race.black ~ "Black",
                          race_ethnicity %in% race.asian.pacific ~ "Asian or Pacific Islander",
                          race_ethnicity %in% race.other ~ "Other"))
data_census<-data_census %>%
  mutate(race = case_when(race %in% race.white ~ "White",
                          race %in% race.black ~ "Black",
                          race %in% race.asian.pacific ~ "Asian or Pacific Islander",
                          race %in% race.other ~ "Other"))
#check variables again
unique(data_survey$race)
unique(data_census$race)
#delete original data
data_survey$race_ethnicity <- NULL

##Create age group
data_survey <- data_survey %>%
  mutate(age.group = case_when(age <=20 ~ '20 or less',
                               age > 20 & age <= 35 ~ '21 to 35',
                               age > 35 & age <= 50 ~ '35 to 50',
                               age > 50 & age <= 65 ~ '50 to 65',
                               age > 65 & age <= 80 ~ '65 to 80',
                               age > 80 ~ 'above 80'))
data_census<-data_census %>%
  mutate(age.group = case_when(age <=20 ~ '20 or less',
                               age  > 20 & age <= 35 ~ '21 to 35',
                               age  > 35 & age <= 50 ~ '35 to 50',
                               age  > 50 & age <= 65 ~ '50 to 65',
                               age  > 65 & age <= 80 ~ '65 to 80',
                               age  > 80 ~ 'above 80'))
data_survey$age.group <- as.factor(data_survey$age.group)
data_census$age.group <- as.factor(data_census$age.group)
#check categories in both data sets
unique(data_survey$age.group)
unique(data_census$age.group)
#delete original data
data_survey$age <- NULL
data_census$age <- NULL
##map highest education variable
#check education in both data sets
unique(data_survey$education)
unique(data_census$educd)
#education categories:
#1.Middle School or lower
#2.High school
#3.College/Bachelor
#4.Master or higher
middleschool_and_lower <- c("no schooling completed","nursery school, preschool",
                            "kindergarten","grade 1","grade 2","grade 3",
                            "grade 4","grade 5","grade 6","grade 7","grade 8",
                            "grade 9","3rd Grade or less","Middle School - Grades 4 - 8")
highschool <- c("grade 10","grade 11","12th grade, no diploma","regular high school diploma",
                "Completed some high school","High school graduate",
                "Other post high school vocational training")
college_bachelor <- c("ged or alternative credential","some college, but less than 1 year",
                      "1 or more years of college credit, no degree",
                      "associate's degree, type not specified","bachelor's degree",
                      "Completed some college, but no degree","Associate Degree",
                      "College Degree (such as B.A., B.S.)")
master_and_higher <-c("master's degree","professional degree beyond a bachelor's degree",
                      "doctoral degree","Completed some graduate, but no degree",
                      "Masters degree","Doctorate degree")
#Survey data
data_survey<-data_survey %>% 
  mutate(highest_edu = case_when(
    education %in% middleschool_and_lower ~ "Middleschool or lower",
    education %in% highschool ~ "High school",
    education %in% college_bachelor ~ "College/Bachelor",
    education %in% master_and_higher ~ "Master or higher"))
data_census<-data_census %>% 
  mutate(highest_edu = case_when(
    educd %in% middleschool_and_lower ~ "Middleschool or lower",
    educd %in% highschool ~ "High school",
    educd %in% college_bachelor ~ "College/Bachelor",
    educd %in% master_and_higher ~ "Master or higher"))
#check data again
unique(data_survey$highest_edu)
unique(data_census$highest_edu)
#delete original data
data_survey$education <- NULL
data_census$educd <- NULL

##map employment variable
#check data
unique(data_survey$employment)
unique(data_census$empstat)
#mapping
emp.true <- c("Full-time employed","Part-time employed","Self-employed")
emp.false <- c("Unemployed or temporarily on layoff","Homemaker","")
laborforce.false.other <- c("Retired","Student","Permanently disabled","Other:")
data_survey <- data_survey %>%
  mutate(empstat = case_when(employment %in% emp.true ~ "employed",
                             employment %in% emp.false ~ "unemployed",
                             employment %in% laborforce.false.other ~ 
                               "not in labor force"))
data_survey$empstat <- as.factor(data_survey$empstat)
#check data again
unique(data_survey$empstat)
unique(data_census$empstat)
#delete original data
data_survey$employment <- NULL

##map household income variable
min(data_census$hhincome)
max(data_census$hhincome)
data_census<-data_census %>% 
  mutate(household_income = 
           case_when(hhincome<=14999 ~ "Less than $14,999",
                     hhincome>=15000 & hhincome<=19999~"$15,000 to $19,999",
                     hhincome>=20000 & hhincome<=24999~"$20,000 to $24,999",
                     hhincome>=25000 & hhincome<=29999~"$25,000 to $29,999",
                     hhincome>=30000 & hhincome<=34999~"$30,000 to $34,999",
                     hhincome>=35000 & hhincome<=39999~"$35,000 to $39,999",
                     hhincome>=40000 & hhincome<=44999~"$40,000 to $44,999",
                     hhincome>=45000 & hhincome<=49999~"$45,000 to $49,999",
                     hhincome>=50000 & hhincome<=54999~"$50,000 to $54,999",
                     hhincome>=55000 & hhincome<=59999~"$55,000 to $59,999",
                     hhincome>=60000 & hhincome<=64999~"$60,000 to $64,999",
                     hhincome>=65000 & hhincome<=69999~"$65,000 to $69,999",
                     hhincome>=70000 & hhincome<=74999~"$70,000 to $74,999",
                     hhincome>=75000 & hhincome<=79999~"$75,000 to $79,999",
                     hhincome>=80000 & hhincome<=84999~"$80,000 to $84,999",
                     hhincome>=85000 & hhincome<=89999~"$85,000 to $89,999",
                     hhincome>=90000 & hhincome<=94999~"$90,000 to $94,999",
                     hhincome>=95000 & hhincome<=99999~"$95,000 to $99,999",
                     hhincome>=100000 & hhincome<=124999~"$100,000 to $124,999",
                     hhincome>=125000 & hhincome<=149999~"$125,000 to $149,999",
                     hhincome>=150000 & hhincome<=174999~"$150,000 to $174,999",
                     hhincome>=175000 & hhincome<=199999~"$175,000 to $199,999",
                     hhincome>=200000 & hhincome<=249999~"$200,000 to $249,999",
                     hhincome>=250000~"$250,000 and above"
                     )) 
data_census$household_income <- as.factor(data_census$household_income)
#check data again
unique(data_survey$household_income)
unique(data_census$household_income)
#delete original data
data_census$hhincome <- NULL

###drop useless variable
data_survey$registration <- NULL
data_survey$vote_intention <- NULL
data_census$citizen <- NULL

###output csv
write_csv(data_survey, "data_survey.csv")
write_csv(data_census, "data_census.csv")

