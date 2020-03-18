# APPLIED ECONOMETRICS
# Angel Henriquez | Eduardo Ruano
# Research Question----
# Does the implementation of more schools in a school district have an impact on scores?

# DiD Approach----
# Identification problem: imperfect mobility of schools
# fix by choosing 2 school districts that are at their max number of schools. That way school mobility only moves in 1 direction, satisfying the common trends assumption.

# changes only need to happen in one school district
# change in number of schools, control for number of students

# Preliminaries

#clear working space
rm(list = ls())

# call packages
library(stargazer)
library(maps)
library(ggplot2)
library(rvest)
library(plyr)
library(readxl)
library(dplyr)
library(foreign)
#setting working directory
setwd("~/Desktop/#ECON 173/ECON 173 Project")

cse <- function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)}

# turn off scientific notation except for big numbers. 
options(scipen = 9)

# ACT DATA----
# act 2016-2017
act17 <- read_excel("act17.xls")
act17$year <- "2017"
act17$cname <- tolower(act17$cname)
act17$dname <- tolower(act17$dname)
act17$sname <- tolower(act17$sname)

names(act17)[9] <- "enroll_12_2017"
names(act17)[10] <- "test_takers_2017"
names(act17)[11] <- "average_english_score_2017"
names(act17)[12] <- "average_reading_score_2017"
names(act17)[13] <- "average_math_score_2017"
names(act17)[14] <- "average_science_score_2017"
names(act17)[15] <- "number_proficient_2017"
names(act17)[16] <- "percent_proficient_2017"

# act data
# act 2015-2016
act16 <- read_excel("act16.xls")
act16$year <- "2016"
act16$cname <- tolower(act16$cname)
act16$dname <- tolower(act16$dname)
act16$sname <- tolower(act16$sname)

names(act16)[6] <- "enroll_12_2016"
names(act16)[7] <- "test_takers_2016"
names(act16)[8] <- "average_english_score_2016"
names(act16)[9] <- "average_reading_score_2016"
names(act16)[10] <- "average_math_score_2016"
names(act16)[11] <- "average_science_score_2016"
names(act16)[12] <- "number_proficient_2016"
names(act16)[13] <- "percent_proficient_2016"

# act data
# act 2014-2015
act15 <- read_excel("act15.xls")
act15$year <- "2015"
act15$cname <- tolower(act15$cname)
act15$dname <- tolower(act15$dname)
act15$sname <- tolower(act15$sname)

names(act15)[6] <- "enroll_12_2015"
names(act15)[7] <- "test_takers_2015"
names(act15)[8] <- "average_english_score_2015"
names(act15)[9] <- "average_reading_score_2015"
names(act15)[10] <- "average_math_score_2015"
names(act15)[11] <- "average_science_score_2015"
names(act15)[12] <- "number_proficient_2015"
names(act15)[13] <- "percent_proficient_2015"

# merging old school data----
# merging 2011-2012 and 2014-2015 school data based on school name and county name
all_act_data <- merge(act17,act16, by.x=c("sname", "dname", "cname"), by.y=c("sname", "dname", "cname"), all = TRUE)
all_act_data <- merge(all_act_data,act15, by.x=c("sname", "dname", "cname"), by.y=c("sname", "dname", "cname"), all = TRUE)

rm('act15')
rm('act16')
rm('act17')

# finding number of schools per county in 2017
num_school_counties_2017 <- all_act_data[c(1:7, 9:17)]
num_school_counties_2017 <- subset(num_school_counties_2017, num_school_counties_2017$year != "NA")
num_school_counties_2017 <- num_school_counties_2017 %>% group_by(cname) %>% mutate(num_school_counties_2017 = n())
num_school_counties_2017 <- num_school_counties_2017[c(3,17)]
num_school_counties_2017 <- num_school_counties_2017 %>% distinct()

# finding number of schools per county in 2016
num_school_counties_2016 <- all_act_data[c(1:7, 20:28)]
num_school_counties_2016 <- subset(num_school_counties_2016, num_school_counties_2016$year != "NA")
num_school_counties_2016 <- num_school_counties_2016 %>% group_by(cname) %>% mutate(num_school_counties_2016 = n())
num_school_counties_2016 <- num_school_counties_2016[c(3,17)]
num_school_counties_2016 <- num_school_counties_2016 %>% distinct()

# finding number of schools per county in 2015
num_school_counties_2015 <- all_act_data[c(1:7, 31:39)]
num_school_counties_2015 <- subset(num_school_counties_2015, num_school_counties_2015$year != "NA")
num_school_counties_2015 <- num_school_counties_2015 %>% group_by(cname) %>% mutate(num_school_counties_2015 = n())
num_school_counties_2015 <- num_school_counties_2015[c(3,17)]
num_school_counties_2015 <- num_school_counties_2015 %>% distinct()

all_num_school_counties <- merge(num_school_counties_2017, num_school_counties_2016 , by.x = c("cname"), by.y = c("cname"), all = TRUE)
all_num_school_counties <- merge(all_num_school_counties, num_school_counties_2015, by.x = c("cname"), by.y = c("cname"), all = TRUE)

rm('num_school_counties_2017')
rm('num_school_counties_2016')
rm('num_school_counties_2015')

# averaging county scores
# need to do this step afterwards because the county name/number variable is provided after merging
# calculating average ACT score proficiency per county

avg_perc_prof_2017 <- all_act_data[c(1:7, 9:17)]
avg_perc_prof_2017 <- subset(avg_perc_prof_2017, avg_perc_prof_2017$percent_proficient_2017 != "NA")
avg_perc_prof_2017$percent_proficient_2017 <- as.numeric(avg_perc_prof_2017$percent_proficient_2017)
avg_perc_prof_2017 <- subset(avg_perc_prof_2017, avg_perc_prof_2017$percent_proficient_2017 != "NA")
avg_perc_prof_2017 <- avg_perc_prof_2017 %>% group_by(cname) %>% mutate(avg_percent_profficient_2017 = mean(percent_proficient_2017))
avg_perc_prof_2017 <- avg_perc_prof_2017[c(3,17)]
avg_perc_prof_2017 <- avg_perc_prof_2017 %>% distinct()

avg_perc_prof_2016 <- all_act_data[c(1:7, 20:28)]
avg_perc_prof_2016 <- subset(avg_perc_prof_2016, avg_perc_prof_2016$percent_proficient_2016 != "NA")
avg_perc_prof_2016$percent_proficient_2016 <- as.numeric(avg_perc_prof_2016$percent_proficient_2016)
avg_perc_prof_2016 <- subset(avg_perc_prof_2016, avg_perc_prof_2016$percent_proficient_2016 != "NA")
avg_perc_prof_2016 <- avg_perc_prof_2016 %>% group_by(cname) %>% mutate(avg_percent_profficient_2016 = mean(percent_proficient_2016))
avg_perc_prof_2016 <- avg_perc_prof_2016[c(3,17)]
avg_perc_prof_2016 <- avg_perc_prof_2016 %>% distinct()

avg_perc_prof_2015 <- all_act_data[c(1:7, 31:39)]
avg_perc_prof_2015 <- subset(avg_perc_prof_2015, avg_perc_prof_2015$percent_proficient_2015 != "NA")
avg_perc_prof_2015$percent_proficient_2015 <- as.numeric(avg_perc_prof_2015$percent_proficient_2015)
avg_perc_prof_2015 <- subset(avg_perc_prof_2015, avg_perc_prof_2015$percent_proficient_2015 != "NA")
avg_perc_prof_2015 <- avg_perc_prof_2015 %>% group_by(cname) %>% mutate(avg_percent_profficient_2015 = mean(percent_proficient_2015))
avg_perc_prof_2015 <- avg_perc_prof_2015[c(3,17)]
avg_perc_prof_2015 <- avg_perc_prof_2015 %>% distinct()

all_avg_prof_scores <- merge(avg_perc_prof_2017, avg_perc_prof_2016, by.x=c("cname"), by.y = c("cname"), all = TRUE)
all_avg_prof_scores <- merge(all_avg_prof_scores, avg_perc_prof_2015, by.x=c("cname"), by.y = c("cname"), all = TRUE)

rm('avg_perc_prof_2017')
rm('avg_perc_prof_2016')
rm('avg_perc_prof_2015')

# High School Population Data

# Available High School Age Bracket population data (14 to 17 yrs old)
# government data file source: https://www.cde.ca.gov/ds/sd/sd/filesenr.asp

# 2016-2017 High School Population Data
school_enrollment_2016.2017 <- read.delim("~/Desktop/#ECON 173/ECON 173 Project/school_enrollment_2016_2017.txt")
school_enrollment_2016.2017 <- subset(school_enrollment_2016.2017, GR_9 != 0 & GR_10 != 0 & GR_11 != 0 & GR_12 != 0)
# school total enrollments for 2017 - 2018 school year
# grouping variables by CDS number
school_enrollment_2016.2017 <- school_enrollment_2016.2017[c(1,22)]
# number of test takers per county
high_school_enrollment_2016.2017 <- school_enrollment_2016.2017 %>% 
  dplyr::group_by(CDS_CODE) %>% 
  summarise_all(funs(sum))
rm('school_enrollment_2016.2017')

# 2015-2016 High School Population Data
school_enrollment_2015.2016 <- read.delim("~/Desktop/#ECON 173/ECON 173 Project/school_enrollment_2015_2016.txt")
school_enrollment_2015.2016 <- subset(school_enrollment_2015.2016, GR_9 != 0 & GR_10 != 0 & GR_11 != 0 & GR_12 != 0)
# school total enrollments for 2017 - 2018 school year
# grouping variables by CDS number
school_enrollment_2015.2016 <- school_enrollment_2015.2016[c(1,22)]
# number of test takers per county
high_school_enrollment_2015.2016 <- school_enrollment_2015.2016 %>% 
  dplyr::group_by(CDS_CODE) %>% 
  summarise_all(funs(sum))
rm('school_enrollment_2015.2016')

# 2014-2015 High School Population Data
school_enrollment_2014.2015 <- read.delim("~/Desktop/#ECON 173/ECON 173 Project/school_enrollment_2014_2015.txt")
school_enrollment_2014.2015 <- subset(school_enrollment_2014.2015, GR_9 != 0 & GR_10 != 0 & GR_11 != 0 & GR_12 != 0)
# school total enrollments for 2014 - 2015 school year
# grouping variables by CDS number
school_enrollment_2014.2015 <- school_enrollment_2014.2015[c(1,22)]
# number of test takers per county
high_school_enrollment_2014.2015 <- school_enrollment_2014.2015 %>% 
  dplyr::group_by(CDS_CODE) %>% 
  summarise_all(funs(sum))
rm('school_enrollment_2014.2015')

# renaming columns to reflect years
colnames(high_school_enrollment_2016.2017)[2] <- "enrollment_2016_2017"
colnames(high_school_enrollment_2015.2016)[2] <- "enrollment_2015_2016"
colnames(high_school_enrollment_2014.2015)[2] <- "enrollment_2014_2015"

# merging high school enrollments by cds code
all_enrollment_data <- merge(high_school_enrollment_2016.2017, high_school_enrollment_2015.2016, by.x = c("CDS_CODE"), by.y = c("CDS_CODE"), all = TRUE)
all_enrollment_data <- merge(all_enrollment_data, high_school_enrollment_2014.2015, by.x = c("CDS_CODE"), by.y = c("CDS_CODE"), all = TRUE)

rm('high_school_enrollment_2014.2015')
rm('high_school_enrollment_2015.2016')
rm('high_school_enrollment_2016.2017')

# merging using cds code
enroll_data_merger_part1 <- all_act_data[c(1:4)]
enroll_data_merger_part2 <- all_act_data[c(1:3,18)]
enroll_data_merger_part3 <- merge(enroll_data_merger_part1,enroll_data_merger_part2, by.x = c("sname","dname","cname","cds.x"), by.y = c("sname","dname","cname","cds.y"), all = TRUE)
enroll_data_merger_part4 <- subset(enroll_data_merger_part3, enroll_data_merger_part3$cds.x != "NA")
enroll_data_merger_part5 <- enroll_data_merger_part4[c(3,4)]
enroll_data_merger_part5$cds.x <- as.numeric(enroll_data_merger_part5$cds.x)
enroll_data_merger_part5$county_code <- floor(enroll_data_merger_part5$cds.x/1000000000000)
all_enrollment_data$county_code <- floor(all_enrollment_data$CDS_CODE/1000000000000)
enrollment_data <- merge(enroll_data_merger_part5, all_enrollment_data, by.x = c("county_code"), by.y = c("county_code"), all = TRUE)
# cleaning dataset
enrollment_data$enrollment_2016_2017[is.na(enrollment_data$enrollment_2016_2017)] <- 0
enrollment_data$enrollment_2015_2016[is.na(enrollment_data$enrollment_2015_2016)] <- 0
enrollment_data$enrollment_2014_2015[is.na(enrollment_data$enrollment_2014_2015)] <- 0

enrollment_data <- enrollment_data %>% 
  dplyr::group_by(cname) %>% 
  summarise_all(funs(sum))

# removing unnecessary columns
enrollment_data <- enrollment_data[c(1,5:7)]
# dividing enrollment to millions (1,000,000)
# enrollment data is in millions of students
enrollment_data$enrollment_2016_2017 = enrollment_data$enrollment_2016_2017/1000000
enrollment_data$enrollment_2015_2016 = enrollment_data$enrollment_2015_2016/1000000
enrollment_data$enrollment_2014_2015 = enrollment_data$enrollment_2014_2015/1000000

rm('enroll_data_merger_part1')
rm('enroll_data_merger_part2')
rm('enroll_data_merger_part3')
rm('enroll_data_merger_part4')
rm('enroll_data_merger_part5')
rm('all_enrollment_data')

all_project_data <- merge(enrollment_data, all_num_school_counties, by.x = c("cname"), by.y = c("cname"), all = TRUE)
all_project_data <- merge(all_project_data, all_avg_prof_scores, by.x = c("cname"), by.y = c("cname"), all = TRUE)

rm('enrollment_data')
rm('all_num_school_counties')
rm('avg_perc_prof_2015')
rm('avg_perc_prof_2016')
rm('avg_perc_prof_2017')
rm('all_avg_prof_scores')

# making subsets of yearly data to merge onto a table
only_2017_data <- all_project_data[c(1,2,5,8)]
only_2016_data <- all_project_data[c(1,3,6,9)]
only_2015_data <- all_project_data[c(1,4,7,10)]

# naming year variables
only_2017_data$year <- "2017"
only_2016_data$year <- "2016"
only_2015_data$year <- "2015"

colnames(only_2017_data)[2] <- "total_enrollment"
colnames(only_2016_data)[2] <- "total_enrollment"
colnames(only_2015_data)[2] <- "total_enrollment"

colnames(only_2017_data)[3] <- "total_number_of_schools"
colnames(only_2016_data)[3] <- "total_number_of_schools"
colnames(only_2015_data)[3] <- "total_number_of_schools"

colnames(only_2017_data)[4] <- "perc_proficient"
colnames(only_2016_data)[4] <- "perc_proficient"
colnames(only_2015_data)[4] <- "perc_proficient"

# combined dataset will be used for diff-in-diff
combineddataset = rbind(only_2017_data, only_2016_data)
combineddataset = rbind(combineddataset, only_2015_data)

rm('only_2015_data')
rm('only_2016_data')
rm('only_2017_data')

# will use merced and humboldt
ps_data = subset(combineddataset, cname == "merced" | combineddataset$cname == "humboldt")

# Regression Formulas
# percent_proficient=β0+δ0post16+β1humb+δ1(humb×post96)+ε.
# percent_proficient=β0+δ0post16+β1humb+δ1(humb×post96)+β3total_enrollment+ε

# this is all for humboldt
ps_data = ps_data %>%
  mutate(post16 = year>= 2016,
         humb = cname == "humboldt",
         merc = cname == "merced")

model1 = lm(perc_proficient ~ humb*post16, data = ps_data)
model2 = lm(perc_proficient ~ humb*post16 + total_enrollment, data = ps_data)
summary(model1)
summary(model2)

stargazer(model1, model2, type = "text")

# plots to check trends of proficiency and enrollment changes
ggplot(ps_data, aes(year, total_number_of_schools, group = cname)) + geom_point() + geom_line() + labs(title = "Changes in Schools Trends from 2014-2015 to 2016-2017", y = "Number of Schools", x = "School Year")
ggplot(ps_data, aes(year, total_enrollment, group = cname)) + geom_point() + geom_line() + labs(title = "Changes in Enrollment Trends from 2014-2015 to 2016-2017", y = "Enrollment (millions of students)", x = "School Year")
