

#set your working directory
setwd("~/RSTUDIO FILES/R-WORKSHOP")

library(tidyverse)

#reading in your data

df <- read.csv("Wafanyikazi.csv",
               stringsAsFactors = F,
               header = T)

##### COLNAMES####

#checking for column names
colnames(df)



#######TIDYVERSE #######
#The following are must know functions in Tidyverse package as they will form your day to day data wrangling/management codes.


#######SELECT ######

#using the select ()to pick two variables

heist <- df %>%
  select(Age, Gender)

#checking for variables starting with letter "a"
starts_a <- df %>%
  select(starts_with("A"))

#contains letter "a"
contains_a <- df %>%
  select(contains("A"))

#end with ends with letter "a"
ends_a <- df %>%
  select(ends_with("t"))




###### FILTERING #####

#filter females only
female_data <- df %>%
  filter(Gender == "Female")

#filter age 30 and below
age_30 <- df %>%
  filter(Age <= 30)

#filter age 31 and above
age_31 <- df %>%
  filter(Age >= 31)


#filter promotion
female_data <- df %>%
  filter(Gender == "Female")



#filter females nyeri age< 26 from nyeri county earn <5000
female_nyeri <- df %>%
  filter(Gender == "Female" & County == "Nyeri" &
           Age >= 26 & Income < 5000)


#filter females and exclude the juniors

females_NoJnr <- df %>%
  filter(Gender == "Female" & Income > 6000 & Role != "Junior")


##### MUTATE #####


income_df <- df %>%
  select(Income) %>%
  mutate(Income2 = Income * 100)



#creating a swahili gender col
swahili <- df %>%
  select(Gender) %>%
  mutate(jinsia =
           if_else(Gender == "Male", "Mme", "Mke"))



#### IFELSE #####

#Nested Ifelse
##Department
dep_ifesle <- df %>%
  select(Department) %>%
  mutate(Department2 =
           ifelse(
             Department == "Finance",
             'F',
             ifelse(
               Department == "Research Analyst" ,
               "R",
               ifelse(
                 Department == "Operations",
                 "O",
                 ifelse(Department == "Associate", "A", "D")
               )
             )
           ))




#creating age Groups
age_group <- df %>%
  select(Age) %>%
  mutate(age2 =
           ifelse(Age < 25, "Young",
                  ifelse(Age > 25 & Age < 40, "Middle", "old")))

##### Renaming #####

#rename age to Miaka(swahili)
age_miaka <- df %>%
  rename(miaka = Age)


#Group_by & Summarise

##Group according to Gender and get the summary stats

gender_group <- df %>%
  group_by(Gender) %>%
  summarise(mean_age = mean(Age))

#group according to Department and get the summary stats

Depart_group <- df %>%
  group_by(Department) %>%
  summarise(mean_age = mean(Age))

#group according to county and get the summary stats

county_group <- df %>%
  group_by(County) %>%
  summarise(
    minimum = min(Income),
    max = max(Income),
    mean = mean(Income)
  )
