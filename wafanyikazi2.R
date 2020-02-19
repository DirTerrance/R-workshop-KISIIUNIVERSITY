
library(tidyverse)



colnames(df)

#using the select ()to pick two variables

heist <- df %>%
  select(Age, Gender)


a <- df %>%
  select(starts_with("A"))

#contains
aa <- df %>%
  select(contains("A"))

#end with
aaa <- df %>%
  select(ends_with("t"))


#filter females
female_data <- df %>%
  filter(Gender=="Female")

#filter age 30
age <- df %>%
  filter(Age<=30)



#filter age 31
 age_31f <- df %>%
filter(Age>=31)


 #filter promotion
 female_data <- df %>%
   filter(Gender=="Female")




 #filter males
 #filter females nyeri age< 26 from nyeri county earn <5000
 female_nyeri <- df %>%
   filter(Gender=="Female"&County=="Nyeri"&Age>=26& Income <5000)


  #filter females
 fe <- df %>%
   filter(Gender=="Female"&Income >6000&Role!="Junior")


 #mutate
 income_df<- df %>%
   select(Income) %>%
   mutate(Income2 =Income*100)

 #mutate
 income_df<-df %>%
 select(Income) %>%
mutate(Income3=Income*1000)



 swahili<-df %>%
   select(Gender) %>%
   mutate(jinsia=
            if_else(Gender=="Male", "Mme", "Mke"))







