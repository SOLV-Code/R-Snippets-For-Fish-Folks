#I found this cheatsheet really helpful, eventually, after getting over the first hurdle. 
# https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

#But there is a lot more there than you need at the beginning. The most #powerful bits are 
# select / filter / rename / left_join

# This script has a simple illustration of all 4


# create 2 data frames
# typical situation: mismatched labels, different units, some variable for linking
df_a <- data.frame(SampleID = letters, Obs = sample(1:100,26))
df_a

df_b <- data.frame(Label = c("B","E","G","M","T"), Result = c(1000,2000,3000,4000, NA))
df_b

library(tidyverse)

# fix the data frames
# %>% just says "feed the result of the left side into 
#  the function on the right as the first argument"

df_a_fixed <- df_a %>% mutate(SampleID = tolower(SampleID)) %>% # fixes any upper/lower case inconsistencies, and removes "factor"  
                      rename(Obs_A= Obs) %>% # fix the col headings
                      dplyr::filter(!(SampleID %in% c("i","j","k") )) # filter out some records
  
df_a_fixed

df_b_fixed <- df_b %>% rename(SampleID = Label, Obs = Result) %>% # fix the col headings
                       mutate(SampleID = tolower(SampleID))   %>% # fix the encoding of the sample
                       mutate(Obs = Obs /1000) %>% # rescale the values to be consistent
                      rename(Obs_B = Obs) %>% 
                      na.omit()   # drop any rows with NA

df_b_fixed 


# now join them together


df_merged <- left_join(df_a_fixed,df_b_fixed, by = "SampleID")

df_merged 
