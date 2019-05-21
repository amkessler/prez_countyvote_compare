library(tidyverse)
library(janitor)
library(lubridate)

rawdata <- read_csv("countypres_2000-2016.csv", 
                        col_types = cols(FIPS = col_character(), 
                        version = col_character(), year = col_character()))


#clean up column names and create new dataframe to work with
prezcounties <- rawdata %>% 
  clean_names()

#list the columns and some sample data from each
glimpse(prezcounties)


#limit to:
#--Ds and Rs for party
#--2008, 2012, 2018 cycles
#--No records where the county includes "District" (to filter out CD rows)
#--No records where state code is NA (Empty)
prezcounties <- prezcounties %>% 
  filter(party %in% c("democrat", "republican"),
         year %in% c("2008", "2012", "2016"),
         !str_detect(county, "District"),
         !is.na(state_po))


#check to see if filtering worked
prezcounties %>% 
  count(year)

prezcounties %>% 
  count(party)


#winnow down to just the needed columns
prezcounties <- prezcounties %>% 
  select(year, state_po, county, fips, party, candidatevotes, totalvotes)

#reshape the data to wide - put R and D results on same row ####
prezcounties_wide <- prezcounties %>% 
  spread(party, candidatevotes)

#create column that calculates a party winner 
prezcounties_wide <- prezcounties_wide %>% 
  mutate(winner = if_else(democrat > republican, "D", "R"))

#calculate pct of vote and then pct point margin of victory
prezcounties_wide <- prezcounties_wide %>% 
  mutate(
    pct_d = (democrat/totalvotes)*100,
    pct_r = (republican/totalvotes)*100,
    margin = round_half_up(abs(pct_d - pct_r),2)
  )
  

#reshape again, this time to get all three cycles going across ####
#with the party winners listed
three_cycles_across <- prezcounties_wide %>% 
  select(year, county, state_po, fips, winner) %>%
  spread(year, winner) %>% 
  clean_names() %>% #to take care of column names that are all numbers (adds a preceding 'x')
  arrange(state_po, county) #sort by state, then within each state by county 
  
#let's see what this looks like
three_cycles_across

#now we'll do something similar but this time for the margin victories for each cycle
margins_across <- prezcounties_wide %>% 
  select(year, county, state_po, fips, margin) %>%
  spread(year, margin) %>% 
  clean_names() %>% #to take care of column names that are all numbers (adds a preceding 'x')
  arrange(state_po, county) %>%  #sort by state, then within each state by county 
  select(fips, x2008_margin = x2008, x2012_margin = x2012, x2016_margin = x2016)

margins_across

#join to two tables together
final <- inner_join(three_cycles_across, margins_across)


#now that we have these results formatted, it's just a matter of
#filtering based on the winner of each election

#find counties with two obama wins and then a trump win
final %>% 
  filter(
    x2008 == "D", #Obama wins
    x2012 == "D", #Obama wins
    x2016 == "R"  #Trump wins
  )

#save the result with a new name
final_twoobama_trump <- final %>% 
  filter(
    x2008 == "D", #Obama wins
    x2012 == "D", #Obama wins
    x2016 == "R"  #Trump wins
  )

#export to a file
write_csv(final_twoobama_trump, "final_twoobama_trump.csv")

#now we'll do a similar one, but this time if Obama won EITHER in 2008 or 2012
final_oneobama_trump <- final %>% 
  filter(
    (x2008 == "D" | x2012 == "D"), #Obama in either 2008 or 2012 - the "|" means "or"
    x2016 == "R"  #Trump wins
    )

#export to a file
write_csv(final_oneobama_trump, "final_oneobama_trump.csv")
