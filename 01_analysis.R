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
  select(year, state_po, county, fips, party, candidatevotes)

#reshape the data to wide - put R and D results on same row ####
prezcounties_wide <- prezcounties %>% 
  spread(party, candidatevotes)

#create column that calculates a party winner 
prezcounties_wide <- prezcounties_wide %>% 
  mutate(winner = if_else(democrat > republican, "D", "R"))

#reshape again, this time to get all three cycles going across ####
#with the party winners listed
three_cycles_across <- prezcounties_wide %>% 
  select(year, county, state_po, fips, winner) %>%
  spread(year, winner) %>% 
  clean_names() #to take care of column names that are all numbers (adds a preceding 'x')

#let's see what this looks like
three_cycles_across

#now that we have these results formatted, it's just a matter of
#filtering based on the winner of each election

#find counties with two obama wins and then a trump win
three_cycles_across %>% 
  filter(
    x2008 == "D", #Obama wins
    x2012 == "D", #Obama wins
    x2016 == "R"  #Trump wins
  )

#save the result with a new name
finalresults <- three_cycles_across %>% 
  filter(
    x2008 == "D", #Obama wins
    x2012 == "D", #Obama wins
    x2016 == "R"  #Trump wins
  )

#export finalresults to a file
write_csv(finalresults, "finalresults.csv")