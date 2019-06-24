#goal: find counties that went ok for Rs in 2012 (still won), but great for Rs in 2016

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
#--Iowa
#--Ds and Rs for party
#--2012, 2016 cycles
#--No records where the county includes "District" (to filter out CD rows)
#--No records where state code is NA (Empty)
prezcounties <- prezcounties %>% 
  filter(state_po == "IA",
        party %in% c("democrat", "republican"),
         year %in% c("2012", "2016"),
         !str_detect(county, "District"))


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



#### NOW THE ANALYSIS OF JUST REPUBLICAN GOOD VS. GREAT ####

#filter on only R winners
gopwinners <- prezcounties_wide %>% 
  filter(winner == "R")

  
#reshape again, this time to put both 2012 and 2016 cycles going across ####
#margin victories for each cycle
margins_across <- gopwinners %>% 
  select(year, county, state_po, fips, margin) %>%
  spread(year, margin) %>% 
  clean_names() %>% #to take care of column names that are all numbers (adds a preceding 'x')
  arrange(state_po, county) %>%  #sort by state, then within each state by county 
  select(fips, county, x2012_margin = x2012, x2016_margin = x2016)

head(margins_across, 3)

#filter out NAs from 2012 where republican did not win
margins_across <- margins_across %>% 
  filter(!is.na(x2012_margin))


#now let's find counties with slim gop victories in 2012 but huge for Trump in 2016 ####
final_bigiowajumps <- margins_across %>% 
  filter(x2012_margin <= 12, #single digit win
         x2016_margin >=24) %>% 
  arrange(x2012_margin)

#rename margin columns for clarity
final_bigiowajumps <- final_bigiowajumps %>% 
  rename(
    gop2012_margin = x2012_margin,
    gop2016_margin = x2016_margin
  )

final_bigiowajumps

#export to a file
write_csv(final_bigiowajumps, "final_bigiowajumps.csv")
