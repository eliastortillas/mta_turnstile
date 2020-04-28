# Call the necessary lirbaries
# Primarily using dplyr and rest of tidyverse
library(tidyverse)
library(readxl)

## IMPORT AND CLEAN THE DATA

setwd("~/interactives/mta_turnstile") 
list.files()
mta <- read_excel("/Users/eliasguerra/interactives/mta_turnstile/data/turnstile-200222-200327.xlsx") # read_excel from readr
str(mta)

# mta_14st <- filter(mta, STATION == "14 ST-UNION SQ")
# write_csv(mta_14st, "/Users/eliasguerra/interactives/mta-turnstile/data/mta_example14st.csv")

# clean up the date and time
mta_date <- as.character.Date(mta$DATE) %>% str_sub(1,10)
mta_time <- as.character(mta$TIME) %>% str_sub(12,21)
mta_dt <- paste(mta_date,"T",mta_time, sep = "") %>% lubridate::ymd_hms()

# (show what they look like here)
# idk why i make this another step i just like it this way

mta$date_time <- mta_dt
mta$just_date <- mta_date
mta$just_time <- mta_time

# new look, same great taste
mta$date_time[2]


# Create new column "new.entries" and "new.exits" 
# calculated from cumulative "ENTRIES"/"EXITS" for each station
new_entries <- vector(length = nrow(mta))
new_exits <- vector(length = nrow(mta))
new_scp <- vector(length = nrow(mta)) # new_scp indicates device ID (dif turnstile?)
for (i in 1:nrow(mta)) {
  new_entries[i] <- mta$ENTRIES[i+1] - mta$ENTRIES[i]
  new_exits[i] <- mta$EXITS[i+1] - mta$EXITS[i]
  new_scp[i] <- mta$SCP[i+1] != mta$SCP[i] 
}

mta$new_entries <- new_entries # Not the same as "ENTRIES" 
mta$new_exits <- new_exits
new_scp[1] <- TRUE
mta$new_scp <- new_scp



# Notice that the negative new entries/exits match new_scp (and I fixed one in line 43)
mta_cleanr <- mta %>%
  filter(new_scp == F) %>% 
  select(date_time,STATION, LINENAME, SCP, ENTRIES, new_entries, new_exits, just_date, just_time)
head(mta_cleanr, 10)

#This will take the total rides per day and the average rides every four hours. 
mta_perday <-
  mta_cleanr %>%
  group_by(STATION, just_date) %>%
  summarise(avg_entries = mean(new_entries), # avg is four each unit of time data was collected, four hours
            total_entries = sum(new_entries),
            avg_exits = mean(new_exits),
            total_exits = sum(new_exits)) %>%
  filter(just_date != "1899-12-31") # This date gets included for some reason
head(mta_perday, 10)

# We have a our data in a form we can use now.
head(mta_cleanr)

# Time to graph to it.
mta_perday %>% filter(STATION == "14 ST-UNION SQ") %>%
  mutate(month_day = str_sub(just_date, 6,10)) %>%
  ggplot(aes(x = month_day, y = total_entries)) +
  geom_bar(stat = "identity") +
  ylab("Daily turnstiles entries") + xlab("Date") + 
  ggtitle("Total daily turnstile entries at 14-Street Union Sq.") +
  coord_flip() 


# MTA geocoded data from Christopher Whong 
# https://github.com/chriswhong/nycturnstiles/blob/master/geocoded.csv
data_files <- list.files("~/interactives/mta_turnstile/data")
setwd("~/interactives/mta_turnstile/data")
geo_income <- read_csv(data_files[1])
View(head(geo_income))
View(head(mta_perday))
select(geo_income, -the_geom)

mta_stations <- mta_perday$STATION %>% unique %>% data_frame(station_names=.) 
geo_station <- geo_income$station_name %>% unique %>% data_frame(station_names=.)
overlapping_stations <- c(mta_stations$station_names, geo_station$station_names)
unique(overlapping_stations) %>% sort()
  

# Average traffic at station per day
# Compare cumulative hourly over a few days