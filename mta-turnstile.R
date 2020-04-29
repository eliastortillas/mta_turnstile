# Call the necessary lirbaries
# Primarily using dplyr and rest of tidyverse
library(tidyverse)
library(readxl)

## IMPORT AND CLEAN THE DATA

list.files("~/interactives/mta_turnstile")
mta <- read_excel("~/interactives/mta_turnstile/data/turnstile-200222-200327.xlsx") # read_excel from readr
str(mta)
View(head(mta, 1000))
View(sample_n(mta, 1000) )

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
  select(date_time,
         station_name = STATION, 
         train_lines = LINENAME, 
         scp = SCP, 
         entries_obsolete= ENTRIES, 
         entries = new_entries, 
         exits = new_exits, 
         date = just_date, 
         time = just_time)
head(mta_cleanr, 10)
# write_csv(mta_cleanr, "~/interactives/mta_turnstile/data/export/mta_clean.csv")

# We have a our data in a form we can use now.
head(mta_cleanr)
# write_csv(mta_cleanr, "~/interactives/mta_turnstile/data/export/mta_clean.csv") 
# Only running ^this once for now in case you want to access it and then download it urself
# mta_cleeeean <- read_csv("~/interactives/mta_turnstile/data/export/mta_clean.csv")

#This will take the total rides per day and the average rides every four hours. 
mta_perday <-
  mta_cleanr %>%
  group_by(station_name, date) %>%
  summarise(avg_entries = mean(entries), # avg is four each unit of time data was collected, four hours
            total_entries = sum(entries),
            avg_exits = mean(exits),
            total_exits = sum(exits)) %>%
  filter(date != "1899-12-31") # This date gets included for some reason
head(mta_perday, 10)



# Time to graph to it.
mta_perday %>% filter(station_name == "14 ST-UNION SQ") %>%
  mutate(month_day = str_sub(date, 6,10)) %>%
  ggplot(aes(x = month_day, y = total_entries)) +
  geom_bar(stat = "identity") +
  ylab("Daily turnstiles entries") + xlab("Date") + 
  ggtitle("Total daily turnstile entries at 14-Street Union Sq.") +
  coord_flip() 


# Going to add the geolocated-income data from Sam
geo_income <- read_csv("~/interactives/mta_turnstile/data/2018-med-income-ACS_by_subway-station - 2018-med-income-ACS_by_subway-station.csv")
geo_income$income <- geo_income$ct_median_income_2018_ACS %>% 
  str_remove("[:punct:]") %>%
  str_sub(2)



mta_stations <- mta_cleanr$station_name %>% unique %>% data_frame(station_name=.) 
geo_station <- geo_income$station_name %>% unique %>% data_frame(station_name=.)
both_station_names <- c(mta_stations$station_name, geo_station$station_name) %>% sort()
both_station_names

# Let's look at stations that include "PARKSIDE"
both_station_names[str_which(both_station_names, "PARKSIDE")] 

#need to switch "av" to "ave" in mta data. idk why this is so hard.
# First we need to check how "av" is written and not correct the "aves"
mta_cleanr$ave_true <- FALSE
mta_cleanr$ave_true[str_which(mta_cleanr$station_name, "AVE")] <- TRUE
mta_cleanr$av_true <- FALSE
mta_cleanr$av_true[str_which(mta_cleanr$station_name, "AV")] <- TRUE 
mta_cleanr$av_not_ave <- FALSE
mta_cleanr$av_not_ave[which(mta_cleanr$av_true == TRUE & mta_cleanr$ave_true == FALSE)] <- TRUE
mta_cleanr$station_name[mta_cleanr$av_not_ave == T] <-
  str_replace_all(mta_cleanr$station_name[mta_cleanr$av_not_ave == T], "AV", "AVE")
mta_cleanr %>% 
  slice(str_which(mta_cleanr$station_name, "AVE")) %>% 
  select(date_time, station_name) %>% 
  sample_n(20)


mta_geo_messy <- full_join(x = mta_cleanr %>% select(date_time, station_name, train_lines, entries, exits, date, time, scp),
                     y = geo_income %>% select(station_name, borough,train_lines, long, lat, income , service, unit), 
                     by = c("station_name","train_lines"))
mta_geo <- mta_geo_messy %>% filter(!is.na(income) & !is.na(entries))
mta_geo_unjoined <- mta_geo_messy %>% filter(is.na(income) | is.na(entries))

# Now we've got a starting point for the turnstile data with geo-income. 

mtageo_day <- mta_geo_messy %>% 
  group_by(date, station_name, train_lines) %>%
  summarise(total_entries = sum(entries), 
            total_exits = sum(exits),
            income = income[1], 
            long= long[1], 
            lat = lat[1]) %>%
  filter(date == "2020-02-27" | date == "2020-03-27") %>% # We're going to compare these two days
  filter(!is.na(income) & !is.na(long) & !is.na(lat)) %>%
  arrange(station_name)

mta_daydif <- mtageo_day %>%  pivot_wider(names_from = date, values_from = c(total_entries, total_exits)) 
colnames(mta_daydif)  <- colnames(mta_daydif) %>% str_replace_all("-","_")  
mta_daydif <- mta_daydif %>% mutate(entry_dif = total_entries_2020_02_27 - total_entries_2020_03_27, 
                                    exit_dif = total_exits_2020_02_27 - total_exits_2020_03_27) %>%
  select(-c(total_entries_2020_02_27:total_exits_2020_03_27))

#Time to graph
mta_daydif$income <- as.numeric(mta_daydif$income)
ggplot(mta_daydif) + geom_point(aes(x=long, y = lat, size = entry_dif, color = income, alpha = .1)) + 
  theme(legend.position = "none")


#This isn't  what we want but it's a start. 
            



# I could have done this in the same data set but this way we're seeing all the calculations
# We're going to look at the difference in ridership between Feb and March


str(mtageo_day)
mtageo_day$income <- as.numeric(mtageo_day$income)
hist(as.numeric(mtageo_day$income))
  

  
  



# Average traffic at station per day
# Compare cumulative hourly over a few days