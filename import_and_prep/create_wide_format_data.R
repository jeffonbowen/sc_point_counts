### Prepare Wide Format Data for Analysis

# We have two types of point count surveys:
#     -   5-min fixed radius
#     -   10-min unlimited radius
# 
# There are various possible data sets that can be generated, depending on the 
# analysis needed. However there are three basic datasets that can be subset 
# if something different needed.
# 
# 1.  All data:
#   
# -   Both 5- and 10-min surveys and observations at all distances.
# -   Good for total richness.
# -   Can be used with QPAD since QPAD can accommodate the different durations and survey distances.
# -   Not good for average station richness or for rarefaction.
# 
# 2.  Fixed 100m / 5-min surveys.
# 
# -   Good for analysis over max number of years.
# -   Good for analyses that cannot accommodate varying survey distance and duration. For example rarefaction.
# -   Filter for songbirds only.
# -   Then reformat data to wide format for use in vegan: samples by species.
# -   To make the wide matrix, it is necessary to join up with the survey data. This is because there were some surveys with no species detections and therefore do not appear in the observations table.


# Prep -------------------------------------------------------------------

# Load libraries
library(tidyverse)

# Make list of songbirds to filter data on:
song_list <- BCbirds %>%
  filter(IsSongbird == TRUE)
song_list <-song_list$SpCode


# Create all duration / all distance data --------------------------------

# Create the filtered list of obs. Files written to 'data_processed' so that 
# subsequent analyses can load the data without running the scripts here.

song_wide_all <- obs %>% 
  left_join(surveys, by = c("StationID", "Visit")) %>% 
  #  filter(`Distance Category` != ">100") %>%
  filter(SpCode %in% song_list) %>% 
  filter(is.na(Flyovers)) %>% 
  group_by(StationID, Visit, SpCode) %>% 
  summarise(count = sum(Count)) %>% 
  pivot_wider(names_from = "SpCode", values_from = count, values_fill = 0) %>% 
  select(sort(names(.))) %>% 
  select(StationID, Visit, everything()) %>% 
  full_join(surveys, by = c("StationID", "Visit")) %>% 
  left_join(stations, by = "StationID") %>%
  select(StationID, Visit, Year, Footprint, `Mitigation Property`, Valley,
         ValleyUpstPine, ValleyUpstDam, BBFM_ID, 
         Easting, Northing, Longitude, Latitude, BHC20, Date, Time,
         `SurveyDuration`, `B-ALFL`:`B-YRWA`) %>% 
  replace(is.na(.), 0) %>% 
  arrange(StationID)

# write_csv(song_wide_all, "data_processed/song_wide_all.csv")


# Create the 5-min, fixed 100m counts --------------------------------------
  
song_wide_5min_100m <- obs %>% 
  left_join(surveys, by = c("StationID", "Visit")) %>% 
  rowwise() %>% 
  mutate(count5 = 
           case_when(year(Date) > 2016 ~ sum(c(`Count 0-3 min`, `Count 3-5 min`),
                                             na.rm = TRUE),
                     TRUE ~ `Count 5 min`)) %>% 
  filter(`Distance Category` != ">100") %>%
  filter(SpCode %in% song_list) %>% 
  filter(is.na(Flyovers)) %>% 
  group_by(StationID, Visit, SpCode) %>% 
  summarise(count = sum(count5)) %>% 
  pivot_wider(names_from = "SpCode", values_from = count, values_fill = 0) %>% 
  select(sort(names(.))) %>% 
  select(StationID, Visit, everything()) %>% 
  full_join(surveys, by = c("StationID", "Visit")) %>% 
  left_join(stations, by = "StationID") %>%
  select(StationID, Visit, Year, Footprint, `Mitigation Property`, Valley,
         ValleyUpstPine, ValleyUpstDam, BBFM_ID, 
         Easting, Northing, Longitude, Latitude, BHC20, Date, Time, 
         `SurveyDuration`, `B-ALFL`:`B-YRWA`) %>% 
  replace(is.na(.), 0) %>% 
  arrange(StationID)

# write_csv(song_wide_5min_100m, "data_processed/song_wide_5min_100m.csv")


# Create 5-minute unlimited -----------------------------------------------

song_wide_5min_unlim <- obs %>% 
  left_join(surveys, by = c("StationID", "Visit")) %>% 
  rowwise() %>% 
  mutate(count5 = 
           case_when(year(Date) > 2016 ~ sum(c(`Count 0-3 min`, `Count 3-5 min`),
                                             na.rm = TRUE),
                     TRUE ~ `Count 5 min`)) %>% 
  #  filter(`Distance Category` != ">100") %>%
  filter(SpCode %in% song_list) %>% 
  filter(is.na(Flyovers)) %>% 
  group_by(StationID, Visit, SpCode) %>% 
  summarise(count = sum(count5)) %>% 
  pivot_wider(names_from = "SpCode", values_from = count, values_fill = 0) %>% 
  select(sort(names(.))) %>% 
  select(StationID, Visit, everything()) %>% 
  full_join(surveys, by = c("StationID", "Visit")) %>% 
  left_join(stations, by = "StationID") %>%
  select(StationID, Visit, Year, Footprint, `Mitigation Property`, Valley,
         ValleyUpstPine, ValleyUpstDam, BBFM_ID, 
         Easting, Northing, Longitude, Latitude, BHC20, Date, Time, 
         `SurveyDuration`, `B-ALFL`:`B-YRWA`) %>% 
  replace(is.na(.), 0) %>% 
  arrange(StationID)

# write_csv(song_wide_5min_unlim, "data_processed/song_wide_5min_unlim.csv")
