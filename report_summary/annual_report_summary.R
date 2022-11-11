#
### Summary of the data for the annual report ###
#


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)


# Load Data ---------------------------------------------------------------

# Use the import data script to bring the data in. 
# Then run the rest.
  
source("import_and_prep/import_point_count_data.R")


# Load the BHC names
BHC_lookup <- read_xlsx("../sc_common_files/data_reference/BHClookup.xlsx")


# Survey Summary ----------------------------------------------------------

# Number of songbird point count stations and surveys conducted in 2022 by bird habitat class.

# Set the summary year.
summary_year <- "2022"

surveys_out <- surveys %>% 
  left_join(stations, "StationID") %>%
  filter(Year == summary_year) %>% 
  left_join(BHC_lookup, "BHC20") %>% 
  group_by(`Bird Habitat Class`, Sort) %>% 
  summarise(stations = n_distinct(StationID),
            surveys = n()) %>% 
  arrange(Sort)

write_csv(surveys_out, "out/survey_count_2022.csv")


# Species Tally -----------------------------------------------------------

# Make list of songbirds to filter data on:
song_list <- 
  BCbirds %>%
  filter(IsSongbird == TRUE)

species_tally <- 
  obs %>% 
  left_join(stations, "StationID") %>% 
  filter(Year == summary_year) %>% 
  #  filter(SpCode %in% song_list$SpCode) %>% 
  group_by(SpCode) %>% 
  summarize(`Survey Detections` = sum(Count),
            `Incidental Detections` = sum(Flyovers)) %>% 
  left_join(BCbirds, "SpCode") %>% 
  mutate(Songbird = if_else(IsSongbird == TRUE, "Yes", ""),
         `Incidental Detections` = replace_na(`Incidental Detections`, 0)) %>% 
  select(sort, Songbird, `English Name`, `Scientific Name`, `BC List`, COSEWIC, 
         `SARA Status`, `Survey Detections`, `Incidental Detections`) %>% 
  arrange(sort) %>% 
  select(!sort)

write_csv(species_tally, "out/species_tally_2022.csv")


# Station Info -----------------------------------------------------------

station_tally <- stations %>% 
  left_join(surveys, "StationID") %>% 
  dplyr::filter(Year == summary_year) %>% 
  mutate(Time = format(Time, "%H:%M")) %>% 
  pivot_wider(StationID, names_from = Visit, values_from = c(Date, Time)) %>% 
  left_join(stations, "StationID") %>% 
  mutate(`UTM Zone` = "10") %>% 
  left_join(BHC_lookup, "BHC20") %>% 
  dplyr::select(StationID, "UTM Zone", "UTM Easting" = Easting, "UTM Northing" = Northing,
                "Survey 1 Date" = Date_1, "Survey 1 Time" = Time_1, 
                "Survey 2 Date" = Date_2, "Survey 2 Time" = Time_2,
                `Bird Habitat Class`)

write_csv(station_tally, "out/station_tally_2022.csv")


# Species Richness -------------------------------------------------------

survey_richness <- 
  obs %>% 
  left_join(stations, "StationID") %>% 
  filter(Year == summary_year) %>% 
  #  filter(SpCode %in% song_list$SpCode) %>% 
  group_by(StationID, Visit) %>% 
  summarize(n_distinct(SpCode))

summary(survey_richness)

# Species Tally by Year for Valley Upstream of Dam -----------------------

species_tally_all <- 
  obs %>% 
  dplyr::left_join(stations, "StationID") %>% 
  filter(SpCode %in% song_list$SpCode,
         ValleyUpstDam == "Yes") %>% 
  group_by(SpCode, Year) %>% 
  summarize(`Survey Detections` = sum(Count)) %>% 
  left_join(BCbirds, "SpCode") %>% 
  mutate(Songbird = if_else(IsSongbird == TRUE, "Yes", "")) %>% 
  dplyr::select(sort, Year, Songbird, `English Name`, `Scientific Name`, 
                `BC List`, COSEWIC, `SARA Status`, `Breeding Bird`, 
                `Survey Detections`) %>% 
  arrange(sort) %>% 
  dplyr::select(!sort) %>% 
  pivot_wider(SpCode:`Breeding Bird`, names_from = "Year", 
              values_from = `Survey Detections`)

write_csv(species_tally_all, "out/species_tally_all.csv")

surveys %>% 
  left_join(stations, "StationID") %>% 
  dplyr::filter(ValleyUpstDam == "Yes") %>% 
  group_by(Year) %>% 
  summarize(n = n())

