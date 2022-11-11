### Single Year of Data Import

# This script brings in the current year of data and then the QA code
# in  "point count data import.Rmd" can be used for QA. 

library(tidyverse)
library(readxl)
library(lubridate)

stations <- read_excel("point_counts/data_raw/Site C Songbird Data 2022.xlsx", sheet = "Station") %>% 
  rename(StationID = 'Sample Station Label',
         Easting = `Easting Sample Station`,
         Northing = `Northing Sample Station`) %>%
  mutate(Year = str_sub(`Survey Name`,-4)) %>% 
  select(StationID, Year, everything(), 
         -c(`Sample Station Photos`, `Sample Station Comments`,
            `UTM Zone Sample Station`, `Survey Name`, Station))

surveys <- read_excel("point_counts/data_raw/Site C Songbird Data 2022.xlsx", 
                      sheet = "Surveys") %>% 
  rename(StationID = 'Sample Station Label') %>% 
  select(StationID, Visit, Date, Time, `Survey Duration`) %>% 
  mutate(Date = as_date(Date), 
         Time = as_datetime(Time)) 
# Update the date in `Time` with correct date from `Date` 
date(surveys$Time) <- date(surveys$Date)
# Most analysis of the songbird data must account for temporal survey-level effects
# These variables can be created now and to be used later.
# Year, TSSR, YDAY, DSLS

obs <- read_excel("point_counts/data_raw/Site C Songbird Data 2022.xlsx", 
                  sheet = "Observations",
                  col_types = c(
                    "text",	"numeric",	"text",	"numeric",	"text",	"text",	"text",	"numeric",
                    "numeric",	"numeric",	"numeric",	"numeric",	"text",	"text",	"text",	"text",	
                    "numeric",	"numeric",	"text",	"numeric",	"text",	"guess",	"guess",	"guess"),
                  na = "NA") %>% 
  rename(StationID = 'Sample Station Label', SpCode = "Species Code") %>% 
  select(StationID, Visit, SpCode, `Count 5 min`, `Count 0-3 min`, `Count 3-5 min`, 
         `Count 5-10 min`, `Total Count`, `Distance Category (m)`, Flyovers) %>% 
  filter(str_detect(SpCode, "B-U", negate = TRUE)) # Clear out the unknowns

# Bring in the BC Bird list from BC Species and Ecosystem Explorer
BCbirds <- read_excel("point_counts/data_reference/summaryExport_bcsee_2021-10-21.xlsx", sheet = 1) %>% 
  dplyr::select(`English Name`, `Scientific Name`, `Species Code`, Order, 
                `BC List`, COSEWIC, `SARA Status`, `Breeding Bird`) %>% 
  rename(SpCode = "Species Code") %>%
  mutate(IsSongbird = ifelse(Order %in% c("Columbiformes", "Caprimulgiformes", 
                                          "Piciformes", "Passeriformes"), 
                             TRUE, 
                             FALSE),
         sort = row_number()) %>%
  filter(!is.na(SpCode))

# Now go run the code under import