###
### Import the Point Count Data For All Years ###
###


# Set up Environment ------------------------------------------------------

# Packages
library(tidyverse)
library(readxl)
library(lubridate)
library(skimr)
library(kableExtra)

options(scipen = 999)


# Read and Tidy data ------------------------------------------------------

# The data imported below is a copy of the master data file that is located in the project Sharepoint site. Make sure it is up to date.
# https://tetratechinc.sharepoint.com/:f:/r/teams/704-ENV.VENV03095-01/Shared%20Documents/002-Songbird/Data/All%20Years

# Set some nicer names for variables regularly used. Get rid of variables that 
# will not be used in any analysis, just for cleaner tables.


# Stations ----------------------------------------------------------------

stations <- read_excel("data_raw/Songto2022data.xlsx", "Stations") %>% 
  dplyr::rename(StationID = 'Sample Station Label',
                Easting = `Easting Sample Station`,
                Northing = `Northing Sample Station`) %>%
  dplyr::mutate(Year = str_sub(`Survey Name`,-4)) %>% 
  dplyr::select(StationID, Year, everything(), 
                -c(`Sample Station Photos`, `Sample Station Comments`,
                   `UTM Zone Sample Station`, `Survey Name`, Station))


# Surveys -----------------------------------------------------------------

surveys <- read_excel("data_raw/Songto2022data.xlsx", "Surveys", 
                      col_types = c("text",	"text",	"numeric",	"date",	"date",	
                                    "date",	"numeric",	"text",	"text",	"text",	
                                    "text",	"numeric",	"text",	"text",	"text",	
                                    "text",	"text")) %>% 
  dplyr::rename(StationID = 'Sample Station Label') %>%
  dplyr::select(StationID, Visit, Date, Time, `SurveyDuration`, `Inventory Type`) %>% 
  dplyr::mutate(Date = as_date(Date), 
                Time = as_datetime(Time)) 
# Update the date in "Time" with correct date from `Date` 
date(surveys$Time) <- date(surveys$Date)


# Observations ------------------------------------------------------------

obs <- read_excel("data_raw/Songto2022data.xlsx", "Observations",
                  col_types = c("text", "numeric", "text", "text", "text", 
                                "numeric", "numeric", "numeric", "numeric", 
                                "numeric", "text", "text", "text", "text", 
                                "numeric", "numeric", "text", "guess", "text",
                                "text", "text"),
                  na = "NA") %>% 
  dplyr::rename(StationID = 'Sample Station Label', SpCode = Species) %>% 
  dplyr::select(StationID, Visit, SpCode, `Count 5 min`, `Count 0-3 min`, `Count 3-5 min`, 
                `Count 5-10 min`, Count, `Distance Category`, Flyovers) %>% 
  dplyr::filter(str_detect(SpCode, "B-U", negate = TRUE)) # Clear out the unknowns


# Create BC Bird list from BC Species and Ecosystem Explorer --------

BCbirds <- read_excel("../sc_common_files/data_reference/summaryExport_bcsee_2022-11-10.xlsx", 
                      sheet = 1) %>% 
  dplyr::select(`English Name`, `Scientific Name`, `Species Code`, Order, 
                `BC List`, COSEWIC, `SARA Status`, `Breeding Bird`) %>% 
  rename(SpCode = "Species Code") %>%
  mutate(IsSongbird = ifelse(Order %in% c("Columbiformes", "Caprimulgiformes", 
                                          "Piciformes", "Passeriformes"), 
                             TRUE, 
                             FALSE),
         sort = row_number()) %>% 
  filter(!is.na(SpCode))


