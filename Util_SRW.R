
library(tidyverse)
library(dplyr)
library(scales)
library(devtools)
library(readxl)
library(MCMCglmm)
library(ggridges)
library(ggpubr)
library(cowplot)


HistoricggpubrHistoric_prey_dens <- read.csv("Historic_prey_density.csv") %>%
  mutate(month = substr(date, 1, 1),
         year = substr(date, nchar(date) - 1, nchar(date)),
         station_type = case_when(
           grepl("w", station, ignore.case = TRUE) ~ "target",
           grepl("st", station, ignore.case = TRUE) ~ "station",
           TRUE ~ NA_character_  # Keep other cases as NA
         )) %>%
  rename(Calanidae = cal_dens,
         `Small copepods` = small_dens,
         Eucalanidae = euc_dens,
         Metridinidae = met_dens,
         Centropagidae = cen_dens,
         Candaciidae = can_dens,
         other = oth_dens,
         Euphausiidae = eup_dens,
         Bivalvia = biv_dens) %>% 
  pivot_longer(cols = Calanidae:Bivalvia, names_to = "taxa", values_to = "dens_by_m3")


# Convert the first letter of the month to the full month name
month_names <- c("J" = "January", "F" = "February", "M" = "March", "A" = "April", "M" = "May", "A" = "August", "S" = "September", "O" = "October", "N" = "November", "D" = "December")

Historic_prey_dens$month <- month_names[Historic_prey_dens$month]
Historic_prey_dens$year <- as.numeric(Historic_prey_dens$year) + 2000





Modern_prey_dens <- read.csv("Modern_prey_density.csv") %>% 
  mutate(station_type = ifelse(grepl("ST", Sample.code), "station",
                               ifelse(grepl("W", Sample.code), "target", NA)),
         year = substr(Date, 1, 4),
         month = case_when(
           substr(Date, 5, 6) == "01" ~ "January",
           substr(Date, 5, 6) == "11" ~ "November",
           substr(Date, 5, 6) == "12" ~ "December"
         )
  ) %>%
  rename(date = Date,
         station = Station.whale.number,
         Calanidae = cal_dens,
         `Small copepods` = small_dens,
         Eucalanidae = euc_dens,
         Metridinidae = met_dens,
         Centropagidae = cen_dens,
         Candaciidae = can_dens,
         other = oth_dens,
         Euphausiidae = eup_dens,
         Bivalvia = biv_dens) %>% 
  pivot_longer(cols = Calanidae:Bivalvia, names_to = "taxa", values_to = "dens_by_m3")
Modern_prey_dens



# Convert date column to character in Historic_prey_dens
Historic_prey_dens$date <- as.character(Historic_prey_dens$date)

# Select desired columns from Historic_prey_dens
Historic_prey_dens_selected <- Historic_prey_dens %>%
  #rename("sample_num" = "?..sample_num") %>%
  select(sample_num, date, station, station_type, year, month, taxa, dens_by_m3)

# Convert date column to character in Modern_prey_dens
Modern_prey_dens$date <- as.character(Modern_prey_dens$date)
# Convert year column to double in Modern_prey_dens
Modern_prey_dens$year <- as.double(Modern_prey_dens$year)

# Select desired columns from Modern_prey_dens
Modern_prey_dens_selected <- Modern_prey_dens %>%
  #rename("sample_num" = "?..sample_num") %>%
  select(sample_num, date, station, station_type, year, month, taxa, dens_by_m3)

# Combine data frames
All_prey_density <- bind_rows(Historic_prey_dens_selected, Modern_prey_dens_selected) %>% 
  mutate(Calanoida = ifelse(taxa %in% c("Calanidae", "Small copepods", "Eucalanidae", 
                                        "Metridinidae", "Centropagidae", "Candaciidae"), "Y", "N"))

All_prey_density



SRW_dive_data <- read.csv("dive_table_3-30-25.csv") %>% 
  separate(dive_start_timestamp, into = c("date", "time"), sep = " ") %>% 
  mutate(time = as.POSIXct(time, format="%H:%M:%S"))



  
