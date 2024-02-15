library(tidyverse)
library(here)
library(writexl)
library(lubridate)

BP <- read_csv(here("data_raw/Selected_variables_BP.csv"))
connected <- read_csv(here("data_raw/Selected_variables_connected.csv"))

BPtidy <- separate(BP, `lake;month;year;ocsed`, into = c("lake", "month", "year", "ocsed"), sep = ";")
connectedtidy <- separate(connected, `lake;month;year;ocsed`, into = c("lake", "month", "year", "ocsed"), sep = ";")

data <- rbind(BPtidy, connectedtidy)

data$Combined <- paste(data$lake, data$month, data$year)
data <- filter(data, year < 2009)

data <- select(data, ocsed, Combined)

write_xlsx(data, here("data/dataRDA.xlsx"))


load(file = here("data/data_tidy.RData")) # load object data_tidy
data_tidy <- mutate(data_tidy, LakeType = case_when(Lake == "CL" | Lake == "CH" ~ "Not connected",
                                                    Lake == "TP" | Lake == "CN" | Lake == "MP" ~ "Connected",
                                                    Lake == "BP" ~ "Half connected"))

`%out%` <- function(a,b) ! a %in% b
data_tidy <- filter(data_tidy, Year < 2009)
data_tidy <- filter(data_tidy, Year > 2005)
species_to_exclude <- c("Eurycercus lamellatus", "Ostracod", "A. quadrangularis", "Diaptomus")
data_tidy <- filter(data_tidy, Species %out% species_to_exclude)

lakes_to_exclude <- c("CL","CH")
data_tidy <- filter(data_tidy, Lake %out% lakes_to_exclude)
data_tidy$month <- month(data_tidy$Date, label = FALSE) # Add month names


data_extra <- data_tidy %>% mutate("Abundance_ind/L" = (Counts / Proportion_of_sample_counted) / Sampling_volume_net_L,
                                   "Relative_abundance" = Counts / Total_individuals * 100)

data_wide <- pivot_wider(data = select(data_extra, -Counts, -"Relative_abundance"), names_from = "Species", values_from = "Abundance_ind/L")
data_wide$Combined <- paste(data_wide$Lake, data_wide$month, data_wide$Year)
data_wide <- na.omit(data_wide)

data <- as.data.frame(data_wide[15:35])

write_xlsx(data, here("data/data_matrixRDA.xlsx"))

