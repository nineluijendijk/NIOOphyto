library(tidyverse)
library(here)
library(readxl)

data <- read_excel(here("data_raw/Zooplakton_all_data.xlsx"), range = cell_cols("A:AH")) %>% select(c(-"Counts", -"Total individuals")) # load data

species_analyzed <- c("D. hyalina", "D. pulex", "Cyclopoid copepods", "B. longirostris", "A. quadrangularis", "Diaptomus") # species analyzed by Kailin Hu

species <- colnames(data)[9:32] # extract species' names

data_n <- pivot_longer(data, cols = species, names_to = "Species", values_to = "Counts") # make species a variable

colnames <- colnames(data_n)
new_colnames <- gsub("\\ ", "_", colnames)
data_tidy <- data_n %>% setNames(new_colnames) # no more spaces in column names

data_tidy$Counts[data_tidy$Analyzed_by == "Iain" & is.na(data_tidy$Counts) & data_tidy$Species != "A. quadrangularis" & data_tidy$Species != "Diaptomus"] <- 0 # change NA to 0 where applicable

data_tidy$Counts[data_tidy$Species == species_analyzed & is.na(data_tidy$Counts)] <- 0 # change NA to 0 where applicable

view(data_tidy)

save(data_tidy, file = here("data/data_tidy.rds")) # save tidy dataframe
