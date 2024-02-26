library(tidyverse)
library(here)
library(readxl)
library(writexl)

overlap <- read.delim(here("data/overlap.txt")) %>% unlist()

data <- read_excel(here("data_raw/OCsed_all_lakes.xlsx"))

data$Combined <- paste(data$Lake, data$Date)

data <- select(data, `OC sed. rate (g C m-2 d-1)`, Combined)

dataF <- filter(data, Combined %in% overlap)

dataT <- tibble(dataF$Combined, dataF$`OC sed. rate (g C m-2 d-1)`)

dataR <- arrange(dataT, `dataF$Combined`)


load(file = here("data/data_tidy.RData")) # load object data_tidy
data_tidy <- mutate(data_tidy, LakeType = case_when(Lake == "CL" | Lake == "CH" ~ "Not connected",
                                                    Lake == "TP" | Lake == "CN" | Lake == "MP" ~ "Connected",
                                                    Lake == "BP" ~ "Half connected"))

`%out%` <- function(a,b) ! a %in% b
data_tidy <- filter(data_tidy, Year < 2009)
data_tidy <- filter(data_tidy, Year > 2005)
species_to_exclude <- c("Eurycercus lamellatus", "Ostracod", "A. quadrangularis", "Diaptomus")
data_tidy <- filter(data_tidy, Species %out% species_to_exclude)

data_extra <- data_tidy %>% mutate("Abundance_ind/L" = (Counts / Proportion_of_sample_counted) / Sampling_volume_net_L,
                                   "Relative_abundance" = Counts / Total_individuals * 100)

data_wide <- pivot_wider(data = select(data_extra, -Counts, -"Relative_abundance"), names_from = "Species", values_from = "Abundance_ind/L")

data_wide$Combined <- paste(data_wide$Lake, data_wide$Date)

data_wide <- na.omit(data_wide)

data2 <- as.data.frame(data_wide[14:34])

data2F <- filter(data2, Combined %in% overlap)

data2T <- tibble(data2F$Combined, data2F[1:20])

data2R <- arrange(data2T, `data2F$Combined`)

write_xlsx(dataR, here("data/dataRDAOverlap.xlsx"))

write_xlsx(data2R, here("data/data_matrixRDAOverlap.xlsx"))
