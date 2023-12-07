# Excel overview

library(tidyverse)
library(here)
library(readxl)
library(writexl)

data <- read_excel(here("data_raw/Zooplakton_all_data.xlsx"), range = cell_cols("A:J")) %>% select(-"Counts") # load data

load(file = here("data/data_tidy.RData")) # load object data_tidy

data <- mutate(data, `Abundance Ind/L` = (`Total individuals` / `Proportion of sample counted`) / `Sampling volume net (L)`)

BodyLengths <- tibble(Species = c("Ceriodaphnia sp.", "C. megalops", "D. cucullata", "D. curvirostris", "D. hyalina var. gellata", "D. hyalina var. lacustris", "D. hyalina", "D. longispina", "D. pulex", "D. magna", "Calanoid copepods", "Cyclops", "Bosmina coregoni", "B. longirostris", "Sida sp.", "S. crystallina", "Chydorus ovalis", "Eurycercus lamellatus", "Alona spp.", "A. quadrangularis", "Asplanchna", "Keratella spp.", "Ostracod", "Diaptomus"),
                      BodyLength = c(0.4636, 0.925, 0.8593, 1.58, 0.9064, 0.9064, 0.9064, 1.0347, 1.1656, 1.4214, 0.6860, 1.0369, 0.4230, 0.3637, 0.5075, 0.5075, 0.475, 1.975, 0.5235, 0.9679, 0.4747, 0.3495, 1.25, 0.9886)) # species and their body sizes

dataSizes <- left_join(data_tidy, BodyLengths, by = "Species") # add species body size to the dataframe

dataSizes <- mutate(dataSizes, SizeClass = case_when(BodyLength <= 0.6 ~ "% Small (<= 0.6 mm)",
                                           BodyLength <= 1.0 ~ "% Medium ( 0.6 < x <= 1.0 mm)",
                                           BodyLength > 1.0 ~ "% Large (> 1.0 mm)"))

summary <- dataSizes %>% group_by(Lake, SizeClass, Date) %>% summarize(sum_counts = sum(Counts, na.rm = TRUE)) # Calculate counts per Date

totalIndividuals <- select(data, c(Lake, Date, `Total individuals`))

summary <- left_join(summary, totalIndividuals)

summary <- summary %>% mutate("Relative_abundance" = sum_counts / `Total individuals` * 100) # Calculate relative abundance per Date

summaryShort <- select(summary, -c(sum_counts, `Total individuals`))

summaryWide <- pivot_wider(data = summaryShort, names_from = SizeClass, values_from = Relative_abundance)

dataWide <- left_join(data, summaryWide)

dataWideClean <- select(dataWide, -c(`Sampling volume net (L)`, `Total volume sample (ml)`,
                                     `Counted volume (ml)`, `Proportion of sample counted`))

summaryS <- dataSizes %>% group_by(Lake, Species, Date) %>% summarize(sum_counts = sum(Counts, na.rm = TRUE),
                                                            sum_total = sum(Total_individuals, na.rm = TRUE))

summaryV <- summaryS %>% mutate("Relative_abundance" = sum_counts / sum_total * 100) # Calculate relative abundance per Date

summaryW <- left_join(summaryV, BodyLengths, by = "Species")

WeightedMean <- summaryW %>% group_by(Lake, Date) %>% summarize(WeightedMean_mm = weighted.mean(BodyLength, Relative_abundance))

dataComplete <- left_join(dataWideClean, WeightedMean)

dataComplete$Date <- format(as.POSIXct(dataComplete$Date , format = '%Y/%m/%d %H:%M:%S'),format='%Y/%m/%d')

dataComplete$Date <- as.Date(dataComplete$Date)

write_xlsx(dataComplete, here("data/overviewNumbers.xlsx"))
