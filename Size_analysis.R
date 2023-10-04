# Categorizing by size

library(tidyverse)
library(here)

load(file = here("data/data_tidy.RData")) # load object data_tidy

BodyLengths <- tibble(Species = c("Ceriodaphnia sp.", "C. megalops", "Daphnia cullata", "D. curvirostris", "D. hyalina var. gellata", "D. hyalina var. lacustris", "D. hyalina", "D. longispina", "D. pulex", "D. magna", "Calanoid copepods", "Cyclopoid copepods", "Bosmina coregoni", "B. longirostris", "Sida sp.", "S. crystallina", "Chydorus ovalis", "Eurycercus lamellatus", "Alona spp.", "A. quadrangularis", "Asplancha spp.", "Keratella spp.", "Ostracod", "Diaptomus"),
       BodyLength = c(0.7666667, 0.925, 0.975, 1.58, 1.5, 2, 1.57, 1.67, 2, 4, 3, 1, 0.725, 0.45, 2.325, 2.325, 0.475, 1.975, 0.4925, 0.8, 0.65, 0.2, 1.25, 2.005)) # species and their body sizes

data <- left_join(data_tidy, BodyLengths, by = "Species") # add species body size to the dataframe

data <- mutate(data, SizeClass = case_when(BodyLength <= 0.6 ~ "Small (<= 0.6)",
                                      BodyLength <= 1.0 ~ "Medium ( 0.6 < x <= 1.0)",
                                      BodyLength > 1.0 ~ "Large (> 1.0)"))


summarytot <- data %>% group_by(Species, Month) %>% summarize(sum_total = sum(Total_individuals, na.rm = TRUE))
summarytot <- head(summarytot[-1], n = 12)
summarycou <- data %>% group_by(SizeClass, Month) %>% summarize(sum_counts = sum(Counts, na.rm = TRUE)) # Calculate counts per month
summary <- left_join(summarycou, summarytot, by = "Month")

summary <- summary %>% mutate("Relative_abundance" = sum_counts / sum_total * 100) # Calculate relative abundance per month

summary %>%
  ggplot(aes(x = Month, y = Relative_abundance, fill = SizeClass)) +
  geom_col(color = "Black", linewidth = 0.05) +
  labs(y = "Relative abundance (%)", fill = "Size class") +
  theme_minimal() # Generate plot of relative abundance per month, not sure about the colors
