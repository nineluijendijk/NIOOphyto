# Diversity indices

library(tidyverse)
library(here)
library(vegan)

load(file = here("data/data_tidy.RData")) # load object data_tidy

Ldata <- data_tidy %>% filter(LakeName == "Church Pond") 

Mdata <- Ldata %>% group_by(Month, Species, LakeName) %>% summarize(Count = sum(Counts, na.rm = TRUE))

summary <- Mdata[c("Month", "Species", "Count")] %>% group_by(Month) %>% summarize(richness = specnumber(Count),
                                                                        shannon = diversity(Count, index = "shannon"),
                                                                        simpson = diversity(Count, index = "simpson"),
                                                                        invsimpson = 1/simpson,
                                                                        total = sum(Count))

summary %>% pivot_longer(cols = c(richness, shannon, simpson, invsimpson),
                         names_to = "metric",
                         values_to = "Count") %>%
  ggplot(aes(x = total, y = Count))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~metric, nrow = 4, scales = "free_y") # need to fix axis labels
