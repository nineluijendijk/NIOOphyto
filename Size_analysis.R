library(tidyverse)
library(here)
library(plotly)
library(htmlwidgets)

load(file = here("data/data_tidy.RData")) # load object data_tidy

BodyLengths <- tibble(Species = c("Ceriodaphnia sp.", "C. megalops", "D. cucullata", "D. curvirostris", "D. hyalina var. gellata", "D. hyalina var. lacustris", "D. hyalina", "D. longispina", "D. pulex", "D. magna", "Calanoid copepods", "Cyclops", "Bosmina coregoni", "B. longirostris", "Sida sp.", "S. crystallina", "Chydorus ovalis", "Eurycercus lamellatus", "Alona spp.", "A. quadrangularis", "Asplanchna", "Keratella spp.", "Ostracod", "Diaptomus"),
                      BodyLength = c(0.7666667, 0.925, 0.975, 1.58, 0.9515, 0.9515, 0.9515, 1.67, 1.2296, 1.3822, 3, 0.9926, 0.725, 0.3551, 2.325, 2.325, 0.475, 1.975, 0.4925, 0.9719, 0.65, 0.2, 1.25, 0.9826)) # species and their body sizes

data <- left_join(data_tidy, BodyLengths, by = "Species") # add species body size to the dataframe

data <- mutate(data, SizeClass = case_when(BodyLength <= 0.6 ~ "Small (<= 0.6 mm)",
                                           BodyLength <= 1.0 ~ "Medium ( 0.6 < x <= 1.0 mm)",
                                           BodyLength > 1.0 ~ "Large (> 1.0 mm)"))
plotlist <- list()
lakes <- unique(data$LakeName)
years <- unique(data$Year)

for (i in 1:length(lakes)){
  Ldata <- filter(data, LakeName == lakes[i])
  
  for (j in 1:length(years)){
    Ydata <- filter(Ldata, Year == years[j])
    
    if (nrow(Ydata) > 0) {
      
      Xdata <- Ydata %>% mutate("Abundance_ind/L" = (Counts / Proportion_of_sample_counted) / Sampling_volume_net_L,
                                "Relative_abundance" = Counts / Total_individuals * 100)
      
      summary <- Xdata %>% group_by(Species, SizeClass, Month) %>% summarize(sum_counts = sum(Counts, na.rm = TRUE),
                                                                             sum_total = sum(Total_individuals, na.rm = TRUE)) # Calculate counts per month
      
      summary <- summary %>% group_by(SizeClass, Month, sum_total) %>% summarize(sum_counts = sum(sum_counts, na.rm = TRUE))
      
      summary <- summary %>% mutate("Relative_abundance" = sum_counts / sum_total * 100) # Calculate relative abundance per month
      result <- summary %>% ggplot(aes(x = Month, y = Relative_abundance, fill = SizeClass))+
        geom_col(color = "Black", linewidth = 0.05)+
        labs(y = "Relative abundance (%)", title = paste0(lakes[[i]], " in ", years[[j]]))+
        theme_minimal()
      
      plotlist[[(length(plotlist)+1)]] <- result
    } else {
      warning(paste0("No data for ", lakes[[i]], " in the year ", years[[j]]))
    }
  }
}

# subplot(plotlist) 