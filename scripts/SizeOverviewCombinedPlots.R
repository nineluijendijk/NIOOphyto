library(tidyverse)
library(here)
library(plotly)
library(htmlwidgets)
library(patchwork)
library(gridExtra)

load(file = here("data/data_tidy.RData")) # load object data_tidy

BodyLengths <- tibble(Species = c("Ceriodaphnia sp.", "C. megalops", "D. cucullata", "D. curvirostris", "D. hyalina var. gellata", "D. hyalina var. lacustris", "D. hyalina", "D. longispina", "D. pulex", "D. magna", "Calanoid copepods", "Cyclops", "Bosmina coregoni", "B. longirostris", "Sida sp.", "S. crystallina", "Chydorus ovalis", "Eurycercus lamellatus", "Alona spp.", "A. quadrangularis", "Asplanchna", "Keratella spp.", "Ostracod", "Diaptomus"),
                      BodyLength = c(0.4636, 0.925, 0.8593, 1.58, 0.9064, 0.9064, 0.9064, 1.0347, 1.1656, 1.4214, 0.6860, 1.0369, 0.4230, 0.3637, 0.5075, 0.5075, 0.475, 1.975, 0.5235, 0.9679, 0.4747, 0.3495, 1.25, 0.9886)) # species and their body sizes

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
      
      summary <- Ydata %>% group_by(Species, SizeClass, Date) %>% summarize(sum_counts = sum(Counts, na.rm = TRUE),
                                                                            sum_total = sum(Total_individuals, na.rm = TRUE)) # Calculate counts per Date
      
      summary <- summary %>% group_by(SizeClass, Date, sum_total) %>% summarize(sum_counts = sum(sum_counts, na.rm = TRUE))
      
      summaryV <- Ydata %>% group_by(Species, Date) %>% summarize(sum_counts = sum(Counts, na.rm = TRUE),
                                                                  sum_total = sum(Total_individuals, na.rm = TRUE))
      
      summaryV <- summaryV %>% mutate("Relative_abundance" = sum_counts / sum_total * 100) # Calculate relative abundance per Date
      
      summaryW <- left_join(summaryV, BodyLengths, by = "Species")
      
      WeightedMean <- summaryW %>% group_by(Date) %>% summarize(WeightedMean_mm = weighted.mean(BodyLength, Relative_abundance))
      
      #print(knitr::kable(WeightedMean, 
      #format = "markdown",
      #caption = paste0("The weighted mean for ", lakes[i], " in ", years[j])))
      
      summary <- summary %>% mutate("Relative_abundance" = sum_counts / sum_total * 100) # Calculate relative abundance per Date
      
      result <- summary %>% ggplot(aes(x = Date, y = Relative_abundance, fill = SizeClass))+
        geom_col(color = "Black", linewidth = 0.05)+
        labs(x = "Month", y = "Relative abundance (%)", title = paste0(lakes[[i]], " in ", years[[j]]))+
        theme_minimal()
      
      tbl <- tableGrob(t(WeightedMean))
      
      print(wrap_plots(result, tbl))
      
    } else {
      warning(paste0("No data for ", lakes[[i]], " in the year ", years[[j]]))
    }
  }
}
