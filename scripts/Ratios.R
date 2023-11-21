# Ratios

library(tidyverse)
library(here)
library(readxl)
library(RColorBrewer)

load(file = here("data/data_tidy.RData")) # load object data_tidy
classification <- read_excel(here("data_raw/SpeciesRatioClassification.xlsx"))

dataM <- left_join(data_tidy, classification, by = "Species")

`%out%` <- function(a,b) ! a %in% b

RatioPlot <- function(data, lake, year, class) {

  Ldata <- filter(data, Lake == lake) # perform the analysis per lake
  
  Ydata <- filter(Ldata, Year == year) # perform the analysis per year

  if (lake %out% c("CN", "TP", "MP", "CH", "CL", "BP")) {
    
    stop(paste0("Lake ", lake, " does not exist. Use one of the following:\nBP for Beeston Pond\nCH for Church Pond\nCL for Clifton Pond\nCN for Coneries Lake\nTP for Tween Pond\nMP for Main Pond"))
  
    } else if (nrow(Ydata) <= 0) {
    
      warning(paste0("There is no data for ", lake, " in the year ", year))
  
    } else if (class %in% c("Class1", "Class2", "Class3")) {
      
  summary <- Ydata %>% group_by(Species, Date) %>% summarize(sum_counts = sum(Counts, na.rm = TRUE),
                                                                sum_total = sum(Total_individuals, na.rm = TRUE)) # calculate counts per Date
      
  summary <- left_join(summary, classification, by = "Species")

  summary <- summary %>% mutate("Relative_abundance" = sum_counts / sum_total * 100) # calculate relative abundance per Date

  getPalette <- colorRampPalette(brewer.pal(3, "Dark2")) # prepare color palette containing the most distinct colors

  summary %>% ggplot(aes(x = Date, y = Relative_abundance, fill = get(class)))+
        geom_col()+
        labs(y = "Relative abundance (%)",
             title = paste0("Relative abundances of different groups in ", lake, " in ", year),
             fill = "Groups")+
        scale_fill_manual(values = getPalette(nrow(unique(summary[, class]))))+
        theme_minimal() -> result
  ggplotly(result)
  
  } else {
    stop(paste0(class, " is not one of the possible classes. Use Class1 (Cladocerans/Copepods/Other), Class2 (Daphnia/Other) or Class3 (Large Daphnia/Other)."))
  }
}

# RatioPlot(dataM, "CL", 2011, "Class1")

lakes <- unique(data_tidy$Lake) # extract the names of the lakes
years <- unique(data_tidy$Year) # extract the years where counts were measured
classes <- c("Class1", "Class2", "Class3")

for (i in lakes) {
  for (j in years) {
    for (k in classes) {
      print(RatioPlot(dataM, i, j, k))
    }
  }
}
