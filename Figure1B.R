# Script for generating Figure 1B
# % IDPs caused by hazard type per year (in a proportional stacked area chart)

# Load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(scales)

# Read in data
data <- read_excel("data/IDMC_Internal_Displacement_Disasters_Events_2008_2021.xlsx") ### Beware of path 

# Process the data
process_data_fig1b <- function (x){
  processed <- x %>%
    filter(!row_number() %in% c(1))%>%
    mutate(`Hazard Type` = replace(`Hazard Type`, `Hazard Type` == "Wet mass movement" | `Hazard Type` =="Wet Mass movement", "Wet mass movement"),
           `Hazard Type` = replace(`Hazard Type`, `Hazard Type` == "Volcanic eruption", "Volcanic activity"),
           `Internal Displacements` = as.numeric(`Internal Displacements`),
           `Internal Displacements` = replace(`Internal Displacements`, is.na(`Internal Displacements`), 0))%>%
    group_by(`Hazard Type`, Year)%>%
    mutate(number_records_per_year_by_hazard_type = n(), 
           sum_idps_per_year_by_hazard_type = sum(`Internal Displacements`))%>%
    ungroup()%>%
    select(Year, `Hazard Type`, sum_idps_per_year_by_hazard_type)%>%
    mutate(`Hazard Type` = as.factor(`Hazard Type`),
           Year = as.numeric(Year, "%Y"))%>%
    filter(!is.na(`Hazard Type`))
}

# Applying the 'process_data' function
data_plot <- process_data_fig1b(data)

# Specifying order of hazard types in legend
fill.order <- factor(data_plot$`Hazard Type`, levels = c("Flood", "Storm", "Earthquake", 
                                                         "Drought", "Wildfire", "Volcanic activity", 
                                                         "Extreme temperature", "Dry mass movement", 
                                                         "Wet mass movement", "Mass movement"))

# Figure 1b ggplot script
figure_1b <- ggplot(data_plot , aes(x=Year, y=sum_idps_per_year_by_hazard_type, fill=fill.order)) + 
  geom_area(position = "fill", alpha=0.8 , size=0.5, colour="grey")+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent, expand = c(0,0))+
  scale_x_continuous(expand = c(0,0), n.breaks = 4)+
  scale_fill_manual(values = c('#1b4332', '#1bb28c','#bb8557', '#fed45b', '#007ea7', '#e86a58', 'grey', "orange","chocolate3","wheat3"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, colour="black", size = 16),
        axis.text.y = element_text(colour="black", size = 16),
        axis.title.y = element_text(size = 17),
        #axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.7))+
  guides(fill=guide_legend(title="Displacement Cause"))+
  labs(y = "Proportion")

pdf("figure_1b.pdf",width = 12, height = 7) ## May want to change path for github repo structure
figure_1b
dev.off()



