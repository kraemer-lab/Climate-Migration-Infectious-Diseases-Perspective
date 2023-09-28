# Script for generating Figure 1C
# Number of IDPs caused by hazard type between 2008 and 2021 as a bar chart

# Load packages
library(readxl)
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)
library(gridExtra)

# Read in data
data <- read_excel("data/Internal Displacement Data/IDMC_Internal_Displacement_Disasters_Events_2008_2021.xlsx") ## Beware of path

# Process the data
process_data_fig1c <- function(x) {
  processed <- x %>%
    filter(!row_number() %in% 1) %>%
    mutate(
      `Internal Displacements` = replace(`Internal Displacements`, is.na(`Internal Displacements`), 0),
      `Hazard Type` = case_when(
        `Hazard Type` %in% c("Wet mass movement", "Wet Mass movement") ~ "Wet Mass Movement",
        `Hazard Type` == "Volcanic eruption" ~ "Volcanic activity",
        TRUE ~ `Hazard Type`
      ),
      `Internal Displacements` = as.numeric(`Internal Displacements`)
    ) %>%
    group_by(Year) %>%
    mutate(sum_idps_per_year = sum(`Internal Displacements`)) %>%
    ungroup() %>%
    group_by(`Hazard Type`) %>%
    mutate(
      sum_idps_per_hazard_type = sum(`Internal Displacements`),
      number_records_per_hazard_type = n(),
      number_countries_per_hazard_type = n_distinct(ISO3)
    ) %>%
    ungroup() %>%
    group_by(`Hazard Type`, Year) %>%
    mutate(
      number_records_per_year_by_hazard_type = n(),
      sum_idps_per_year_by_hazard_type = sum(`Internal Displacements`),
      proportion_of_records_made_up_by_hazard_type_per_year = sum_idps_per_year_by_hazard_type / sum_idps_per_year
    ) %>%
    ungroup() %>%
    group_by(`Hazard Type`, ISO3) %>%
    mutate(
      number_records_per_hazard_type_and_country = n(),
      proportion_of_records_made_up_by_country_per_hazard_type = number_records_per_hazard_type_and_country / number_records_per_hazard_type
    ) %>%
    ungroup() %>%
    select(`Hazard Type`, sum_idps_per_hazard_type, `Hazard Category`) %>%
    mutate(
      `Hazard Type` = recode(
        `Hazard Type`,
        "Dry mass movement" = "Other",
        "Mass movement" = "Other",
        "Severe winter condition" = "Other",
        "Volcanic activity" = "Volcanic\nactivity",
        "Wet Mass Movement" = "Wet\nmass\nmov.",
        "Extreme temperature" = "Extreme\ntemp",
        "Earthquake" = 'Earth-\nquake'
      ),
      `Hazard Type` = replace(`Hazard Type`, is.na(`Hazard Type`), "Other")
    ) %>%
    group_by(`Hazard Type`) %>%
    filter(row_number() == 1)
  return(processed)
}

# Applying the process_data function
data_fig1c <- process_data_fig1c(data)

data_fig1c$`Hazard Category`[data_fig1c$`Hazard Type` == 'Other'] <- "Weather related"

# Setting the order of the bars along the x axis
positions_1c <- c("Flood", "Storm", 'Earth-\nquake', "Wildfire", "Drought", "Volcanic\nactivity", "Extreme\ntemp","Wet\nmass\nmov.", "Other")

# Figure 1c ggplot script
plot_1c <- ggplot(data=data_fig1c, aes(x=`Hazard Type`, y=sum_idps_per_hazard_type))+
  geom_col(aes(fill = `Hazard Category`), alpha = 0.8)+
  scale_x_discrete(limits = positions_1c)+
  scale_fill_manual(values = c("#bb8557", "#1b4332"))+
  scale_y_continuous(expand=c(0, 0))+
  theme_minimal()+
  labs(y = "Number of Internal Displacements")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, colour = "black", size = 12),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size = 17),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black", size = 0.7),
        axis.line.y = element_line(color="black", size = 0.7))

pdf("plot_1c.pdf", width = 9, height = 7)
plot_1c
dev.off()


