library(readxl)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggrepel)
library(ggpubr)

db <- read_xlsx('db.xlsx')
options(scipen=999)

africa <- subset(db, db$Continent == 'Africa') 

africa_db <- ggplot()+
  theme_classic()+
  geom_point(data = db, aes(x=GDP, y=DALY,size = Population), alpha = 0.6, colour = 'grey90')+ 
  geom_point(data = africa, aes(x=GDP, y=DALY,size = Population,colour = Continent))+ 
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=10))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_color_manual(values=c('#1b4332'), name='')+
  xlab('GDP per capita ($)')+
  ylab('Share of disease burden from communicable disease (DALY)')+
  ylim(0, 80)+
  scale_size(range=c(0,8), limits = c(10000,1450000000), breaks=c(10000, 50000000, 75000000,1000000000, 1450000000), labels=c('10,000', '50 million', '75 million','1 billion', '1.45 billion'))+
  geom_text_repel(data=africa, mapping = aes(x=GDP, y=DALY,label = Entity), size =2.0)+
  coord_cartesian(clip = "off") +
  theme(legend.title = element_blank())+
  theme(legend.position = 'right')+
  theme(legend.text = element_text(color="black", size=10))+
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1)))+
  scale_x_continuous(trans = 'log10', breaks = c(1000,2000, 5000, 10000, 20000, 50000, 100000))

africa_db


asia_db <- subset(db, db$Continent == 'Asia') 

asia_db_plot <- ggplot()+
  theme_classic()+
  geom_point(data = db, aes(x=GDP, y=DALY,size = Population), alpha = 0.6, colour = 'grey90')+ 
  geom_point(data = asia_db, aes(x=GDP, y=DALY,size = Population,colour = Continent))+ 
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=10))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_color_manual(values=c('#1bb28c'), name='')+
  xlab('GDP per capita ($)')+
  ylab('Share of disease burden from communicable disease (DALY)')+
  ylim(0, 80)+
  scale_size(range=c(0,8), limits = c(10000,1450000000), breaks=c(10000, 50000000, 75000000,1000000000, 1450000000), labels=c('10,000', '50 million', '75 million','1 billion', '1.45 billion'))+
  geom_text_repel(data=asia_db, mapping = aes(x=GDP, y=DALY,label = Entity), size =2.0)+
  coord_cartesian(clip = "off") +
  theme(legend.title = element_blank())+
  theme(legend.position = 'right')+
  theme(legend.text = element_text(color="black", size=10))+
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1)))+
  scale_x_continuous(trans = 'log10', breaks = c(1000,2000, 5000, 10000, 20000, 50000, 100000))

asia_db_plot

europe_db <- subset(db, db$Continent == 'Europe') 

europe_plot <- ggplot()+
  theme_classic()+
  geom_point(data = db, aes(x=GDP, y=DALY,size = Population), alpha = 0.6, colour = 'grey90')+ 
  geom_point(data = europe_db, aes(x=GDP, y=DALY,size = Population,colour = Continent))+ 
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=10))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_color_manual(values=c('#bb8557'), name='')+
  xlab('GDP per capita ($)')+
  ylab('Share of disease burden from communicable disease (DALY)')+
  ylim(0, 80)+
  scale_size(range=c(0,8), limits = c(10000,1450000000), breaks=c(10000, 50000000, 75000000,1000000000, 1450000000), labels=c('10,000', '50 million', '75 million','1 billion', '1.45 billion'))+
  geom_text_repel(data=europe_db, mapping = aes(x=GDP, y=DALY,label = Entity), size =2.0, max.overlaps = 40)+
  coord_cartesian(clip = "off") +
  theme(legend.title = element_blank())+
  theme(legend.position = 'right')+
  theme(legend.text = element_text(color="black", size=10))+
  guides(colour = guide_legend(override.aes = list(size = 0, alpha = 1)))+
  scale_x_continuous(trans = 'log10', breaks = c(1000,2000, 5000, 10000, 20000, 50000, 100000))

europe_plot

NA_db <- subset(db, db$Continent == 'North America') 

NA_plot <- ggplot()+
  theme_classic()+
  geom_point(data = db, aes(x=GDP, y=DALY,size = Population), alpha = 0.6, colour = 'grey90')+ 
  geom_point(data = NA_db, aes(x=GDP, y=DALY,size = Population,colour = Continent))+ 
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=10))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_color_manual(values=c('#fed45b'), name='')+
  xlab('GDP per capita ($)')+
  ylab('Share of disease burden from communicable disease (DALY)')+
  ylim(0, 80)+
  scale_size(range=c(0,8), limits = c(10000,1450000000), breaks=c(10000, 50000000, 75000000,1000000000, 1450000000), labels=c('10,000', '50 million', '75 million','1 billion', '1.45 billion'))+
  geom_text_repel(data=NA_db, mapping = aes(x=GDP, y=DALY,label = Entity), size =2.0, max.overlaps = 40)+
  coord_cartesian(clip = "off") +
  theme(legend.title = element_blank())+
  theme(legend.position = 'right')+
  theme(legend.text = element_text(color="black", size=10))+
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1)))+
  scale_x_continuous(trans = 'log10', breaks = c(1000,2000, 5000, 10000, 20000, 50000, 100000))

NA_plot

Oceania_db <- subset(db, db$Continent == 'Oceania') 

Oceania_plot <- ggplot()+
  theme_classic()+
  geom_point(data = db, aes(x=GDP, y=DALY,size = Population), alpha = 0.6, colour = 'grey90')+ 
  geom_point(data = Oceania_db, aes(x=GDP, y=DALY,size = Population,colour = Continent))+ 
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=10))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_color_manual(values=c('#007ea7'), name='')+
  xlab('GDP per capita ($)')+
  ylab('Share of disease burden from communicable disease (DALY)')+
  ylim(0, 80)+
  scale_size(range=c(0,8), limits = c(10000,1450000000), breaks=c(10000, 50000000, 75000000,1000000000, 1450000000), labels=c('10,000', '50 million', '75 million','1 billion', '1.45 billion'))+
  geom_text_repel(data=Oceania_db, mapping = aes(x=GDP, y=DALY,label = Entity), size =2.5, max.overlaps = 40)+
  coord_cartesian(clip = "off") +
  theme(legend.title = element_blank())+
  theme(legend.position = 'right')+
  theme(legend.text = element_text(color="black", size=10))+
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1)))+
  scale_x_continuous(trans = 'log10', breaks = c(1000,2000, 5000, 10000, 20000, 50000, 100000))

Oceania_plot

SA_db <- subset(db, db$Continent == 'South America') 

SA_plot <- ggplot()+
  theme_classic()+
  geom_point(data = db, aes(x=GDP, y=DALY,size = Population), alpha = 0.6, colour = 'grey90')+ 
  geom_point(data = SA_db, aes(x=GDP, y=DALY,size = Population,colour = Continent))+ 
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=10))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_color_manual(values=c('#e86a58'), name='')+
  xlab('GDP per capita ($)')+
  ylab('Share of disease burden from communicable disease (DALY)')+
  ylim(0, 80)+
  scale_size(range=c(0,8), limits = c(10000,1450000000), breaks=c(10000, 50000000, 75000000,1000000000, 1450000000), labels=c('10,000', '50 million', '75 million','1 billion', '1.45 billion'))+
  geom_text_repel(data=SA_db, mapping = aes(x=GDP, y=DALY,label = Entity), size =2.5, max.overlaps = 40)+
  coord_cartesian(clip = "off") +
  theme(legend.title = element_blank())+
  theme(legend.position = 'right')+
  theme(legend.text = element_text(color="black", size=10))+
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1)))+
  scale_x_continuous(trans = 'log10', breaks = c(1000,2000, 5000, 10000, 20000, 50000, 100000))

SA_plot

ggarrange(africa_db, asia_db_plot,SA_plot, Oceania_plot, NA_plot, europe_plot  , nrow =2, ncol = 3, common.legend = TRUE)



