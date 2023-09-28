library(dplyr)

owid_co2_data <- read_excel('owid-co2-data.xlsx')
co2 <- subset(owid_co2_data, owid_co2_data$year == 2020) 
co3 <- co2[!is.na(co2$iso_code),]

vuln_read_2020 <- read_excel('vuln_read_2020.xlsx')
colnames(vuln_read_2020)[1] <- 'iso_code'
colnames(db)[2] <- 'iso_code'

joined_tibble <- left_join(vuln_read_2020, co3,
                           by = c("iso_code" = "iso_code"))
joined_tibble2 <- left_join(db, joined_tibble,
                           by = c("iso_code" = "iso_code"))

names(joined_tibble)
sapply(joined_tibble, class)    
joined_tibble$vuln_read_2020 <- as.numeric(as.character(joined_tibble$vuln_2020))
joined_tibble$vuln_2020_health <- as.numeric(as.character(joined_tibble$vuln_2020_health))

vuln_co2 <- ggplot(joined_tibble2, aes(x=share_global_cumulative_co2, y=vuln_2020,size = GDP,colour = Continent, label = Name))+
  theme_classic()+
  geom_point(alpha = .6)+ 
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=10))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_color_manual(values=c('#1b4332', '#1bb28c','#bb8557', '#fed45b', '#007ea7', '#e86a58', '#cccccc'), name='')+
  xlab('Share of global cumulative CO2 emissions (%)')+
  ylab('Vulnerability index')+
  geom_text_repel(max.overlaps = 8, size = 2)+
  theme(legend.position = 'top')+
  theme(legend.text = element_text(color="black", size=10))+
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1)))+
  scale_x_continuous(trans = 'log10', breaks = c(0,0.01, 0.1, 1, 10, 30))

vuln_co2

