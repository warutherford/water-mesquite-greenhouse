## Santa Rita Experimental Range Desert Grassland Exclosure##
## Field weather data monsoon start 2017
## Greenhouse Mesquite Study
# Austin Rutherford
# 2021-03-2021
# email: arutherford@email.arizona.edu

##install packages if needed, commented out since there are already installed
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("ggpubr")

## load packages
library(tidyverse)
library(lubridate)
library(ggpubr)

# bring the data in
srer_rain <- read_csv(file = "Data/2017-07_ppt.csv")

# look at data
glimpse(srer_rain)

# filter dates to start of ppt events monsoon 2017
monsoon_2017 <- srer_rain %>% 
  dplyr::select(date, temp, rain) %>% 
  dplyr::filter(date >= "2017-07-09" & date <= "2017-08-03")

monsoon_2017_tot <- srer_rain %>% 
  dplyr::select(date, temp, rain) %>% 
  dplyr::filter(date >= "2017-07-01" & date <= "2017-07-31")

# count events in range >0.5mm (22)
tot_water_days <- monsoon_2017 %>% 
  dplyr::select(rain) %>% 
  dplyr::filter(rain >= 0.5) %>% 
  dplyr::summarise(tot_event = n())

# watering amounts calculations
tx_amts <- monsoon_2017 %>% 
  dplyr::select(rain) %>% 
  dplyr::filter(rain > 0) %>%
  summarise(mean = round(mean(rain)), #mean ambient
            mean_wet = round(mean(rain) + (mean(rain) - (mean(rain)*0.65))),# add 65% of ambient to mean ambient
            mean_dry = round(mean(rain) - (mean(rain)*0.65))) # reduce ambient by 65% (conversely, 35% of ambient)

# make precip figure
ppt_bar_monsoon <- monsoon_2017 %>% 
  ggplot()+
  geom_bar(aes(x = date, y = rain), stat = "identity", fill = 'lightblue')+
  geom_line(aes(x = date, y = temp), stat = "identity", color = "#DB4325", size = 1.5)+
  #geom_hline(yintercept = 8, color = "blue1", size = 1.5)+ # add horizontal line of mean ppt
  scale_y_continuous(name = "Total Precipitation (mm)",
                     breaks = seq(0, 25, 2),
                     sec.axis = sec_axis(~.,
                                         name = "Mean Temperature (°C)",
                                         breaks = seq(20, 30, 2)))+
  scale_x_date(date_labels = "%b %d", date_breaks = "2 days" ,
               limits = as.Date(c(NA,NA)),
               expand = c(0, 0),
               name = "Date (2017)")+
  labs_pubr()+
  theme_pubr(x.text.angle = 45)+
  theme(axis.title.y = element_text(hjust = 0.4,
                                    vjust = 1),
        axis.title.y.right = element_text(hjust = 0,
                                          vjust = 1,
                                          size = 11))

ppt_bar_monsoon

ggsave("Figures/2017-07_ppt_monsoon.tiff", plot = ppt_bar_monsoon, height = 4, width = 5, dpi=1200)



