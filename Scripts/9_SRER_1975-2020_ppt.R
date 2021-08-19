## Santa Rita Experimental Range Desert Grassland Exclosure##
## Historic weather data 1975- July 2017
## Greenhouse Mesquite Study
# Austin Rutherford
# 2021-07-23
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
srer_daily <- read_csv(file = "Data/SRER_daily_1975-2020.csv")

# look at data
glimpse(srer_daily)

# gage as factor
as.factor(srer_daily$gage) -> srer_daily$gage

#summarize by day
srer_byday <- srer_daily %>%
  drop_na() %>% 
  group_by(date) %>% 
  summarise(amt_mm_mean = mean(amt_mm),
            n = n(),
            amt_se = (sd(amt_mm)/sqrt(n)))

# gap fill data
srer_filled <- srer_byday %>% 
  ungroup() %>% 
  complete(date = seq(min(date), as.Date("2017-09-30"), by = "day"))

# create y m d columns
srer_filled$year <-year(ymd(srer_filled$date))
srer_filled$month <-month(ymd(srer_filled$date))
srer_filled$day <-day(ymd(srer_filled$date))

# Break up data into bins
#july
july.1<-dplyr::filter(srer_filled, month %in% c(7)) #Warm = july
julymiss.1<-july.1
julymiss.1$miss<-ifelse(is.na(julymiss.1$amt_mm_mean), 1, 0)

julymiss.2<-julymiss.1%>%
  group_by(year)%>%
  summarise(misstot = sum(miss, na.rm=TRUE))

#julymiss.2<-dplyr::filter(julymiss.2, misstot<15)
july.2<-merge(julymiss.2, july.1, by = "year")
july.2$year2<-july.2$year


#seasonal sums

julytot.1 <- july.2 %>% 
  group_by(year2) %>% 
  summarize(julyppt = sum(amt_mm_mean, na.rm=TRUE))

#july ppt characteristics
july_2017_tot <- july.2 %>%
  dplyr::filter(year == 2017) %>%
  summarize(july_2017 = sum(amt_mm_mean, na.rm = TRUE))

#defining 'event' as > 0.5 mm
july.3 <- mutate(july.2,
                 amt_mm_mean = ifelse(amt_mm_mean >= 0.5, amt_mm_mean, 0)) #remove events less than 0.5 mm
july.3 <- mutate(july.3, event = ifelse(amt_mm_mean >= 0.5, 1, 0)) #define event events less than 0.5 mm
july.3 <- mutate(july.3, event = ifelse(is.na(event), 0, event))
july.3 <- mutate(july.3, amt_mm_mean = ifelse(amt_mm_mean == 0 | is.na(amt_mm_mean), 0, amt_mm_mean)) #remove events less than 0.5 mm

#Mean daily ppt
julymeandailyppt.1<-july.3 %>%
  group_by(year2) %>%
  summarize(meandailyppt = mean(amt_mm_mean, na.rm = TRUE))

#Number of events > 2mm
julynumevents.1<-july.3 %>%
  group_by(year2) %>%
  summarize(numevents = sum(event, na.rm = TRUE))

#Consecutive dry days
julycdd.1<-data.frame()
julywet.1<-data.frame()
julydates.1<-unique(july.3$year2)

for (i in julydates.1) {
  a<-dplyr::filter(july.3, year2 == i)
  b<-rle(a$event)
  b<-data.frame(event=b$values, cdlength =b$lengths)
  b$year2<-i
  julycdd.1<-rbind(julycdd.1, b)
  julywet.1<-rbind(julywet.1, b)
}


julycdd.1<-dplyr::filter(julycdd.1, event == 0)

julywet.1<-dplyr::filter(julywet.1, event == 1)


julycdd.2<-julycdd.1 %>%
  group_by(year2) %>%
  summarise(meancdd = mean(cdlength))

julycwd.2<-julywet.1 %>%
  group_by(year2) %>%
  summarise(meancwd = mean(cdlength),
            n = n(),
            cwd_se = (sd(cdlength)/sqrt(n)),
            cwd_sd = sd(cdlength),
            countcwd = sum(cdlength),
            freq = countcwd/31)

# percentiles of ppt
# july
jul_valecdf <- ecdf(julytot.1$julyppt)

julppt_pctile <- julytot.1 %>%
  group_by(year2, julyppt) %>%
  mutate(year_pctile = jul_valecdf(julyppt) * 100)

# percentiles of cwd freq
# july
july_cwd_valecdf <- ecdf(julycwd.2$freq)

julcwd_pctile <- julycwd.2 %>%
  group_by(year2, freq) %>%
  mutate(year_pctile = july_cwd_valecdf(freq) * 100)


