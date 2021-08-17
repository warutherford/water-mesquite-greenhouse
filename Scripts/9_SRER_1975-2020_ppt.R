## Santa Rita Experimental Range Desert Grassland Exclosure##
## Historic weather data 1975- July 2017
## Greenhouse Mesquite Study
# Austin Rutherford
# 2021-07-23
# email: arutherford@email.arizona.edu

## Address:how close 2017 precipitation (amount and pattern) matches the long-term record”
## we might look at the long-term record and determine how many times the amount/pattern 
## (approx) used in our experiment has occurred in the historical record (e.g. frequency of occurrence).
## Rarely/episodically? Frequently? Commonly? Or somewhere in between one of these categories. 
## We might also determine if the frequency of the pattern has changed through time (e.g. maybe the 
## frequency was higher in the early 1900s, and is less frequent in the last decade or two [or vice-versa];
## or maybe the frequency has not changed much over the decades).

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
#fall
fall.1<-dplyr::filter(srer_filled, month %in% c(10, 11, 12)) #Fall = oct, nov, dec
fallmiss.1<-fall.1
fallmiss.1$miss<-ifelse(is.na(fallmiss.1$amt_mm_mean), 1, 0)

fallmiss.2<-fallmiss.1%>%
  group_by(year)%>%
  summarise(misstot = sum(miss, na.rm=TRUE))

#fallmiss.2<-dplyr::filter(fallmiss.2, misstot < 15)
fall.2<-merge(fallmiss.2, fall.1, by = "year")
fall.2$year2<-fall.2$year+1

#winter season  
cool.1<-dplyr::filter(srer_filled, month %in%  c(1:3)) #Cool = Jan-Mar
cool.1$year2<-ifelse(cool.1$month > 10, cool.1$year+1, cool.1$year)
coolmiss.1<-cool.1
coolmiss.1$miss<-ifelse(is.na(coolmiss.1$amt_mm_mean), 1, 0)

coolmiss.2<-coolmiss.1%>%
  group_by(year2)%>%
  summarise(misstot = sum(miss, na.rm=TRUE))

#coolmiss.2<-dplyr::filter(coolmiss.2, misstot<15)
cool.2<-merge(coolmiss.2, cool.1, by = "year2")

#monsoon
warm.1<-dplyr::filter(srer_filled, month %in% c(7:9)) #Warm = july-sept
warmmiss.1<-warm.1
warmmiss.1$miss<-ifelse(is.na(warmmiss.1$amt_mm_mean), 1, 0)

warmmiss.2<-warmmiss.1%>%
  group_by(year)%>%
  summarise(misstot = sum(miss, na.rm=TRUE))

#warmmiss.2<-dplyr::filter(warmmiss.2, misstot<15)
warm.2<-merge(warmmiss.2, warm.1, by = "year")
warm.2$year2<-warm.2$year

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

#spring
spring.1<-dplyr::filter(srer_filled, month %in% c(4:6)) #spring = Apr - June
springmiss.1<-spring.1
springmiss.1$miss<-ifelse(is.na(springmiss.1$amt_mm_mean), 1, 0)

springmiss.2<-springmiss.1%>%
  group_by(year)%>%
  summarise(misstot = sum(miss, na.rm=TRUE))

#springmiss.2<-dplyr::filter(springmiss.2, misstot<15)
spring.2<-merge(springmiss.2, spring.1, by = "year")
spring.2$year2<-spring.2$year

######################################################
## 2. Calc metrics
######################################################

#seasonal sums
falltot.1<-fall.2%>%
  group_by(year2)%>%
  summarize(fallppt = sum(amt_mm_mean, na.rm=TRUE))

cooltot.1<-cool.2%>%
  group_by(year2)%>%
  summarize(coolppt = sum(amt_mm_mean, na.rm=TRUE))

warmtot.1<-warm.2%>%
  group_by(year2)%>%
  summarize(warmppt = sum(amt_mm_mean, na.rm=TRUE))

julytot.1 <- july.2 %>% 
  group_by(year2) %>% 
  summarize(julyppt = sum(amt_mm_mean, na.rm=TRUE))

springtot.1<-spring.2%>%
  group_by(year2)%>%
  summarize(springppt = sum(amt_mm_mean, na.rm=TRUE))

prvwarmtot.1<-warmtot.1
prvwarmtot.1$year2=prvwarmtot.1$year2+1
colnames(prvwarmtot.1)[2]<-"prvwarmppt"

#warm season ppt characteristics
#defining 'event' as > 0.5 mm
warm.3 <- mutate(warm.2,
                 amt_mm_mean = ifelse(amt_mm_mean >= 0.5, amt_mm_mean, 0)) #remove events less than 0.5 mm
warm.3 <- mutate(warm.3, event = ifelse(amt_mm_mean >= 0.5, 1, 0)) #define event events less than 0.5 mm
warm.3 <- mutate(warm.3, amt_mm_mean = ifelse(amt_mm_mean == 0 , NA, amt_mm_mean)) #remove events less than 0.5 mm

#Mean daily ppt
warmmeandailyppt.1<-warm.3 %>%
  group_by(year2) %>%
  summarize(meandailyppt = mean(amt_mm_mean, na.rm = TRUE))

#Number of events > 2mm
warmnumevents.1<-warm.3 %>%
  group_by(year2) %>%
  summarize(numevents = sum(event, na.rm = TRUE))

#Consecutive dry days
cdd.1<-data.frame()
dates.1<-unique(warm.3$year2)

for (i in dates.1) {
  a<-dplyr::filter(warm.3, year2 == i)
  b<-rle(a$event)
  b<-data.frame(event=b$values, cddlength =b$lengths)
  b$year2<-i
  cdd.1<-rbind(cdd.1, b)
}


cdd.1<-dplyr::filter(cdd.1, event == 0)

cdd.2<-cdd.1 %>%
  group_by(year2) %>%
  summarise(meancdd = mean(cddlength))

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

for (i in dates.1) {
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

## Consecutive wet days exact to exp = July 9 to August 3
srer_exp <- srer_filled %>% 
  dplyr::filter(month >= 7 & month <= 8) %>% 
  dplyr::filter(month == 7 & day >= 9 | month == 8 & day <= 3)

#experiment time
exp.1 <-srer_exp
expmiss.1<-exp.1
expmiss.1$miss<-ifelse(is.na(expmiss.1$amt_mm_mean), 1, 0)

expmiss.2<-expmiss.1%>%
  group_by(year)%>%
  summarise(misstot = sum(miss, na.rm=TRUE))

exp.2<-merge(expmiss.2, exp.1, by = "year")
exp.2$year2<-exp.2$year

exptot.1 <- exp.2 %>% 
  group_by(year2) %>% 
  summarize(expppt = sum(amt_mm_mean, na.rm=TRUE))

#exp ppt characteristics
exp_2017_tot <- exp.2 %>%
  dplyr::filter(year == 2017) %>%
  summarize(exp_2017 = sum(amt_mm_mean, na.rm = TRUE))

#defining 'event' as > 0.5 mm
exp.3 <- mutate(exp.2,
                 amt_mm_mean = ifelse(amt_mm_mean >= 0.5, amt_mm_mean, 0)) #remove events less than 0.5 mm
exp.3 <- mutate(exp.3, event = ifelse(amt_mm_mean >= 0.5, 1, 0)) #define event events less than 0.5 mm
exp.3 <- mutate(exp.3, event = ifelse(is.na(event), 0, event))
exp.3 <- mutate(exp.3, amt_mm_mean = ifelse(amt_mm_mean == 0 | is.na(amt_mm_mean), 0, amt_mm_mean)) #remove events less than 0.5 mm

#Mean daily ppt
expmeandailyppt.1<-exp.3 %>%
  group_by(year2) %>%
  summarize(meandailyppt = mean(amt_mm_mean, na.rm = TRUE))

#Number of events > 2mm
expnumevents.1<-exp.3 %>%
  group_by(year2) %>%
  summarize(numevents = sum(event, na.rm = TRUE))

#Consecutive dry days
expcdd.1<-data.frame()
expwet.1<-data.frame()
expdates.1<-unique(exp.3$year2)

for (i in dates.1) {
  a<-dplyr::filter(exp.3, year2 == i)
  b<-rle(a$event)
  b<-data.frame(event=b$values, cdlength =b$lengths)
  b$year2<-i
  expcdd.1<-rbind(expcdd.1, b)
  expwet.1<-rbind(expwet.1, b)
}


expcdd.1<-dplyr::filter(expcdd.1, event == 0)

expwet.1<-dplyr::filter(expwet.1, event == 1)


expcdd.2<-expcdd.1 %>%
  group_by(year2) %>%
  summarise(meancdd = mean(cdlength))

expcwd.2<-expwet.1 %>%
  group_by(year2) %>%
  summarise(meancwd = mean(cdlength),
            n = n(),
            cwd_se = (sd(cdlength)/sqrt(n)),
            cwd_sd = sd(cdlength),
            countcwd = sum(cdlength),
            freq = countcwd/25)

#monsoon as a whole consec wet days (def:June 15 - Sept 30), missing part of monsoon for 1975
srer_monsoon <- srer_filled %>% 
  dplyr::filter(month >= 6 & month <= 9 & year != 1975) %>% 
  dplyr::filter(month == 6 & between(day, 15, 30) | month == 7 | month == 8 | month == 9)

#monsoon
mon.1<-dplyr::filter(srer_filled, month %in% c(6:9)) #mon = june-sept
monmiss.1<-mon.1
monmiss.1$miss<-ifelse(is.na(monmiss.1$amt_mm_mean), 1, 0)

monmiss.2<-monmiss.1%>%
  group_by(year)%>%
  summarise(misstot = sum(miss, na.rm=TRUE))

mon.2<-merge(monmiss.2, mon.1, by = "year")
mon.2$year2<-mon.2$year

montot.1<-mon.2%>%
  group_by(year2)%>%
  summarize(monppt = sum(amt_mm_mean, na.rm=TRUE))

#monsoon time
mon.1 <-srer_monsoon
monmiss.1<-mon.1
monmiss.1$miss<-ifelse(is.na(monmiss.1$amt_mm_mean), 1, 0)

monmiss.2<-monmiss.1%>%
  group_by(year)%>%
  summarise(misstot = sum(miss, na.rm=TRUE))

mon.2<-merge(monmiss.2, mon.1, by = "year")
mon.2$year2<-mon.2$year

montot.1 <- mon.2 %>% 
  group_by(year2) %>% 
  summarize(monppt = sum(amt_mm_mean, na.rm=TRUE))

#mon ppt characteristics
mon_2017_tot <- mon.2 %>%
  dplyr::filter(year == 2017) %>%
  summarize(mon_2017 = sum(amt_mm_mean, na.rm = TRUE))

#defining 'event' as > 0.5 mm
mon.3 <- mutate(mon.2,
                amt_mm_mean = ifelse(amt_mm_mean >= 0.5, amt_mm_mean, 0)) #remove events less than 0.5 mm
mon.3 <- mutate(mon.3, event = ifelse(amt_mm_mean >= 0.5, 1, 0)) #define event events less than 0.5 mm
mon.3 <- mutate(mon.3, event = ifelse(is.na(event), 0, event))
mon.3 <- mutate(mon.3, amt_mm_mean = ifelse(amt_mm_mean == 0 | is.na(amt_mm_mean), 0, amt_mm_mean)) #remove events less than 0.5 mm

#Mean daily ppt
monmeandailyppt.1<-mon.3 %>%
  group_by(year2) %>%
  summarize(meandailyppt = mean(amt_mm_mean, na.rm = TRUE))

#Number of events > 2mm
monnumevents.1<-mon.3 %>%
  group_by(year2) %>%
  summarize(numevents = sum(event, na.rm = TRUE))

#Consecutive dry days
moncdd.1<-data.frame()
monwet.1<-data.frame()
mondates.1<-unique(mon.3$year2)

for (i in dates.1[2:43]) {
  a<-dplyr::filter(mon.3, year2 == i)
  b<-rle(a$event)
  b<-data.frame(event=b$values, cdlength =b$lengths)
  b$year2<-i
  moncdd.1<-rbind(moncdd.1, b)
  monwet.1<-rbind(monwet.1, b)
}


moncdd.1<-dplyr::filter(moncdd.1, event == 0)

monwet.1<-dplyr::filter(monwet.1, event == 1)


moncdd.2<-moncdd.1 %>%
  group_by(year2) %>%
  summarise(meancdd = mean(cdlength))

moncwd.2<-monwet.1 %>%
  group_by(year2) %>%
  summarise(meancwd = mean(cdlength),
            n = n(),
            cwd_se = (sd(cdlength)/sqrt(n)),
            cwd_sd = sd(cdlength),
            countcwd = sum(cdlength),
            freq = countcwd/107)

# percentiles of ppt
# monsoon
mon_valecdf <- ecdf(montot.1$monppt)

monppt_pctile <- montot.1 %>%
  group_by(year2, monppt) %>%
  dplyr::filter(year2 != 1975) %>% 
  mutate(year_pctile = mon_valecdf(monppt) * 100)

# july only
jul_valecdf <- ecdf(julytot.1$julyppt)

julppt_pctile <- julytot.1 %>%
  group_by(year2, julyppt) %>%
  mutate(year_pctile = jul_valecdf(julyppt) * 100)

# experiment period
exp_valecdf <- ecdf(exptot.1$expppt)

expppt_pctile <- exptot.1 %>%
  group_by(year2, expppt) %>%
  mutate(year_pctile = exp_valecdf(expppt) * 100)

# percentiles of cwd freq
# monsoon
mon_cwd_valecdf <- ecdf(moncwd.2$freq)

moncwd_pctile <- moncwd.2 %>%
  group_by(year2, freq) %>%
  dplyr::filter(year2 != 1975) %>% 
  mutate(year_pctile = mon_cwd_valecdf(freq) * 100)

# july only
july_cwd_valecdf <- ecdf(julycwd.2$freq)

julcwd_pctile <- julycwd.2 %>%
  group_by(year2, freq) %>%
  mutate(year_pctile = july_cwd_valecdf(freq) * 100)

# experiment period
exp_cwd_valecdf <- ecdf(expcwd.2$freq)

expcwd_pctile <- expcwd.2 %>%
  group_by(year2, freq) %>%
  mutate(year_pctile = exp_cwd_valecdf(freq) * 100)

