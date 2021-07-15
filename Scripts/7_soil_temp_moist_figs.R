##Soil Temp and Moisture Figures##
##Greenhouse Mesquite Study
#Austin Rutherford
#1/30/2020

##load packages
library(tidyverse)
library(mvnormtest)
library(vegan)
library(ggpubr)

#bring the data in
gh.temp <- read_csv(file = "Data/soil_temp_daily.csv")
gh.moist <- read_csv(file = "Data/soil_moisture.csv")
gh.moist.start <- read_csv(file = "Data/soil_moisture_start_condition.csv")
gh.temp$rep <- as.factor(gh.temp$rep)#make rep a factor
gh.moist$rep <- as.factor(gh.moist$rep)#make rep a factor
gh.moist$depth <- as.factor(gh.moist$depth)#make depth a factor

###Starting Soil Moisture Mean and SE 
gh.moist.start.mean <- describe(gh.moist.start$moisture)
print(gh.moist.start.mean, digits = 5)

#rename tx variables
gh.moist$tx <- as.character(gh.moist$tx)
gh.moist$tx[gh.moist$tx == "control"] <- "Ambient"
gh.moist$tx[gh.moist$tx == "drought"] <- "Dry"
gh.moist$tx[gh.moist$tx == "wet"] <- "Wet"
gh.temp$tx <- as.character(gh.temp$tx)
gh.temp$tx[gh.temp$tx == "control"] <- "Ambient"
gh.temp$tx[gh.temp$tx == "drought"] <- "Dry"
gh.temp$tx[gh.temp$tx == "wet"] <- "Wet"

#group by variables and summarize moisture to get daily avgs
gh.moist.mean <- gh.moist %>% 
  dplyr::group_by(DOY,tx,depth) %>% 
  summarise(moisture = 100 * mean(moisture)) %>% 
  dplyr::mutate(DOY = factor(DOY))

gh.moist.se <- gh.moist %>% 
  dplyr::group_by(DOY,tx,depth) %>% 
  summarise(moisture_mean =  100*mean(moisture),
            moisture_se = 100*(sd(moisture)/sqrt(2)),
            m_se_hi = (moisture_mean + moisture_se),
            m_se_lo = (moisture_mean - moisture_se)) %>% 
  dplyr::mutate(DOY = factor(DOY),
                tx = factor(tx)) %>% 
  # make a new column for day of experiment (DOE)
  mutate(DOE = recode(DOY,
                      "134" = "0",
                      "135" = "1",
                      "136" = "2",
                      "137" = "3",
                      "138" = "4",
                      "139" = "5",
                      "140" = "6",
                      "141" = "7",
                      "142" = "8",
                      "143" = "9",
                      "144" = "10",
                      "145" = "11",
                      "146" = "12",
                      "147" = "13",
                      "148" = "14",
                      "149" = "15",
                      "150" = "16",
                      "151" = "17",
                      "152" = "18",
                      "153" = "19",
                      "154" = "20",
                      "155" = "21",
                      "156" = "22"))

#soil moisture graph
moist <- gh.moist.se %>%
  mutate(DOE = as.numeric(as.character(DOE))) %>%
  ggline(x="DOE", y="moisture_mean", plot_type = "l",
       color = "tx",
       palette = c("grey30","#DB4325","blue"),
       xlab = "Day of Experiment",
       ylab = expression(Volumetric~Water~Content~textstyle("(%)")),
       size = 1,
       legend = "bottom",
       legend.title = "Watering Treatment",
       ylim = c(0, 30))
facet(moist, facet.by = "depth",
      panel.labs = list(depth=c("5 cm","20 cm")),
      ncol = 1,
      nrow = 2)
#ggsave("Figures/soilmoist.tiff", plot = moist, dpi = 1200, width = 10, limitsize = FALSE)

#soil moisture graph with standard error via ggplot2
depth_labels <-  c("5" = "5 cm","20" = "20 cm")
plot_labels <- data.frame(lbl = c("a.", "b."))

moist2 <- gh.moist.se %>%
  mutate(DOE = as.numeric(as.character(DOE))) %>%
  ggplot(aes(x = DOE,
             y = moisture_mean,
             color = tx))+
  scale_x_continuous(breaks = seq(0, 22, 1), expand = c(0,0)) +
  scale_color_manual(values = c("grey30","#DB4325","blue")) +
  ylim(0, 30) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = m_se_lo,
                  ymax = m_se_hi, group = tx), linetype = 3, alpha = 0.2) +
  facet_wrap(~depth, ncol = 1,
             labeller = labeller(depth = depth_labels)) +
  labs(x = "Day of Experiment",
       y = "Volumetric Water Content (%)",
       color = "Watering Treatment") +
  theme_pubr(legend = c(0.5, 0.65)) +
  theme(axis.title.y = element_text(),
        axis.title.x = element_text(),
        legend.title = element_text())+
  guides(colour = guide_legend(nrow = 1, title.position = "left"))

moist2

ggsave("Figures/soilmoist.tiff", plot = moist2, dpi = 1200, limitsize = FALSE)

#group by variables and summarize temperature to get daily avgs
gh.temp.sum <- gh.temp %>% 
  group_by(DOY,tx) %>% 
  summarise(avgtemp = mean(avgtemp),
            maxtemp = mean(maxtemp),
            mintemp = mean(mintemp)) %>% 
  gather(key = "tempvar", value = "temp",-DOY, -tx)

#soil temp graph
temp <- ggline(gh.temp.sum, x="DOY", y="temp", merge = TRUE, plot_type = "l",
                color = "tx",
                palette = c("grey30","#DB4325","blue"),
                xlab = "Day of Year",
                ylab = expression(Temperature~textstyle("(")~C^{"o"}~textstyle(")")),
                size = 1,
                legend = "right",
                legend.title = "Treatment")
facet(temp, facet.by = "tempvar",
      panel.labs = list(tempvar = c("Mean", "Maximum", "Minimum")),
      ncol = 1,
      nrow = 3,
      scales = "free")
#ggsave("Figures/soiltemp.tiff", dpi = 1200, limitsize = FALSE, width = 6, height = 6)

      