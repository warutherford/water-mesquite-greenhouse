##Variable Figures##
##Greenhouse Mesquite Study
#Austin Rutherford
#email: arutherford@email.arizona.edu
#1/31/2020

##load packages and force dplyr over plyr
library(tidyverse)
arrange <- dplyr::arrange
summarise <-dplyr::summarise
library(ggpubr)
library(psych)
library(rcompanion)
library(Rmisc)
library(pastecs)

# Load the data without pot# or PCAs in the original csv file
gh <- read.csv(file = "Data/prve_gh_master_data.csv", 
               colClasses=c("NULL", NA,NA,NA,NA,
                            NA,NA,NA,NA,NA,NA,NA,
                            NA,NA,NA,NA,NA,NA,NA,
                            NA,NA,NA,NA,NA,NA,NA,
                            NA,NA,NA,NA,NA,NA,NA,NA,NA))

#rename sampling point to day sampled
gh$sampling <- as.character(gh$sampling)
gh$sampling[gh$sampling == "1"] <- "Day 11"
gh$sampling[gh$sampling == "2"] <- "Day 22"

gh$sampling <- as.factor(gh$sampling)#make sampling period a factor
gh$tx <- as.factor(gh$tx)#make tx a factor


##AGR Boxplot
#create summary of variable and arrange desc by mean
agr <- gh %>% distinct(agr, sampling, tx) %>% 
  arrange(agr, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(agr = mean(agr, na.rm=TRUE)) %>% 
  arrange(sampling,tx)

#create new column for y value offset above the mean
agr$yloc = c(7, 5, 7, 12, 8, 12)

#create lettering report label based on tx*sampling Tukey HSD
agr$label = c("a", "b", "ab", "c", "a", "c") 

#make plot with labels
ggboxplot(gh, x = "sampling", y = "agr",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,2,1,2,1,2),
          short.panel.labs = FALSE,
          ylim = c(-2, 12),
          legend = "none",
          ylab = expression(Absolute~Growth~Rate~textstyle("(mg")~d^{"-1"}*textstyle(")")),
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = agr, aes(y=yloc, label = label))
ggsave("Figures/agr.tiff", height = 4, width = 5, dpi=1200)

##R:S Boxplot
#create summary of variable and arrange desc by mean
rs <- gh %>% distinct(rootshoot, sampling, tx) %>% 
  arrange(rootshoot, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(rootshoot = mean(rootshoot, na.rm=TRUE)) %>% 
  arrange(sampling,tx)

#create new column for y value offset above the mean
rs$yloc = c(0.4, 0.4, 0.45, 0.6, 0.6, 0.5)

#create lettering report label based on tx*sampling Tukey HSD
rs$label = c("ab", "a", "ab", "c", "bc", "c") 

#make plot with labels
ggboxplot(gh, x = "sampling", y = "rootshoot",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,2,1,2,1,2),
          short.panel.labs = FALSE,
          ylim = c(0, 0.70),
          legend = "none",
          ylab = "Root : Shoot",
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr()) +
  geom_text(data = rs, aes(y=yloc, label = label))
ggsave("Figures/rootshoot.tiff", height = 4, width = 5, dpi=1200)

##SLA Boxplot
#create summary of variable and arrange desc by mean
sla.sum <- gh %>% distinct(sla, sampling, tx) %>% 
  arrange(sla, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(sla = mean(sla, na.rm=TRUE)) %>% 
  arrange(sampling,tx)

#create new column for y value offset above the mean
sla.sum$yloc = c(0.3, 0.4, 0.3, 0.3, 0.4, 0.3)

#create lettering report label based on tx*sampling Tukey HSD
sla.sum$label = c("ab", "ac", "b", "b", "c", "ab") 

#make plot with labels
ggboxplot(gh, x = "sampling", y = "sla",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,2,1,2,1,2),
          short.panel.labs = FALSE,
          ylim = c(0, 0.5),
          legend = "none",
          ylab = expression(Specific~Leaf~Area~textstyle("(")*m^{"-2"}~g^{"-1"}*textstyle(")")),
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = sla.sum, aes(y=yloc, label = label))
ggsave("Figures/sla.tiff", height = 4, width = 5, dpi=1200)

##Leaf Biomass Boxplot
#create summary of variable and arrange desc by mean
dryleaf.sum <- gh %>% distinct(dryleaf, sampling, tx) %>% 
  arrange(dryleaf, sampling, tx) %>% 
  group_by(tx, sampling) %>%
  summarise(dryleaf = mean(dryleaf, na.rm=TRUE)) %>% 
  arrange(sampling, tx)

#create new column for y value offset above the mean
dryleaf.sum$yloc = c(18, 10, 15, 20, 15, 18)

#create lettering report label based on tx*sampling Tukey HSD
dryleaf.sum$label = c("bc", "c", "bc", "a", "bc", "ab") 

#make plot with labels, grams to mg
gh.mg <- gh %>% 
  mutate(dryleaf = 1000*dryleaf)

ggboxplot(gh.mg, x = "sampling", y = "dryleaf",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,2,1,2,1,2),
          short.panel.labs = FALSE,
          ylim = c(0, 20),
          legend = "none",
          ylab = expression(Leaf~Biomass~textstyle("(mg)")),
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = dryleaf.sum, aes(y=yloc, label = label))
ggsave("Figures/leafbiomass.tiff", height = 4, width = 5, dpi=1200)

##Total Root Length Boxplot
#create summary of variable and arrange desc by mean
totrootlength.sum <- gh %>% distinct(totrootlnth, sampling, tx) %>% 
  arrange(totrootlnth, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(totrootlnth = mean(totrootlnth, na.rm=TRUE)) %>% 
  arrange(sampling,tx)

#create new column for y value offset above the mean
totrootlength.sum$yloc = c(750, 500, 750, 1900, 1500, 1750)

#create lettering report label based on tx*sampling Tukey HSD
totrootlength.sum$label = c("ab", "a", "a", "c", "b", "c") 

#make plot with labels
ggboxplot(gh, x = "sampling", y = "totrootlnth",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,2,1,2,1,2),
          short.panel.labs = FALSE,
          ylim = c(0, 2000),
          legend = "none",
          ylab = "Total Root Length (mm)",
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = totrootlength.sum, aes(y=yloc, label = label))
ggsave("Figures/rootlength.tiff", height = 4, width = 5, dpi=1200)

##Tap Root Length Boxplot
#create summary of variable and arrange desc by mean
taprootlength.sum <- gh %>% distinct(taprootlnth, sampling, tx) %>% 
  arrange(taprootlnth, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(taprootlnth = mean(taprootlnth, na.rm=TRUE)) %>% 
  arrange(sampling,tx)

#create new column for y value offset above the mean
taprootlength.sum$yloc = c(400, 300, 400, 550, 350, 500)

#create lettering report label based on tx*sampling Tukey HSD
taprootlength.sum$label = c("a", "b", "ab", "c", "ab", "c") 

#make plot with labels
ggboxplot(gh, x = "sampling", y = "taprootlnth",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,2,1,2,1,2),
          short.panel.labs = FALSE,
          ylim = c(0, 600),
          legend = "none",
          ylab = "Tap Root Length (mm)",
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = taprootlength.sum, aes(y=yloc, label = label))
ggsave("Figures/taprootlength.tiff", height = 4, width = 5, dpi=1200)

##Root Volume Boxplot
#create summary of variable and arrange desc by mean
rootvol.sum <- gh %>% distinct(rootvol, sampling, tx) %>% 
  arrange(rootvol, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(rootvol = mean(rootvol, na.rm=TRUE)) %>% 
  arrange(sampling,tx)

#create new column for y value offset above the mean
rootvol.sum$yloc = c(200, 150, 200, 550, 350, 350)

#create lettering report label based on tx*sampling Tukey HSD
rootvol.sum$label = c("ab", "a", "a", "c", "b", "d") 

#make plot with labels
ggboxplot(gh, x = "sampling", y = "rootvol",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,2,1,2,1,2),
          short.panel.labs = FALSE,
          ylim = c(0, 600),
          legend = "none",
          ylab = expression(Root~Volume~textstyle("(")*mm^{"3"}*textstyle(")")),
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = rootvol.sum, aes(y=yloc, label = label))
ggsave("Figures/rootvol.tiff", height = 4, width = 5, dpi=1200)

##Root Surface Area Boxplot
#create summary of variable and arrange desc by mean
surfarea.sum <- gh %>% distinct(surfarea, sampling, tx) %>% 
  arrange(surfarea, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(surfarea = mean(surfarea, na.rm=TRUE)) %>% 
  arrange(sampling,tx)

#create new column for y value offset above the mean
surfarea.sum$yloc = c(1000, 1000, 1200, 1500, 1500, 1500)

#create lettering report label based on tx*sampling Tukey HSD
surfarea.sum$label = c("ab", "c", "a", "d", "b", "d") 

#make plot with labels
ggboxplot(gh, x = "sampling", y = "surfarea",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,2,1,2,1,2),
          short.panel.labs = FALSE,
          ylim = c(0, 1500),
          legend = "none",
          ylab = expression(Root~Surface~Area~textstyle("(")*mm^{"2"}*textstyle(")")),
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = surfarea.sum, aes(y=yloc, label = label))
ggsave("Figures/rootsurfarea.tiff", height = 4, width = 5, dpi=1200)

# SA:V root ratio boxplot for Day 22
#create summary of variable and arrange desc by mean
rootratio <- gh %>% distinct(surfarea, rootvol, sampling, tx) %>% 
  drop_na() %>% 
  dplyr::filter(sampling == "Day 22") %>% 
  arrange(surfarea, rootvol, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  mutate(rootratio = surfarea/rootvol) %>% 
  arrange(sampling,tx)

rootratio.sum <- rootratio %>% 
  dplyr::filter(sampling == "Day 22") %>% 
  group_by(tx) %>%
  drop_na() %>% 
  summarise(rootratio_mean = mean(rootratio),
            rootratio_se = sd(rootratio)/sqrt(n()))

hist(log(rootratio$rootratio))

m.rootratio <- aov(log(rootratio)~tx, data=rootratio)

means.rootratio <- emmeans(m.rootratio, "tx")

tuk.rootratio <- cld(means.rootratio, Letter="abcde", sort=FALSE)
tuk.rootratio

#create new column for y value offset above the mean
rootratio.sum$yloc = c(5, 10, 7.5)

#create lettering report label based on tx*sampling Tukey HSD
rootratio.sum$label = c("a", "b", "b") 

#make plot with labels
ggbarplot(rootratio,
          x = "tx",
          y = "rootratio",
          color = "tx",
          fill = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          add = "mean_se",
          add.params = list(color = "black", size = 1),
          ylim = c(0, 10),
          legend = "none",
          ylab = "Root Surface Area : Volume (unitless)",
          xlab = FALSE,
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = rootratio.sum, aes(y=yloc, label = label))
ggsave("Figures/rootratio.tiff", height = 4, width = 5, dpi=1200)


##Root Biomass Boxplot
#create summary of variable and arrange desc by mean
rootwt.sum <- gh %>% distinct(rootwt, sampling, tx) %>% 
  arrange(rootwt, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(rootwt = mean(rootwt, na.rm=TRUE)) %>% 
  arrange(sampling,tx)

#create new column for y value offset above the mean
rootwt.sum$yloc = c(25, 25, 25, 90, 60, 90)

#create lettering report label based on tx*sampling Tukey HSD
rootwt.sum$label = c("bc", "c", "c", "a", "b", "a") 

#make plot with labels
gh %>% mutate(rootwt = 1000*rootwt) %>% 
ggboxplot(x = "sampling", y = "rootwt",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,2,1,2,1,2),
          short.panel.labs = FALSE,
          ylim = c(0, 100),
          legend = "none",
          ylab = expression(Root~Biomass~textstyle("(")*mg*textstyle(")")),
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          #panel.labs.font = list(color = "red"),
          ggtheme = theme_pubr())+
  geom_text(data = rootwt.sum, aes(y=yloc, label = label))
ggsave("Figures/rootwt.tiff", height = 4, width = 5, dpi=1200)

##Water Potential Boxplot
#create summary of variable and arrange desc by mean
wp.sum <- gh %>% distinct(wp, sampling, tx) %>% 
  arrange(wp, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(wp = mean(wp, na.rm=TRUE)) %>% 
  arrange(sampling,tx)

#create new column for y value offset above the mean
wp.sum$yloc = c(-0.75, -0.5, -0.25, -1, -1.25, -0.5)

#create lettering report label based on tx*sampling Tukey HSD
wp.sum$label = c("ab", "abc", "a", "bc", "c", "ab") 

#make plot with labels
ggboxplot(gh, x = "sampling", y = "wp",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,2,1,2,1,2),
          short.panel.labs = FALSE,
          ylim = c(-2.5, 0),
          legend = "none",
          ylab = "Water Potential (MPa)",
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = wp.sum, aes(y=yloc, label = label))
ggsave("Figures/wp.tiff", height = 4, width = 5, dpi=1200)

##Fresh Weight Boxplot
#create summary of variable and arrange desc by mean
wetsd.sum <- gh %>% distinct(wetsdling, sampling, tx) %>% 
  arrange(wetsdling, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(wetsdling = 1000* mean(wetsdling, na.rm=TRUE)) %>% 
  arrange(sampling,tx)

#create new column for y value offset above the mean
wetsd.sum$yloc = c(400, 250, 400, 700, 500, 750)

#create lettering report label based on tx*sampling Tukey HSD
wetsd.sum$label = c("a", "b", "ab", "c", "a", "c") 

#make plot with labels
#change grams to mg
gh.wetsd <- gh %>% mutate(wetsdling = 1000*wetsdling)

ggboxplot(gh.wetsd, x = "sampling", y = "wetsdling",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,2,1,2,1,2),
          short.panel.labs = TRUE,
          ylim = c(0, 800),
          legend = "none",
          ylab = "Fresh Weight (mg)",
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          labeller = "label_value",
          ggtheme = theme_pubr(base_size = 10))+
  geom_text(data = wetsd.sum, aes(y=yloc, label = label))
ggsave("Figures/freshwt.tiff", height = 4, width = 5, dpi=1200)

##Seedling Water % Boxplot
#create summary of variable and arrange desc by mean
sdwtr.sum <- gh %>% distinct(sdlingwtr, sampling, tx) %>% 
  arrange(sdlingwtr, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(sdlingwtr = mean(sdlingwtr, na.rm=TRUE)) %>% 
  arrange(sampling,tx)

#create new column for y value offset above the mean
sdwtr.sum$yloc = c(85, 85, 85, 77, 85, 75)

#create lettering report label based on tx*sampling Tukey HSD
sdwtr.sum$label = c("a", "ab", "a", "bc", "ab", "c") 

#make plot with labels
ggboxplot(gh, x = "sampling", y = "sdlingwtr",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,2,1,2,1,2),
          short.panel.labs = FALSE,
          ylim = c(60, 90),
          legend = "none",
          ylab = "Seedling Water Content (%)",
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = sdwtr.sum, aes(y=yloc, label = label))
ggsave("Figures/seedlingwaterper.tiff", height = 4, width = 5, dpi=1200)

##GWC Boxplot
#create summary of variable and arrange desc by mean
gwc.sum <- gh %>% distinct(fivegwc, twentygwc, sampling, tx) %>% 
  arrange(fivegwc, twentygwc, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(fivegwc = mean(fivegwc, na.rm=TRUE),
            twentygwc = mean(twentygwc, na.rm = TRUE)) %>% 
  arrange(sampling,tx) %>% 
  gather(key = "depth", value = "gwc", -sampling, -tx)

#create new column for y value offset above the mean
gwc.sum$yloc = c(7,5,8,7,5,8,14,9,14,14,9,14)


#create lettering report label based on tx*sampling Tukey HSD
gwc.sum$label = c("", "", "", "", "", "","a", "b", "a", "a", "b", "a") #currently in format for 5-20cm, fill"" to do 0-5

#make plot with labels
#0-5cm
ggboxplot(gh, x = "sampling", y = "fivegwc",
          color = "tx",
          #combine = FALSE,
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,1,1,1,1,1),
          short.panel.labs = FALSE,
          ylim = c(0, 8),
          legend = "none",
          ylab = "Gravimetric Water Content (%)",
          xlab = "Sampling Point",
          panel.labs = list(tx=c("0-5 cm Ambient", "0-5 cm Drought", "0-5 cm Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = gwc.sum, aes(x=sampling, y=yloc, label = label, group = tx),
             position=position_dodge(width=1), inherit.aes = FALSE)
ggsave("Figures/fivegwc.tiff", height = 4, width = 5, dpi=1200)

#5-20cm
ggboxplot(gh, x = "sampling", y = "twentygwc",
          color = "tx",
          #combine = FALSE,
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,1,1,1,1,1),
          short.panel.labs = FALSE,
          ylim = c(0, 15),
          legend = "none",
          ylab = "Gravimetric Water Content (%)",
          xlab = "Sampling Point",
          panel.labs = list(tx=c("5-20 cm Ambient", "5-20 cm Drought", "5-20 cm Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = gwc.sum, aes(x=sampling, y=yloc, label = label, group = tx),
            position=position_dodge(width=1), inherit.aes = FALSE)
ggsave("Figures/twentygwc.tiff", height = 4, width = 5, dpi=1200)

##Fine Roots Barplot
#create summary of variable and arrange desc by mean
fr.sum <- gh %>% distinct(fineroots, sampling, tx) %>% 
  arrange(fineroots, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(fineroots = mean(fineroots, na.rm=TRUE)) %>% 
  arrange(sampling,tx)

#create new column for y value offset above the mean
fr.sum$yloc = c(45, 20, 30, 93, 73, 73)

#create lettering report label based on tx*sampling Tukey HSD
fr.sum$label = c("ab", "c", "ac", "d", "be", "e") 

#make plot with labels
ggbarplot(gh, x = "sampling", y = "fineroots",
          fill = "tx",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          add = "mean_se",
          add.params = list(color = "black", size = 1),
          short.panel.labs = FALSE,
          ylim = c(0, 100),
          legend = "none",
          ylab = "Fine Roots (#)",
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = fr.sum, aes(y=yloc, label = label))
ggsave("Figures/fineroots.tiff", height = 4, width = 5, dpi=1200)

#####Ecophys Boxplots##########
#load data
ecophys <- read.csv("Data/ecophys.csv")
summary(ecophys)
ecophys$sampling <- as.factor(ecophys$sampling)#make sampling period a factor
ecophys$tx <- as.character(ecophys$tx)
ecophys$tx[ecophys$tx == "Control"] <- "Ambient"

#create summary of variable and arrange desc by mean
anet.sum <- ecophys %>% distinct(anet, sampling, tx) %>% 
  arrange(anet, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(anet = mean(anet, na.rm=TRUE)) %>% 
  arrange(sampling,tx)

#create new column for y value offset above the mean
anet.sum$yloc = c(27, 10, 25, 20, 15, 18)

#create lettering report label based on tx*sampling Tukey HSD
anet.sum$label = c("a", "b", "a", "ac", "bc", "c") 

#make plot with labels
ggboxplot(ecophys, x = "sampling", y = "anet",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,2,1,2,1,2),
          short.panel.labs = FALSE,
          ylim = c(-5, 30),
          legend = "none",
          ylab = expression(A[net]~textstyle("(μmol CO"[2])~m^{"-2"}~s^{"-1"}*textstyle(")")),
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = anet.sum, aes(y=yloc, label = label))
ggsave("Figures/anet.tiff", height = 4, width = 5, dpi=1200)

#create summary of variable and arrange desc by mean
cond.sum <- ecophys %>% distinct(cond, sampling, tx) %>% 
  arrange(cond, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(cond = mean(cond, na.rm=TRUE)) %>% 
  arrange(sampling,tx)

#create new column for y value offset above the mean
cond.sum$yloc = c(0.23, 0.10, 0.25, 0.18, 0.10, 0.18)

#create lettering report label based on tx*sampling Tukey HSD
cond.sum$label = c("ab", "c", "a", "b", "c", "b") 

#make plot with labels
ggboxplot(ecophys, x = "sampling", y = "cond",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,2,1,2,1,2),
          short.panel.labs = FALSE,
          ylim = c(0, 0.25),
          legend = "none",
          ylab = expression(g[s]~textstyle("(mol H")[2]*O~m^{"-2"}~s^{"-1"}*textstyle(")")),
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = cond.sum, aes(y=yloc, label = label))
ggsave("Figures/cond.tiff", height = 4, width = 5, dpi=1200)

#create summary of variable and arrange desc by mean
trans.sum <- ecophys %>% distinct(trans, sampling, tx) %>% 
  arrange(trans, sampling, tx) %>% 
  group_by(sampling, tx) %>% 
  summarise(trans = mean(trans, na.rm=TRUE)) %>% 
  arrange(sampling,tx)

#create new column for y value offset above the mean
trans.sum$yloc = c(12, 6, 13, 8, 6, 8)

#create lettering report label based on tx*sampling Tukey HSD
trans.sum$label = c("a", "bc", "a", "d", "b", "cd") 

#make plot with labels
ggboxplot(ecophys, x = "sampling", y = "trans",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = "tx",
          linetype = c(1,2,1,2,1,2),
          short.panel.labs = FALSE,
          ylim = c(0, 15),
          legend = "none",
          ylab = expression(italic(E) ~textstyle("(mmol H ")[2]*O~m^{"-2"}~s^{"-1"}*textstyle(")")),
          xlab = "Sampling Point",
          panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())+
  geom_text(data = trans.sum, aes(y=yloc, label = label))
ggsave("Figures/trans.tiff", height = 4, width = 5, dpi=1200)

## Reality check ecophys relationships, not used in paper
# scatter plot of transpiration and anet by treatment 
# as expected, anet goes up so does transpiration, patterns same as boxplots
ggscatter(ecophys, x = "trans", y = "anet",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = c("tx"),
          add = "reg.line",
          short.panel.labs = FALSE,
          ylim = c(-10, 40),
          #legend = TRUE,
          ylab = expression(Anet~textstyle("(mmol H ")[2]*O~m^{"-2"}~s^{"-1"}*textstyle(")")),
          xlab = "Transpiration",
          #panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())

# scatter plot of transpiration and anet by sampling
# as expected, anet goes up so does transpiration, tx patterns same as boxplots
ggscatter(ecophys, x = "trans", y = "anet",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          facet.by = c("sampling"),
          #add = "reg.line",
          short.panel.labs = FALSE,
          ylim = c(-5, 30),
          #legend = TRUE,
          ylab = expression(Anet~textstyle("(mmol H ")[2]*O~m^{"-2"}~s^{"-1"}*textstyle(")")),
          xlab = "Transpiration",
          #panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())

# scatter plot of conductance and anet
# as expected, anet goes up so does conductance, tx patterns same as boxplots
ggscatter(ecophys, x = "cond", y = "anet",
          color = "tx",
          palette = c("grey30", "#DB4325","blue1"),
          #facet.by = c("tx", "sampling"),
          add = "reg.line",
          short.panel.labs = FALSE,
          ylim = c(0, 30),
          #legend = TRUE,
          ylab = expression(Anet~textstyle("(mmol H ")[2]*O~m^{"-2"}~s^{"-1"}*textstyle(")")),
          xlab = "Conductance",
          #panel.labs = list(tx=c("Ambient", "Drought", "Wet")),
          ggtheme = theme_pubr())
