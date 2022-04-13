##Composite variable-Performance vs functional##
##Greenhouse Mesquite Study
#Austin Rutherford
#email: arutherford@email.arizona.edu
#2022-04-05

##install packages if needed, commented out since there are already installed
# install.packages("tidyverse")
# install.packages("caret")
# install.packages("MASS")
# install.packages("ggpubr")
# install.packages("multicon")
# install.packages("glmmTMB")
# install.packages("MuMIn")
# install.packages("bbmle")

#load packages
library(tidyverse)
library(caret)
library(MASS)
library(ggpubr)
library(multicon)
library(glmmTMB)
library(MuMIn)
library(bbmle)

#set graphing/plotting theme
theme_set(theme_classic())

# Load the data without pot# or exploratory PCAs
gh <- read.csv(file = "Data/prve_gh_master_data.csv", 
               colClasses=c("NULL",NA, NA,NA,NA,NA,
                            NA,NA,NA,NA,NA,NA,NA,
                            NA,NA,NA,NA,NA,NA,NA,
                            NA,NA,NA,NA,NA,NA,NA,
                            NA,NA,NA,NA,NA,NA,NA,NA,NA))

glimpse(gh)


# composite performance variable for sampling 1 (Day 11)
perf_1 <- gh %>% filter(sampling == 1) %>% dplyr::select(agr, dryleaf,drysdling,wetsdling,rootshoot,rootwt)
perf.comp.1 <- as.data.frame(composite(perf_1, Zitems = T, rel = F))
perf.comp.1 <- perf.comp.1 %>% rename(perf.comp = "composite(perf_1, Zitems = T, rel = F)")
hist(perf.comp.1)

# composite performance variable for sampling 2 (Day 22)
perf_2 <- gh %>% filter(sampling == 2) %>% dplyr::select(agr, dryleaf,drysdling,wetsdling,rootshoot,rootwt)
perf.comp.2 <- as.data.frame(composite(perf_2, Zitems = T, rel = F))
perf.comp.2 <- perf.comp.2 %>% rename(perf.comp = "composite(perf_2, Zitems = T, rel = F)")
hist(perf.comp.2)

#combine sampling periods
perf.comp <- rbind(perf.comp.1, perf.comp.2)

# perf variable with full data set
gh_full <- cbind(gh, perf.comp)

hist((gh_full$perf.comp))

gh_perffunc <- gh_full %>% dplyr::select(-c(agr, dryleaf,drysdling,wetsdling,rootshoot,rootwt)) %>% # remove perf variables from full
  mutate(tx = as.factor(tx), sampling = as.factor(sampling)) %>% 
  mutate(loc = as.factor(loc))

glimpse(gh_perffunc)

#stepwise procedure
step <- train(perf.comp ~ anet + totrootlnth + surfarea +avgdiam +rootvol +crossings+
                forks +tips+ maxht+ truelvs+ totlflts+ lflnth +light+ cnode+ thorns+
                lfwtr+ sdlingwtr +coarserootdiam +fineroots+sla+ wp+cond+  trans+
                fivegwc+ twentygwc, data=gh_perffunc,
              preProcess= c("center", "scale"),
              method = "lmStepAIC",
              trControl = trainControl(method="repeatedcv", repeats = 10),
              na.action = 'na.omit')

step$finalModel

mod<-lm(perf.comp~surfarea+rootvol+crossings+truelvs+lflnth+sla+cond+trans+fivegwc, data = gh_perffunc, na.action = 'na.omit')
summary(mod)

# individual models for functional traits

gh_scale<-gh_perffunc %>% 
  group_by(tx, sampling) %>%
  mutate(across(c(anet,totrootlnth, taprootlnth, surfarea,avgdiam,rootvol,crossings,
                    forks,tips, maxht,truelvs,totlflts,lflnth,light, cnode, thorns,
                    lfwtr, sdlingwtr,coarserootdiam,fineroots,sla, wp,cond,  trans,
                    fivegwc, twentygwc), scales::rescale)) %>% ungroup()

# lm

totrootlnth_mod <- lm(perf.comp ~ totrootlnth + totrootlnth*sampling*tx, data = gh_scale)
tot<-summary(totrootlnth_mod)
tot$r.squared
tot$adj.r.squared
tot$coefficients[2,4]

surfarea_mod <- lm(perf.comp ~ surfarea + surfarea*sampling*tx, data = gh_scale)
sur<-summary(surfarea_mod)
sur$r.squared
sur$adj.r.squared
sur$coefficients[2,4]

avgdiam_mod <- lm(perf.comp ~ avgdiam + avgdiam*sampling*tx, data = gh_scale)
avgdi<-summary(avgdiam_mod)
avgdi$r.squared
avgdi$adj.r.squared
avgdi$coefficients[2,4]

rootvol_mod <- lm(perf.comp ~ rootvol + rootvol*sampling*tx, data = gh_scale)
rootvol<-summary(rootvol_mod)
rootvol$r.squared
rootvol$adj.r.squared
rootvol$coefficients[2,4]

crossings_mod <- lm(perf.comp ~ crossings + crossings*sampling*tx, data = gh_scale)
cross<-summary(crossings_mod)
cross$r.squared
cross$adj.r.squared
cross$coefficients[2,4]

forks_mod <- lm(perf.comp ~ forks + forks*sampling*tx, data = gh_scale)
forks<-summary(forks_mod)
forks$r.squared
forks$adj.r.squared
forks$coefficients[2,4]

tips_mod <- lm(perf.comp ~ tips + tips*sampling*tx, data = gh_scale)
tips<-summary(tips_mod)
tips$r.squared
tips$adj.r.squared
tips$coefficients[2,4]

maxht_mod <- lm(perf.comp ~ maxht + maxht*sampling*tx, data = gh_scale)
maxht<-summary(maxht_mod)
maxht$r.squared
maxht$adj.r.squared
maxht$coefficients[2,4]

truelvs_mod <- lm(perf.comp ~ truelvs + truelvs*sampling*tx, data = gh_scale)
truelvs<-summary(truelvs_mod)
truelvs$r.squared
truelvs$adj.r.squared
truelvs$coefficients[2,4]

totlflts_mod <- lm(perf.comp ~ totlflts + totlflts*sampling*tx, data = gh_scale)
totlflts<-summary(totlflts_mod)
totlflts$r.squared
totlflts$adj.r.squared
totlflts$coefficients[2,4]

lflnth_mod <- lm(perf.comp ~ lflnth + lflnth*sampling*tx, data = gh_scale)
lflnth<-summary(lflnth_mod)
lflnth$r.squared
lflnth$adj.r.squared
lflnth$coefficients[2,4]

light_mod <- lm(perf.comp ~ light + light*sampling*tx, data = gh_scale)
light<-summary(light_mod)
light$r.squared
light$adj.r.squared
light$coefficients[2,4]

cnode_mod <- lm(perf.comp ~ cnode + cnode*sampling*tx, data = gh_scale)
cnode<-summary(cnode_mod)
cnode$r.squared
cnode$adj.r.squared
cnode$coefficients[2,4]

thorns_mod <- lm(perf.comp ~ thorns + thorns*sampling*tx, data = gh_scale)
thorns<-summary(thorns_mod)
thorns$r.squared
thorns$adj.r.squared
thorns$coefficients[2,4]

lfwtr_mod <- lm(perf.comp ~ lfwtr + lfwtr*sampling*tx, data = gh_scale)
lfwtr<-summary(lfwtr_mod)
lfwtr$r.squared
lfwtr$adj.r.squared
lfwtr$coefficients[2,4]

sdlingwtr_mod <- lm(perf.comp ~ sdlingwtr + sdlingwtr*sampling*tx, data = gh_scale)
sdlingwtr<-summary(sdlingwtr_mod)
sdlingwtr$r.squared
sdlingwtr$adj.r.squared
sdlingwtr$coefficients[2,4]

taprootlnth_mod <- lm(perf.comp ~ taprootlnth + taprootlnth*sampling*tx, data = gh_scale)
taprootlnth<-summary(taprootlnth_mod)
taprootlnth$r.squared
taprootlnth$adj.r.squared
taprootlnth$coefficients[2,4]

coarserootdiam_mod <- lm(perf.comp ~ coarserootdiam + coarserootdiam*sampling*tx, data = gh_scale)
coarserootdiam<-summary(coarserootdiam_mod)
coarserootdiam$r.squared
coarserootdiam$adj.r.squared
coarserootdiam$coefficients[2,4]

fineroots_mod <- lm(perf.comp ~ fineroots + fineroots*sampling*tx, data = gh_scale)
fineroots<-summary(fineroots_mod)
fineroots$r.squared
fineroots$adj.r.squared
fineroots$coefficients[2,4]

sla_mod <- lm(perf.comp ~ sla + sla*sampling + sla*tx + tx*sampling, data = gh_scale)
sla<-summary(sla_mod)
sla$r.squared
sla$adj.r.squared
sla$coefficients[2,4]

wp_mod <- lm(perf.comp ~ wp + wp*sampling*tx, data = gh_scale)
wp<-summary(wp_mod)
wp$r.squared
wp$adj.r.squared
wp$coefficients[2,4]

anet_mod <- lm(perf.comp ~ anet + anet*sampling*tx, data = gh_scale)
anet<-summary(anet_mod)
anet$r.squared
anet$adj.r.squared
anet$coefficients[2,4]

cond_mod <- lm(perf.comp ~ cond + cond*sampling*tx, data = gh_scale)
cond<-summary(cond_mod)
cond$r.squared
cond$adj.r.squared
cond$coefficients[2,4]

trans_mod <- lm(perf.comp ~ trans + trans*sampling*tx, data = gh_scale)
trans<-summary(trans_mod)
trans$r.squared
trans$adj.r.squared
trans$coefficients[2,4]

fivegwc_mod <- lm(perf.comp ~ fivegwc + fivegwc*sampling*tx, data = gh_scale)
fivegwc<-summary(fivegwc_mod)
fivegwc$r.squared
fivegwc$adj.r.squared
fivegwc$coefficients[2,4]

twentygwc_mod <- lm(perf.comp ~ twentygwc + twentygwc*sampling*tx, data = gh_scale)
twentygwc<-summary(twentygwc_mod)
twentygwc$r.squared
twentygwc$adj.r.squared
twentygwc$coefficients[2,4]

sink(file = "Output/gh_stats_regression.txt")#create txt file
# 
tot$adj.r.squared
sur$adj.r.squared
avgdi$adj.r.squared
rootvol$adj.r.squared
cross$adj.r.squared
forks$adj.r.squared
tips$adj.r.squared
maxht$adj.r.squared
truelvs$adj.r.squared
totlflts$adj.r.squared
lflnth$adj.r.squared
light$adj.r.squared
cnode$adj.r.squared
thorns$adj.r.squared
lfwtr$adj.r.squared
sdlingwtr$adj.r.squared
taprootlnth$adj.r.squared
coarserootdiam$adj.r.squared
fineroots$adj.r.squared
sla$adj.r.squared
wp$adj.r.squared
anet$adj.r.squared
cond$adj.r.squared
trans$adj.r.squared
fivegwc$adj.r.squared
twentygwc$adj.r.squared
# 
sink()#write to .txt file





