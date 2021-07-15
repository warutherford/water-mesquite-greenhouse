##Two-way MANOVA/ANOVA##
##Greenhouse Mesquite Study
#Austin Rutherford
#email: arutherford@email.arizona.edu
#1/21/2020

##load packages
library(tidyverse)
library(mvnormtest)
library(vegan)
library(forecast)
library(rcompanion)
library(psych)

#bring the data in
gh <- read.csv(file = "Data/prve_gh_master_data.csv")
gh$sampling <- as.factor(gh$sampling)#make sampling period a factor

#look at data
summary(gh)
glimpse(gh)

#create a matrix of only y-variables for manova
Yvar = as.matrix(cbind(gh[,c(4:35)]))

####Multivariate Normality test##
norm <- t(gh[1:60,4:35])#transpose matrix
mshapiro.test(na.omit(norm))#Fails normality (P<0.05), cant run with missing data/NAs
####

##Non-para Manova
npfit <- adonis2(Yvar ~ tx + sampling + tx*sampling, method= "jaccard", data = gh, na.rm=T)
npfit2 <- adonis(Yvar ~ tx + sampling + tx*sampling, method= "jaccard", data = gh, na.rm=T)

##Manova with all response variables by tx and sampling and the tx-sampling interaction
fit <- manova(Yvar ~ tx + sampling + tx*sampling, data = gh)
summary(fit, test="Pillai")#manova
summary.aov(fit)#anovas

#variable normality
for (i in 4:35){print(shapiro.test(gh[,i]))}

for (i in 4:35){print(BoxCox(gh[,i], lambda = "auto"))}

#Save stat outputs to text file#
sink(file = "Output/gh_stats.txt")#create txt file
summary(fit, test="Pillai")#get manova summary
summary.aov(object = fit)#get anovas
sink()#write to .txt file

closeAllConnections()# sends code output back to console

##Anova and Manova with root response variables tests
roots_var <- gh %>% 
  select(tx, sampling, surfarea, avgdiam, rootvol, taprootlnth, coarserootdiam, fineroots, rootwt)

roots_norm <- as.data.frame(scale(roots_var[,3:9]))

roots_norm <- cbind(roots_norm, roots_var[,1:2])

roots_norm <- roots_norm %>% 
  mutate(sa_to_v = surfarea/rootvol,
         len_diam = taprootlnth/avgdiam)

reg <- lm(avgdiam ~ taprootlnth, data = roots_norm)
summary(reg)
coefficients(reg)

reg2 <- lm(rootvol ~ taprootlnth, data = roots_norm)
summary(reg2)
coefficients(reg2)

reg3 <- lm(rootvol ~ avgdiam, data = roots_norm)
summary(reg3)
coefficients(reg3)

plot(y = roots_norm$rootvol, x = roots_norm$avgdiam)
abline(reg3, col = "blue")

plot(y = roots_norm$rootvol, x = roots_norm$taprootlnth)
abline(reg2, col = "blue")

plot(y = roots_norm$avgdiam, x = roots_norm$taprootlnth)
abline(reg, col = "blue")

psych::describeBy(roots_norm, list(roots_norm$tx, roots_norm$sampling))

summary(aov(rootvol ~ sampling*tx + taprootlnth, data = roots_norm))
summary(aov(rootvol ~ sampling*tx + avgdiam, data = roots_norm))
summary(aov(rootvol ~ sampling*tx + taprootlnth + avgdiam + surfarea, data = roots_norm))
summary(aov(rootvol ~ sampling*tx + taprootlnth + avgdiam, data = roots_norm))

fit_roots <- manova(as.matrix(roots_norm[,c(2,4)]) ~ tx + sampling + tx*sampling, data = gh)
summary(fit_roots, test="Pillai")#manova
summary.aov(fit_roots)#anovas
