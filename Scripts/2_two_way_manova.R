##Two-way MANOVA/ANOVA##
##Greenhouse Mesquite Study
#Austin Rutherford
#email: arutherford@email.arizona.edu
#1/21/2020

##install packages if needed, commented out since there are already installed
# install.packages("tidyverse")
# install.packages("mvnormtest")
# install.packages("vegan")
# install.packages("forecast")
# install.packages("rcompanion")
# install.packages("psych")

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
# sink(file = "Output/gh_stats.txt")#create txt file
# summary(fit, test="Pillai")#get manova summary
# summary.aov(object = fit)#get anovas
# sink()#write to .txt file
# 
# closeAllConnections()# sends code output back to console

##Anova with root volume response variable
# Pull out root variables
roots_var <- gh %>% 
  dplyr::select(tx, sampling, surfarea, avgdiam, rootvol, taprootlnth,
                coarserootdiam, fineroots, rootwt)

# standardize for comparisons
roots_norm <- as.data.frame(scale(roots_var[,3:9]))

# bring back tx and sampling
roots_norm <- cbind(roots_norm, roots_var[,1:2])

# look at descriptive stats
psych::describeBy(roots_norm, list(roots_norm$tx, roots_norm$sampling))

# run anova
summary(aov(rootvol ~ sampling*tx + taprootlnth + avgdiam, data = roots_norm))

