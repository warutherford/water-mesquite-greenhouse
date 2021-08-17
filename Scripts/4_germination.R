#Germination Data Summary
##Greenhouse Mesquite Study
#Austin Rutherford
#email: arutherford@email.arizona.edu
#4/17/2020

##load packages
library(tidyverse)
library(agricolae)
library(psych)

#read in the data
germ <- read.csv(file = "Data/germination.csv")
germ$sampling <- as.factor(germ$sampling)#make sampling period a factor
germ$tx <- as.factor(germ$tx)#make sampling period a factor

#descriptive stats by treatment
describeBy(germ, germ$tx)

#create a anova for germination and treatment
germ.model<-aov(seedlings ~ tx, data = germ)
summary(germ.model)

#post hoc Tukey's for treatment
germ.tuk<-HSD.test(germ.model,"tx", console = TRUE)

#isolate treatment means and Tukey lettering report
groups.tuk<-germ.tuk$groups

#calculate percent germination for each treatment
groups.tuk$percent <- 100*(groups.tuk$seedlings/3)
