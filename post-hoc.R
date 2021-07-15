##Two-way Manova##
##Greenhouse Mesquite Study Post-hoc Tukey
#Austin Rutherford
#email: arutherford@email.arizona.edu
#1/29/2020

##load packages
library(tidyverse)
library(agricolae)
library(emmeans)
library(lattice)
library(lme4)
library(multcomp)
library(multcompView)

#bring the data in
gh <- read.csv(file = "Data/prve_gh_master_data.csv")
gh$sampling <- as.factor(gh$sampling)#make sampling period a factor

#create a matrix of only y-variables for manova
Yvar = as.matrix(cbind(gh[,c(4:35)]))

#Manova with all response variables by tx and sampling and the tx-sampling interaction
fit <- manova(Yvar ~ tx + sampling + tx*sampling, data = gh)
summary(fit, test="Pillai")#manova

#ANOVAs
summary.aov(fit)#anovas

#create interaction term variable to force two-way interation term
int <- with(gh, interaction(tx, sampling))

#create list of variable names
dvList <- names(gh)[4:35]

#create model for each variable between tx and sampling
model <- lapply(dvList, function(x) {
  lm(substitute(i~int, list(i = as.name(x))), data = gh)})

#Tukey HSD for each variable for tx*sampling, letter report orders by mean, not tx
lapply(model, summary)
tuk.all<-lapply(model, function(model) HSD.test(aov(model),"int", group = TRUE,
                                                console = TRUE))

#Transpiration
m.trans<-aov(trans~int, data=gh)

means.trans <- emmeans(m.trans, "int")

tuk.trans<-cld(means.trans, Letter="abcde", sort=FALSE)
tuk.trans

#AGR
m.agr<-aov(agr~int, data=gh)

means.agr <- emmeans(m.agr, "int")

tuk.agr<-cld(means.agr, Letter="abcde", sort=FALSE)
tuk.agr

##Total Root Length
m.rootlnth<-aov(totrootlnth~int, data=gh)

means.rootlnth <- emmeans(m.rootlnth, "int")

tuk.totrootlnth<-cld(means.rootlnth, Letter="abcde", sort=FALSE)
tuk.totrootlnth

##Root Surface Area
m.surfarea<-aov(surfarea~int, data=gh)

means.surfarea <- emmeans(m.surfarea, "int")

tuk.surfarea<-cld(means.surfarea, Letter="abcde", sort=FALSE)
tuk.surfarea

##Root Avg Diam
m.avgdiam<-aov(avgdiam~int, data=gh)

means.avgdiam <- emmeans(m.avgdiam, "int")

tuk.avgdiam<-cld(means.avgdiam, Letter="abcde", sort=FALSE)
tuk.avgdiam

##Root Volume
m.rootvol<-aov(rootvol~int, data=gh)

means.rootvol <- emmeans(m.rootvol, "int")

tuk.rootvol<-cld(means.rootvol, Letter="abcde", sort=FALSE)
tuk.rootvol

##Root Crossings
m.crossings<-aov(crossings~int, data=gh)

means.crossings <- emmeans(m.crossings, "int")

tuk.crossings<-cld(means.crossings, Letter="abcde", sort=FALSE)
tuk.crossings

##Root Forks
m.forks<-aov(forks~int, data=gh)

means.forks <- emmeans(m.forks, "int")

tuk.forks<-cld(means.forks, Letter="abcde", sort=FALSE)
tuk.forks

##Root Tips
m.tips<-aov(tips~int, data=gh)

means.tips <- emmeans(m.tips, "int")

tuk.tips<-cld(means.tips, Letter="abcde", sort=FALSE)
tuk.tips

##Seedling Max Height
m.maxht<-aov(maxht~int, data=gh)

means.maxht <- emmeans(m.maxht, "int")

tuk.maxht<-cld(means.maxht, Letter="abcde", sort=FALSE)
tuk.maxht

##Number of True Leaves
m.truelvs<-aov(truelvs~int, data=gh)

means.truelvs <- emmeans(m.truelvs, "int")

tuk.truelvs<-cld(means.truelvs, Letter="abcde", sort=FALSE)
tuk.truelvs

##Number of Total Leaflets
m.totlflts<-aov(totlflts~int, data=gh)

means.totlflts <- emmeans(m.totlflts, "int")

tuk.totlflts<-cld(means.totlflts, Letter="abcde", sort=FALSE)
tuk.totlflts

##Leaf Length
m.lflnth<-aov(lflnth~int, data=gh)

means.lflnth <- emmeans(m.lflnth, "int")

tuk.lflnth<-cld(means.lflnth, Letter="abcde", sort=FALSE)
tuk.lflnth

##Lignin Height
m.light<-aov(light~int, data=gh)

means.light <- emmeans(m.light, "int")

tuk.light<-cld(means.light, Letter="abcde", sort=FALSE)
tuk.light

##C-node Height
m.cnode<-aov(cnode~int, data=gh)

means.cnode <- emmeans(m.cnode, "int")

tuk.cnode<-cld(means.cnode, Letter="abcde", sort=FALSE)
tuk.cnode

##Number of Thorns
m.thorns<-aov(thorns~int, data=gh)

means.thorns <- emmeans(m.thorns, "int")

tuk.thorns<-cld(means.thorns, Letter="abcde", sort=FALSE)
tuk.thorns

##Dry Leaf Weight
m.dryleaf<-aov(dryleaf~int, data=gh)

means.dryleaf <- emmeans(m.dryleaf, "int")

tuk.dryleaf<-cld(means.dryleaf, Letter="abcde", sort=FALSE)
tuk.dryleaf

##Leaf Water %
m.lfwtr<-aov(lfwtr~int, data=gh)

means.lfwtr <- emmeans(m.lfwtr, "int")

tuk.lfwtr<-cld(means.lfwtr, Letter="abcde", sort=FALSE)
tuk.lfwtr

##Wet Seedling Weight
m.wetsdling<-aov(wetsdling~int, data=gh)

means.wetsdling <- emmeans(m.wetsdling, "int")

tuk.wetsdling<-cld(means.wetsdling, Letter="abcde", sort=FALSE)
tuk.wetsdling

##Dry Seedling Weight
m.drysdling<-aov(drysdling~int, data=gh)

means.drysdling <- emmeans(m.drysdling, "int")

tuk.drysdling<-cld(means.drysdling, Letter="abcde", sort=FALSE)
tuk.drysdling

##Seedling Water %
m.sdlingwtr<-aov(sdlingwtr~int, data=gh)

means.sdlingwtr <- emmeans(m.sdlingwtr, "int")

tuk.sdlingwtr<-cld(means.sdlingwtr, Letter="abcde", sort=FALSE)
tuk.sdlingwtr

##Tap Root Length
m.taprootlnth<-aov(taprootlnth~int, data=gh)

means.taprootlnth <- emmeans(m.taprootlnth, "int")

tuk.taprootlnth<-cld(means.taprootlnth, Letter="abcde", sort=FALSE)
tuk.taprootlnth

##Coarse Root Diameter
m.coarserootdiam<-aov(coarserootdiam~int, data=gh)

means.coarserootdiam <- emmeans(m.coarserootdiam, "int")

tuk.coarserootdiam<-cld(means.coarserootdiam, Letter="abcde", sort=FALSE)
tuk.coarserootdiam

##Number of Fine Roots
m.fineroots<-aov(fineroots~int, data=gh)

means.fineroots <- emmeans(m.fineroots, "int")

tuk.fineroots<-cld(means.fineroots, Letter="abcdefg", sort=FALSE)
tuk.fineroots

##Root Weight
m.rootwt<-aov(rootwt~int, data=gh)

means.rootwt <- emmeans(m.rootwt, "int")

tuk.rootwt<-cld(means.rootwt, Letter="abcdefg", sort=FALSE)
tuk.rootwt

##Root:Shoot
m.rootshoot<-aov(rootshoot~int, data=gh)

means.rootshoot <- emmeans(m.rootshoot, "int")

tuk.rootshoot<-cld(means.rootshoot, Letter="abcdefg", sort=FALSE)
tuk.rootshoot

##Specific Leaf Area
m.sla<-aov(sla~int, data=gh)

means.sla <- emmeans(m.sla, "int")

tuk.sla<-cld(means.sla, Letter="abcdefg", sort=FALSE)
tuk.sla

##Water Potential
m.wp<-aov(wp~int, data=gh)

means.wp <- emmeans(m.wp, "int")

tuk.wp<-cld(means.wp, Letter="abcdefg", sort=FALSE)
tuk.wp

##Anet
m.anet<-aov(anet~int, data=gh)

means.anet <- emmeans(m.anet, "int")

tuk.anet<-cld(means.anet, Letter="abcdefg", sort=FALSE)
tuk.anet

##Conductance
m.cond<-aov(cond~int, data=gh)

means.cond <- emmeans(m.cond, "int")

tuk.cond<-cld(means.cond, Letter="abcdefg", sort=FALSE)
tuk.cond

##0-5cm GWC
m.fivegwc<-aov(fivegwc~int, data=gh)

means.fivegwc <- emmeans(m.fivegwc, "int")

tuk.fivegwc<-cld(means.fivegwc, Letter="abcdefg", sort=FALSE)
tuk.fivegwc

##5-20cm GWC
m.twentygwc<-aov(twentygwc~int, data=gh)

means.twentygwc <- emmeans(m.twentygwc, "int")

tuk.twentygwc<-cld(means.twentygwc, Letter="abcdefg", sort=FALSE)
tuk.twentygwc

###Below is using the model function (not emmeans) to do calcs and then extract letter reports (ordered by mean)
#extract tx*sampling means and lettering reports
group.trans <- tuk.all[[29]]$groups
group.agr <- tuk.all[[32]]$groups
group.totrootlnth <- tuk.all[[1]]$groups
group.surfarea <- tuk.all[[2]]$groups
group.avgdiam <- tuk.all[[3]]$groups
group.rootvol <- tuk.all[[4]]$groups
group.crossings <- tuk.all[[5]]$groups
group.forks <- tuk.all[[6]]$groups
group.tips <- tuk.all[[7]]$groups
group.maxht <- tuk.all[[8]]$groups
group.truelvs <- tuk.all[[9]]$groups
group.totlflts <- tuk.all[[10]]$groups
group.lflnth <- tuk.all[[11]]$groups
group.light <- tuk.all[[12]]$groups
group.cnode <- tuk.all[[13]]$groups
group.thorns <- tuk.all[[14]]$groups
group.dryleaf <- tuk.all[[15]]$groups
group.lfwtr <- tuk.all[[16]]$groups
group.wetsdling <- tuk.all[[17]]$groups
group.drysdling <- tuk.all[[18]]$groups
group.sdlingwtr <- tuk.all[[19]]$groups
group.taprootlnth <- tuk.all[[20]]$groups
group.coarserootdiam <- tuk.all[[21]]$groups
group.fineroots <- tuk.all[[22]]$groups
group.rootwt <- tuk.all[[23]]$groups
group.rootshoot <- tuk.all[[24]]$groups
group.sla <- tuk.all[[25]]$groups
group.wp <- tuk.all[[26]]$groups
group.anet <- tuk.all[[27]]$groups
group.cond <- tuk.all[[28]]$groups
group.fivegwc <- tuk.all[[30]]$groups
group.twentygwc <- tuk.all[[31]]$groups

sink(file = "Output/gh_stats_tukey.txt")#create txt file

group.totrootlnth
group.surfarea
group.avgdiam
group.rootvol
group.crossings
group.forks
group.tips
group.maxht
group.truelvs
group.totlflts
group.lflnth
group.light
group.cnode
group.thorns
group.dryleaf
group.lfwtr
group.wetsdling
group.drysdling
group.sdlingwtr
group.taprootlnth
group.coarserootdiam
group.fineroots
group.rootwt
group.rootshoot
group.sla
group.wp
group.anet
group.cond
group.trans
group.fivegwc
group.twentygwc
group.agr

sink()#write to .txt file
closeAllConnections()# sends code output back to console
