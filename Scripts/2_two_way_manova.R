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
# install.packages("Hmisc")
# install.packages("corrplot")

##load packages
library(tidyverse)
library(mvnormtest)
library(vegan)
library(forecast)
library(rcompanion)
library(psych)
library(Hmisc)
library(corrplot)

#bring the data in
gh <- read.csv(file = "Data/prve_gh_master_data.csv")
gh$sampling <- as.factor(gh$sampling)#make sampling period a factor

#look at data
summary(gh)
glimpse(gh)

# rename variables for graphing
gh <- gh %>% rename(
  Total_Root_Length = totrootlnth,
  Root_Surface_Area = surfarea, 
  Average_Root_Diameter = avgdiam,
  Total_Root_Volume = rootvol,
  Root_Crossings = crossings,
  Root_Forks = forks,
  Root_Tips = tips,
  Maximum_Seedling_Height = maxht,
  True_Leaves = truelvs,
  Total_Leaflets = totlflts,
  Leaf_Length = lflnth,
  Lignin_Height = light,
  Cotyledonary_Node_Height = cnode,
  Thorns = thorns,
  Dry_Leaf_Mass = dryleaf,
  Leaf_Water_Content = lfwtr,
  Fresh_Weight = wetsdling,
  Dry_Seedling_Mass = drysdling,
  Seedling_Water_Content = sdlingwtr,
  Tap_Root_Length = taprootlnth,
  Coarse_Root_Diameter = coarserootdiam,
  Fine_Roots = fineroots,
  Total_Root_Mass = rootwt,
  Root_Shoot_Ratio = rootshoot,
  Specific_Leaf_Area = sla,
  Water_Potential = wp,
  Anet = anet,
  Conductance = cond,
  Zero_Fivecm_Soil_GWC = fivegwc,
  Five_Twentycm_Soil_GWC = twentygwc,
  Absolute_Growth_Rate = agr,
  Transpiration_Rate = trans
)

#create a matrix of only y-variables for manova
Yvar = as.matrix(cbind(gh[,c(4:35)]))

# Correlation plot of variables
corvar <- cor(Yvar, use = "complete.obs") # ignore NAs

corvar2 <- rcorr(as.matrix(Yvar), type = c("pearson")) # get Pearson's r

corvar2$r # pearson's r only
corvar2$P # sig correlations only

cor_small <- as.data.frame(corvar2$r) %>% 
  filter(. <= 0.90 & . >= -0.90) # pull out traits that are not highly correlated

cor_small_mat <- as.matrix(cor_small) # make a matrix for plotting

cor_small_sig <- as.data.frame(corvar2$P) %>% # pull out the insig correlated variables
  filter(. >= 0.05)

# correlation plot of all variables
corplot_all <- corrplot(corvar2$r, type = "upper", order = "hclust", 
                    tl.col = "black", tl.srt = 45)

# correlation plot of variables not highly correlated
corplot <- corrplot(cor_small_mat, is.corr = FALSE)

# insignificant correlations blank of all variables
corplot_sig <- corrplot(corvar2$r, type="upper", order="alphabet", 
         p.mat = corvar2$P, sig.level = 0.01, insig = "blank")


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

