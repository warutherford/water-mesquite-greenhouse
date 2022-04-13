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
# install.packages("ggcorrplot")

##load packages
library(tidyverse)
library(mvnormtest)
library(vegan)
library(forecast)
library(rcompanion)
library(psych)
library(Hmisc)
library(corrplot)
library(ggcorrplot)

#bring the data in
gh <- read.csv(file = "Data/prve_gh_master_data.csv")
gh$sampling <- as.factor(gh$sampling)#make sampling period a factor

#look at data
summary(gh)
glimpse(gh)

#create a matrix of only y-variables for manova
Yvar = as.matrix(cbind(gh[,c(5:36)]))

# Correlation plots of variables, data frame for each sampling and then all variables, normalized data
# sampling 1
covar_samp1 <- gh %>% filter(sampling == 1) %>% #dplyr::select(-light) %>%  # lig ht missing for 1st sample
  mutate(across(c(anet,totrootlnth, taprootlnth, surfarea,avgdiam,rootvol,crossings,
                  forks,tips, maxht,truelvs,totlflts,lflnth, cnode, thorns,
                  lfwtr, sdlingwtr,coarserootdiam,fineroots,sla, wp,cond,  trans,
                  fivegwc, twentygwc), scales::rescale)) %>% 
  rename(
    "Total Root Length" = totrootlnth,
    "Root Surface Area" = surfarea, 
    "Average Root Diameter" = avgdiam,
    "Total Root Volume" = rootvol,
    "Root Crossings" = crossings,
    "Root Forks" = forks,
    "Root Tips" = tips,
    "Max Seedling Height" = maxht,
    "True Leaves" = truelvs,
    "Total Leaflets" = totlflts,
    "Leaf Length" = lflnth,
    "Lignin Height" = light,
    "Cotyledonary Node Height" = cnode,
    "Thorns" = thorns,
    "Dried Leaf Mass" = dryleaf,
    "Leaf Water Content" = lfwtr,
    "Fresh Weight" = wetsdling,
    "Dried Seedling Mass" = drysdling,
    "Seedling Water Content" = sdlingwtr,
    "Tap Root Length" = taprootlnth,
    "Coarse Root Diameter" = coarserootdiam,
    "Fine Roots" = fineroots,
    "Total Root Mass" = rootwt,
    "Root:Shoot" = rootshoot,
    "Specific Leaf Area" = sla,
    "Water Potential" = wp,
    "Anet" = anet,
    "Conductance" = cond,
    "0-5 cm Soil GWC" = fivegwc,
    "5-20 cm Soil GWC" = twentygwc,
    "Absolute Growth Rate" = agr,
    "Transpiration Rate" = trans
  ) %>% 
  ungroup() %>% 
  dplyr::select(sort(tidyselect::peek_vars())) %>% 
  dplyr::select(pot, tx, sampling, loc, "Absolute Growth Rate", "Dried Leaf Mass",
                "Dried Seedling Mass", "Fresh Weight", "Root:Shoot", "Total Root Mass", 
                everything())

# sampling 2    
covar_samp2 <- gh %>% filter(sampling == 2) %>% 
  mutate(across(c(anet,totrootlnth, taprootlnth, surfarea,avgdiam,rootvol,crossings,
                  forks,tips, maxht,truelvs,totlflts,lflnth,light, cnode, thorns,
                  lfwtr, sdlingwtr,coarserootdiam,fineroots,sla, wp,cond,  trans,
                  fivegwc, twentygwc), scales::rescale)) %>% 
  rename(
    "Total Root Length" = totrootlnth,
    "Root Surface Area" = surfarea, 
    "Average Root Diameter" = avgdiam,
    "Total Root Volume" = rootvol,
    "Root Crossings" = crossings,
    "Root Forks" = forks,
    "Root Tips" = tips,
    "Max Seedling Height" = maxht,
    "True Leaves" = truelvs,
    "Total Leaflets" = totlflts,
    "Leaf Length" = lflnth,
    "Lignin Height" = light,
    "Cotyledonary Node Height" = cnode,
    "Thorns" = thorns,
    "Dried Leaf Mass" = dryleaf,
    "Leaf Water Content" = lfwtr,
    "Fresh Weight" = wetsdling,
    "Dried Seedling Mass" = drysdling,
    "Seedling Water Content" = sdlingwtr,
    "Tap Root Length" = taprootlnth,
    "Coarse Root Diameter" = coarserootdiam,
    "Fine Roots" = fineroots,
    "Total Root Mass" = rootwt,
    "Root:Shoot" = rootshoot,
    "Specific Leaf Area" = sla,
    "Water Potential" = wp,
    "Anet" = anet,
    "Conductance" = cond,
    "0-5 cm Soil GWC" = fivegwc,
    "5-20 cm Soil GWC" = twentygwc,
    "Absolute Growth Rate" = agr,
    "Transpiration Rate" = trans
  ) %>% 
  ungroup() %>% 
  dplyr::select(sort(tidyselect::peek_vars())) %>% 
  dplyr::select(pot, tx, sampling, loc, "Absolute Growth Rate", "Dried Leaf Mass",
                "Dried Seedling Mass", "Fresh Weight", "Root:Shoot", "Total Root Mass", 
                everything())

# all variables
gh_scale <- gh %>%
  mutate(across(c(anet,totrootlnth, taprootlnth, surfarea,avgdiam,rootvol,crossings,
                  forks,tips, maxht,truelvs,totlflts,lflnth,light, cnode, thorns,
                  lfwtr, sdlingwtr,coarserootdiam,fineroots,sla, wp,cond,  trans,
                  fivegwc, twentygwc), scales::rescale)) %>% 
  rename(
    "Total Root Length" = totrootlnth,
    "Root Surface Area" = surfarea, 
    "Average Root Diameter" = avgdiam,
    "Total Root Volume" = rootvol,
    "Root Crossings" = crossings,
    "Root Forks" = forks,
    "Root Tips" = tips,
    "Max Seedling Height" = maxht,
    "True Leaves" = truelvs,
    "Total Leaflets" = totlflts,
    "Leaf Length" = lflnth,
    "Lignin Height" = light,
    "Cotyledonary Node Height" = cnode,
    "Thorns" = thorns,
    "Dried Leaf Mass" = dryleaf,
    "Leaf Water Content" = lfwtr,
    "Fresh Weight" = wetsdling,
    "Dried Seedling Mass" = drysdling,
    "Seedling Water Content" = sdlingwtr,
    "Tap Root Length" = taprootlnth,
    "Coarse Root Diameter" = coarserootdiam,
    "Fine Roots" = fineroots,
    "Total Root Mass" = rootwt,
    "Root:Shoot" = rootshoot,
    "Specific Leaf Area" = sla,
    "Water Potential" = wp,
    "Anet" = anet,
    "Conductance" = cond,
    "0-5 cm Soil GWC" = fivegwc,
    "5-20 cm Soil GWC" = twentygwc,
    "Absolute Growth Rate" = agr,
    "Transpiration Rate" = trans
  ) %>% 
  ungroup() %>% 
  dplyr::select(sort(tidyselect::peek_vars())) %>% 
  dplyr::select(pot, tx, sampling, loc, "Absolute Growth Rate", "Dried Leaf Mass",
                "Dried Seedling Mass", "Fresh Weight", "Root:Shoot", "Total Root Mass", 
                everything())

# break it out by PPTx for interest
gh_drought <- gh_scale %>% filter(tx=="Drought")

gh_ambient <- gh_scale %>% filter(tx=="Ambient")

gh_wet <- gh_scale %>% filter(tx=="Wet")

## Examine variables that aren't highly correlated
# corvar2 <- corr.test(as.matrix(Yvar), adjust = "holm") # get Pearson's r
# 
# M<-corvar2$r # pearson's r only
# p_mat<-corvar2$P # sig correlations only
# 
# cor_small <- as.data.frame(corvar2$r) %>% 
#   filter(. <= 0.90) %>%  # pull out traits that are not highly correlated
#   filter(. >= -0.90)
#   
# cor_small_mat <- as.matrix(cor_small) # make a matrix for plotting
# 
# # correlation plot of variables not highly correlated
# ggcorrplot(cor_small_mat, method = "square", type = "upper", ggtheme = theme_bw,
#                             lab = TRUE, lab_size = 3, hc.order = F, outline.color = "black",
#                             title = "Day 22", tl.cex = 18, pch.cex = 5, digits = 2, legend.title = "Pearson Corr")

# corr tests by sampling and precip treatment

c_test_1 <- corr.test(covar_samp1[,c(5:10)], covar_samp1[,c(11:36)], adjust = "holm")

c_test_2 <- corr.test(covar_samp2[,c(5:10)], covar_samp2[,c(11:36)], adjust = "holm")

c_test_all <- corr.test(gh_scale[,c(5:10)], gh_scale[,c(11:36)], adjust = "holm")

c_test_dr <- corr.test(gh_drought[,c(5:10)], gh_drought[,c(11:36)], adjust = "holm")

c_test_am <- corr.test(gh_ambient[,c(5:10)], gh_ambient[,c(11:36)], adjust = "holm")

c_test_wt <- corr.test(gh_wet[,c(5:10)], gh_wet[,c(11:36)], adjust = "holm")


day_11_cor<- ggcorrplot(c_test_1$r, method = "square", type = "full", ggtheme = theme_bw(), sig.level = 0.05,
           p.mat = c_test_1$p.adj, insig = "blank", lab = TRUE, lab_size = 12, hc.order = F, outline.color = "black",
           tl.cex = 54, pch.cex = 5, digits = 2, legend.title = "Pearson Corr")

# ggsave(filename = "Figures/day-ll-corr.tiff",
#        plot = day_11_cor,
#        dpi = 800,
#        width = 15,
#        height = 30,
#        scale = 3,
#        units = "cm",
#        compression = "lzw")


day_22_cor <- ggcorrplot(c_test_2$r, method = "square", type = "full", ggtheme = theme_bw, sig.level = 0.05,
           p.mat = c_test_2$p.adj, insig = "blank", lab = TRUE, lab_size = 12, hc.order = F, outline.color = "black",
            tl.cex = 54, pch.cex = 5, digits = 2, legend.title = "Pearson Corr")

# ggsave(filename = "Figures/day-22-corr.tiff",
#        plot = day_22_cor,
#        dpi = 800,
#        width = 15,
#        height = 30,
#        scale = 3,
#        units = "cm",
#        compression = "lzw")


# below are correlation plots for all the data together, ambient, wet, and dry treatments respectively
ggcorrplot(c_test_all$r, method = "square", type = "full", ggtheme = theme_bw, sig.level = 0.05,
           p.mat = c_test_all$p.adj, insig = "blank", lab = TRUE, lab_size = 3, hc.order = F, outline.color = "black")

ggcorrplot(c_test_am$r, method = "square", type = "full", ggtheme = theme_bw, sig.level = 0.05,
           p.mat = c_test_am$p.adj, insig = "blank", lab = TRUE, lab_size = 3, hc.order = F, outline.color = "black",
           title = "Ambient")

ggcorrplot(c_test_wt$r, method = "square", type = "full", ggtheme = theme_bw, sig.level = 0.05,
           p.mat = c_test_wt$p.adj, insig = "blank", lab = TRUE, lab_size = 3, hc.order = F, outline.color = "black",
           title = "Wet")

ggcorrplot(c_test_dr$r, method = "square", type = "full", ggtheme = theme_bw, sig.level = 0.05,
           p.mat = c_test_dr$p.adj, insig = "blank", lab = TRUE, lab_size = 3, hc.order = F, outline.color = "black",
           title = "Dry")

# Correlagram test of wrapping by a factor - test
# for reproducibility
# set.seed(123)
# 
# ## plot
# t<-gh_scale %>% filter(sampling == 1) %>% 
#   grouped_ggcorrmat(
#   ## arguments relevant for `ggcorrmat`
#   cor.vars = Total_Root_Length:Absolute_Growth_Rate,
#   #data = gh_scale,
#   type = "parametric", ## pearson corr
#   grouping.var = tx,
#   ## arguments relevant for `combine_plots`
#   plotgrid.args = list(nrow = 3),
#   annotation.args = list(
#     tag_levels = "a",
#     title = "",
#     caption = ""
#   ),
#   output = "table"
# )

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

