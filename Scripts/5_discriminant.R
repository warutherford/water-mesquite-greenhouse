##Discriminant Analysis##
##Greenhouse Mesquite Study
#Austin Rutherford
#email: arutherford@email.arizona.edu
#1/27/2020

##install packages if needed, commented out since there are already installed
# install.packages("tidyverse")
# install.packages("caret")
# install.packages("MASS")
# install.packages("ggpubr")

#load packages
library(tidyverse)
library(caret)
library(MASS)
library(ggpubr)

#set graphing/plotting theme
theme_set(theme_classic())

# Load the data without pot# or exploratory PCAs
gh <- read.csv(file = "Data/prve_gh_master_data.csv", 
               colClasses=c("NULL", NA,NA,NA,NA,
                            NA,NA,NA,NA,NA,NA,NA,
                            NA,NA,NA,NA,NA,NA,NA,
                            NA,NA,NA,NA,NA,NA,NA,
                            NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))

# Omit NAs
gh_na <- na.omit(gh)

#####All Samples LDA Analysis####
# Remove variables with no variance and filter by sampling period 1
gh_all<- gh_na %>% 
  dplyr::select(-loc,-sampling)

# Create random set of data
set.seed(5)

# Make training data 
training.samples <- gh_all$tx %>%
  createDataPartition(p = 0.8, list = FALSE)

# Split training (80%) and test set (20%)
train.data <- gh_all[training.samples, ]
test.data <- gh_all[-training.samples, ]

# Estimate preprocessing parameters, normalize data
preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)

# Fit the model, Linear Disc Analysis
model_all <- lda(tx~., data = train.transformed)
model_all

# model_small <- lda(tx~fineroots+crossings+forks+totrootlnth+surfarea+maxht+lflnth+rootvol+tips+taprootlnth, data = train.transformed)
# model_small

#extract loadings
lda.coef <- as.data.frame(coefficients(model_all))
lda.coef.tbl <- rownames_to_column(lda.coef,"measurement")

lda.coef.small <- as.data.frame(coefficients(model_small))
lda.coef.tbl.small <- rownames_to_column(lda.coef.small,"measurement")

#rename measurements for graphing
lda.coef.tbl$measurement <- as.character(lda.coef.tbl$measurement)
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "totrootlnth"] <- "Total Root Length"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "surfarea"] <- "Root Surface Area"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "avgdiam"] <- "Average Root Diameter"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "rootvol"] <- "Total Root Volume"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "crossings"] <- "Root Crossings"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "forks"] <- "Root Forks"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "tips"] <- "Root Tips"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "maxht"] <- "Maximum Seedling Height"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "truelvs"] <- "True Leaves"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "totlflts"] <- "Total Leaflets"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "lflnth"] <- "Leaf Length"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "light"] <- "Lignin Height"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "cnode"] <- "Cotyledonary Node Height"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "thorns"] <- "Thorns"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "dryleaf"] <- "Dried Leaf Mass*"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "lfwtr"] <- "Leaf Water Content"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "wetsdling"] <- "Fresh Weight*"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "drysdling"] <- "Dried Seedling Mass*"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "sdlingwtr"] <- "Seedling Water Content"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "taprootlnth"] <- "Tap Root Length"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "coarserootdiam"] <- "Coarse Root Diameter"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "fineroots"] <- "Fine Roots"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "rootwt"] <- "Total Root Mass*"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "rootshoot"] <- "Root:Shoot*"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "sla"] <- "Specific Leaf Area"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "wp"] <- "Water Potential"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "anet"] <- "Anet"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "cond"] <- "Conductance"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "fivegwc"] <- "0-5cm Soil GWC"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "twentygwc"] <- "5-20cm Soil GWC"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "agr"] <- "Absolute Growth Rate*"
lda.coef.tbl$measurement[lda.coef.tbl$measurement == "trans"] <- "Transpiration Rate"

#graph loadings for LD1
lda.coef.tbl$colorLD1 <- ifelse(lda.coef.tbl$LD1 < 0, "Negative","Positive")
lda.tbl<-lda.coef.tbl %>% 
  arrange(abs(LD1))
ggbarplot(lda.tbl, x = "measurement", y = "LD1",
          fill = "colorLD1",
          color = "white",
          palette = c("#DB4325","black"),
          sort.val = "none",          # Sort the value in ascending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          #x.text.angle = 180,          # Rotate vertically x axis texts
          ylab = "LD1 Loading Score",
          #xlab = "Measurement",
          xlab = "",
          legend = "none",
          rotate = TRUE,
          ggtheme = theme_pubr(base_size = 20))+
  labs_pubr(base_size = 20)


ggsave("Figures/LD1_loadings_ascend.tiff", dpi = 1200, scale = 2)

#graph loadings for LD2
lda.coef.tbl$colorLD2 <- ifelse(lda.coef.tbl$LD2 < 0, "Negative","Positive")
lda.tbl.2<-lda.coef.tbl %>% 
  arrange(abs(LD2))
ggbarplot(lda.tbl.2, x = "measurement", y = "LD2",
          fill = "colorLD2",
          color = "white",
          palette = c("#DB4325","black"),
          sort.val = "none",          # Sort the value in ascending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          #x.text.angle = 90,          # Rotate vertically x axis texts
          ylab = "LD2 Loading Score",
          xlab = "",
          legend = "none",
          rotate = TRUE,
          ggtheme = theme_pubr(base_size = 20))+
  labs_pubr(base_size = 20)
ggsave("Figures/LD2_loadings_ascend.tiff", dpi = 1200, scale = 2)

# Make predictions
predictions <- model_all %>% predict(test.transformed)
names(predictions)

# Model accuracy, closer 1 the better
mean(predictions$class==test.transformed$tx)

# Predicted classes
head(predictions$class, 6)

# Predicted probabilities of class membership.
head(predictions$posterior, 6) 

# Linear discriminant
head(predictions$x, 3) 

#Graph LDA for all samples by points with ggplot2
lda.data.all <- cbind(train.transformed, predict(model_all)$x)
ggplot(lda.data.all, aes(LD1, LD2)) +
  geom_point(aes(color = tx))+
  stat_ellipse(aes(fill= tx),
                    alpha = 0.2,
               geom = "polygon",
               level=0.95)+
  scale_colour_manual(values = c("Ambient" = "grey30",
                                 "Drought" = "#DB4325",
                                 "Wet" = "Blue"))+
  scale_fill_manual(values = c("Ambient" = "grey30",
                                 "Drought" = "#DB4325",
                                 "Wet" = "Blue"))+
  ylab("LD2 (14.8%)") +
  xlab("LD1 (85.2%)") +
  theme_pubr(legend = "none")+
  guides(fill=FALSE)

#ggsave("Figures/LDA_all.tiff", height = 4, width = 5, dpi = 1200) #save figure

##small model LDA
#rename measurements for graphing
lda.coef.tbl.small$measurement <- as.character(lda.coef.tbl.small$measurement)
lda.coef.tbl.small$measurement[lda.coef.tbl.small$measurement == "totrootlnth"] <- "Total Root Length"
lda.coef.tbl.small$measurement[lda.coef.tbl.small$measurement == "surfarea"] <- "Root Surface Area"
lda.coef.tbl.small$measurement[lda.coef.tbl.small$measurement == "rootvol"] <- "Total Root Volume"
lda.coef.tbl.small$measurement[lda.coef.tbl.small$measurement == "crossings"] <- "Root Crossings"
lda.coef.tbl.small$measurement[lda.coef.tbl.small$measurement == "forks"] <- "Root Forks"
lda.coef.tbl.small$measurement[lda.coef.tbl.small$measurement == "tips"] <- "Root Tips"
lda.coef.tbl.small$measurement[lda.coef.tbl.small$measurement == "maxht"] <- "Maximum Seedling Height"
lda.coef.tbl.small$measurement[lda.coef.tbl.small$measurement == "lflnth"] <- "Leaf Length"
lda.coef.tbl.small$measurement[lda.coef.tbl.small$measurement == "taprootlnth"] <- "Tap Root Length"
lda.coef.tbl.small$measurement[lda.coef.tbl.small$measurement == "fineroots"] <- "Fine Roots"

#graph loadings for LD1
lda.coef.tbl.small$colorLD1 <- ifelse(lda.coef.tbl.small$LD1 < 0, "Negative","Positive")
lda.tbl.1<-lda.coef.tbl.small %>% 
  arrange(abs(LD1))
ggbarplot(lda.tbl.1, x = "measurement", y = "LD1",
          fill = "colorLD1",
          color = "white",
          palette = c("#DB4325","black"),
          sort.val = "none",          # Sort the value in ascending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          #x.text.angle = 180,          # Rotate vertically x axis texts
          ylab = "LD1 Loading Score",
          #xlab = "Measurement",
          xlab = "",
          legend = "none",
          rotate = TRUE,
          ggtheme = theme_pubr(base_size = 20))+
  labs_pubr(base_size = 20)
#ggsave("Figures/LD1_loadings_ascend.tiff", dpi = 1200, scale = 2)

#graph loadings for LD2
lda.coef.tbl.small$colorLD2 <- ifelse(lda.coef.tbl.small$LD2 < 0, "Negative","Positive")
lda.tbl.2<-lda.coef.tbl.small %>% 
  arrange(abs(LD2))
ggbarplot(lda.tbl.2, x = "measurement", y = "LD2",
          fill = "colorLD2",
          color = "white",
          palette = c("#DB4325","black"),
          sort.val = "none",          # Sort the value in ascending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          #x.text.angle = 90,          # Rotate vertically x axis texts
          ylab = "LD2 Loading Score",
          xlab = "",
          legend = "none",
          rotate = TRUE,
          ggtheme = theme_pubr(base_size = 20))+
  labs_pubr(base_size = 20)
#ggsave("Figures/LD2_loadings_ascend.tiff", dpi = 1200, scale = 2)

# Make predictions
predictions <- model_small %>% predict(test.transformed)
names(predictions)

# Model accuracy, closer 1 the better
mean(predictions$class==test.transformed$tx)

# Predicted classes
head(predictions$class, 6)

# Predicted probabilities of class membership.
head(predictions$posterior, 6) 

# Linear discriminant
head(predictions$x, 3) 

#Graph LDA for all samples by points with ggplot2
lda.data.all <- cbind(train.transformed, predict(model_small)$x)
ggplot(lda.data.all, aes(LD1, LD2)) +
  geom_point(aes(color = tx))+
  stat_ellipse(aes(fill= tx),
               alpha = 0.2,
               geom = "polygon",
               level=0.95)+
  scale_colour_manual(values = c("Ambient" = "grey30",
                                 "Drought" = "#DB4325",
                                 "Wet" = "Blue"))+
  scale_fill_manual(values = c("Ambient" = "grey30",
                               "Drought" = "#DB4325",
                               "Wet" = "Blue"))+
  ylab("LD2 (39%)") +
  xlab("LD1 (61%)") +
  theme_pubr(legend = "none")+
  guides(fill=FALSE)

#ggsave("Figures/LDA_all.tiff", height = 4, width = 5, dpi = 1200) #save figure


