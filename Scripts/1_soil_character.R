##Soil chem and characterization statistics##
##Greenhouse Mesquite Study
#Austin Rutherford
#email: arutherford@email.arizona.edu
#10/14/2020

##install packages if needed, commented out since there are already installed
# install.packages("tidyverse")
# install.packages("agricolae")
# install.packages("psych")

##load packages
library(tidyverse)
library(agricolae)
library(psych)

#bring the data in
soil <- read.csv(file = "Data/soil_character.csv")
soil$depth <- as.factor(soil$depth)#make depth a factor
soil$type <- as.factor(soil$type) #make type a factor

# recode soil type for t-test
soil %>% 
  mutate(type = recode(type,
                "Sandy Loam" = 1,
                "Loamy Sand" = 2))

soil$type <- as.numeric(soil$type) #make type a number

str(soil)

#descriptive stats by treatment
soil.desc <- describeBy(soil, soil$depth)
print(soil.desc, digits = 5)

#Welch two sample t-test for soil variables between depth
#pH
ph.test <- t.test(pH ~ depth, data = soil)
ph.test$p.value
ph.test$estimate
ph.test$stderr

#EC
EC.test <- t.test(EC ~ depth, data = soil)
EC.test$p.value
EC.test$estimate
EC.test$stderr

#Ca
Ca.test <- t.test(Ca ~ depth, data = soil)
Ca.test$p.value
Ca.test$estimate
Ca.test$stderr

#Mg
Mg.test <- t.test(Mg ~ depth, data = soil)
Mg.test$p.value
Mg.test$estimate
Mg.test$stderr

#Na
Na.test <- t.test(Na ~ depth, data = soil)
Na.test$p.value
Na.test$estimate
Na.test$stderr

#K
K.test <- t.test(K ~ depth, data = soil)
K.test$p.value
K.test$estimate
K.test$stderr

#nitrate
nitrate.test <- t.test(nitrate ~ depth, data = soil)
nitrate.test$p.value
nitrate.test$estimate
nitrate.test$stderr

#phosphate
phosphate.test <- t.test(phosphate ~ depth, data = soil)
phosphate.test$p.value
phosphate.test$estimate
phosphate.test$stderr

#esp
esp.test <- t.test(esp ~ depth, data = soil)
esp.test$p.value
esp.test$estimate
esp.test$stderr

#CEC
CEC.test <- t.test(CEC ~ depth, data = soil)
CEC.test$p.value
CEC.test$estimate
CEC.test$stderr

#sand
sand.test <- t.test(sand ~ depth, data = soil)
sand.test$p.value
sand.test$estimate
sand.test$stderr

#silt
silt.test <- t.test(silt ~ depth, data = soil)
silt.test$p.value
silt.test$estimate
silt.test$stderr

#clay
clay.test <- t.test(clay ~ depth, data = soil)
clay.test$p.value
clay.test$estimate
clay.test$stderr

#thera r
theta.test <- t.test(thetar ~ depth, data = soil)
theta.test$p.value
theta.test$estimate
theta.test$stderr


#type
type.test <- t.test(type ~ depth, data = soil)
type.test$p.value
type.test$estimate
type.test$stderr
