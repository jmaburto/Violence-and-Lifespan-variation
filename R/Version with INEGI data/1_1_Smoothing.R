#################################################################################
# 1. Program to smooth causes of death each year over age########################
# 2. convert to proportions######################################################
# 3. Apply proportions to lifetable mx at the national level#####################
# 4. I am interested in 20 years, from 1995-2015#################################
# 5. What is the effect of the rise of homicIde mortality on LV##################
# 6. How the mortality hump changed in 1995, 2000, 2005, 2010 and 2015###########
# 7. Do not want to use AM concept,##############################################
# 8. Then replicate results for state specfic effect of homicides in LV##########
#################################################################################

rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)
library(MortalitySmooth)

setwd("C:/Users/jmaburto/Documents/GitHub/Violence-and-Lifespan-variation")

#### sex=1 <- males
#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 
#### 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer,
#### 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories

# These data comes from INEGI data micro files
load('Data/Counts&Rates_1990-2015Mex.RData')
Data_Counts[Data_Counts$year == 2015,]
Data_Counts <- Data_Counts[Data_Counts$year > 1994 & Data_Counts$year < 2016,]

# Get total for the country
National       <- Data_Counts[, lapply(.SD, sum, na.rm=TRUE), by= list(year,sex,age), 
                        .SDcols=colnames(Data_Counts)[5:length(colnames(Data_Counts))]] 
National$state <- 0
National       <- National[,c(1:2,22,3:21)]

Data_Counts    <- rbind(Data_Counts,National)
Data_Counts    <- Data_Counts[with(Data_Counts,order(year,sex,state,age)),]
gdata::keep(Data_Counts,sure = T)

# Now, smooth each cause independently, get the shape and then return to the total
# then calculate proportions and apply those to national lifetables.

source("R/Version with INEGI data/Functions.R")

ages      <- 0:100
nage      <- 5
Dxs       <- mysmooth.DT(DT = Data_Counts, nage = nage, ages = ages)

# save Dxs, it takes time.
save(Dxs, file = 'Data/Smooth_Deaths_withIllDefined.RData')


i <- 0
y <- 2011
plot(Data_Counts[year==y & sex ==1 & state == i,]$age,Data_Counts[year==y & sex ==1 & state == i,]$`12`)
Dxs[year==y & sex ==1 & state == i & Cause == 12, lines(age, Dxs)]
Dxs[year==1995 & sex ==1 & state == i & Cause == 12, lines(age, Dxs)]
Dxs[year==2014 & sex ==1 & state == i & Cause == 12, lines(age, Dxs)]
