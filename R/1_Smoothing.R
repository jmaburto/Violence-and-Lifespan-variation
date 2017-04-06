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


'All other'
#### sex=1 <- males
#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 
#### 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer,
#### 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories


# These data comes from INEGI data
load('Data/Counts&Rates_1990-2015Mex.RData')

# Get total for the country
National <- Data_Counts[, lapply(.SD, sum, na.rm=TRUE), by= list(year,sex,age), 
                        .SDcols=colnames(Data_Counts)[5:length(colnames(Data_Counts))]] 
National <- National[National$year > 1994, c(1:3,5:20)]

# State specific data
States   <- Data_Counts[Data_Counts$year > 1994, c(1:3,5:20)]


# Now, smooth each cause independently, then calculate proportions and apply those to
# national lifetables.

source("R/Functions.R")

for (i in causes){
  Mxs      <- Counts[,sm.chunk.2(.SD,i),by=list(state,sex)]
  Mxs      <- Mxs[with(Mxs,order(year,sex,state,age)),]
  sm.rates.age.only[,paste0("g",i-4)] <- Mxs$mxs
  print(i)
  # cbind(sm.rates,i= Mxs$mxs)
}

