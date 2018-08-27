#################################################################################
# 1. Program to smooth causes of death each year over age########################
# 2. convert to proportions######################################################
# 3. Apply proportions to lifetable mx at the national level#####################
# 4. I am interested in 22 years, from 1995-2017#################################
# 5. What is the effect of the rise of homicide mortality on LV##################
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
load('Data/Counts&Rates_1990-2016Mex.RData')
Data_Counts[Data_Counts$year == 2016,]
Data_Counts <- Data_Counts[Data_Counts$year > 1994 & Data_Counts$year <= 2016,]

# Get total for the country
National       <- Data_Counts[, lapply(.SD, sum, na.rm=TRUE), by= list(year,sex,age), 
                        .SDcols=colnames(Data_Counts)[5:length(colnames(Data_Counts))]] 
National$state <- 0
National       <- National[,c(1:2,22,3:21)]
Data_Counts    <- rbind(Data_Counts,National)
Data_Counts    <- Data_Counts[with(Data_Counts,order(year,sex,state,age)),]
Data_Counts    <- Data_Counts[,c(1:19,21)]
gdata::keep(Data_Counts,sure = T)

# Now, smooth each cause independently, get the shape and then return to the total
# then calculate proportions and apply those to national lifetables.

# Ill defined causes are taken out, as if they were portornially distributed
source("R/Functions.R")

#DT    <- Data_Counts[Data_Counts$state == 1 & Data_Counts$sex == 1 & Data_Counts$year == 1995,]
#MDx   <- DT[,1:6]
#DX    <- DT[,6]
# mysmooth.DT(DT = DT, nage = nage, ages = ages)
 
# smooth deaths, see file Functions, run this line of you want to re smooth
# Data_Counts <- Data_Counts[Data_Counts$state == 1 & Data_Counts$sex == 1 & Data_Counts$year == 1995,]
ages      <- 0:100
nage      <- 5
Dxs       <- mysmooth.DT(DT = Data_Counts, nage = nage, ages = ages)

# save Dxs, it takes time.
save(Dxs, file = 'Data/Smooth_Deaths.RData')


i <- 0
y <- 2011
plot(Data_Counts[year==y & sex ==1 & state == i,]$age,Data_Counts[year==y & sex ==1 & state == i,]$`12`)
Dxs[year==y & sex ==1 & state == i & Cause == 12, lines(age, Dxs)]
Dxs[year==1995 & sex ==1 & state == i & Cause == 12, lines(age, Dxs)]
Dxs[year==2014 & sex ==1 & state == i & Cause == 12, lines(age, Dxs)]
  
# An example of smoothing one cause
# now smooth (did this with GC)
# i <- causes[1]
# DX      <- acast(National[National$sex==1,], age~year, value.var = colnames(National)[i], fill = 0,drop = F)
# EX      <- acast(National[National$sex==1,], age~year, value.var = "Pop", fill = 0,drop=F)
# W     		  <- EX*0
# W[EX != 0] 	<- 1
# fit   	<- Mort1Dsmooth(
#   x = ages,
#   y = DX[,17],
#   w = W[,17],
#   offset = log(EX[,17]),
#   control=list(RANGE=c(10^0 , 10^8)))
# 
# plot(ages, log(DX[,17]/EX[,17]))
# plot(ages, DX[,17])
# lines(ages,fit$fitted.values)
# length(x)
# length(y)
# length(offset)
# length(w)
