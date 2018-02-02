##################################################################################
# Program to convert to proportions##############################################
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

setwd("C:/Users/jmaburto/Documents/GitHub/Violence-and-Lifespan-variation")

#### sex=1 <- males
#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 
#### 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer,
#### 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories

# if you want to re smooth
#source(file = 'R/1_1_Smoothing.R')

# These data comes from 1_1_Smoothing.R
load('Data/Smooth_Deaths_withIllDefined.RData')
unique(Dxs$Cause)

#Get data for population
load('Data/Population_CONAPO.rdata')
Nx     <- (basepryentMX)
gdata:: keep(Dxs,Nx,sure = T)
source('R/Version with INEGI data/Functions.R')

# Get homogeneous datasets
# rename variables
names(Nx) <- c('row','year','state.name','state','cvegeo','sex2','age','Nx')
# Get a variable name as integer for sex
Nx$sex <- 2 # for females
Nx$sex[Nx$sex2=='Hombres'] <- 1 # for males

# Get the same order in all of them 
Nx <- data.table(Nx)
Nx <- Nx[,c('year','sex','state','age','Nx')]
Nx <- Nx[year >= 1995 & year <= 2015,]
# order all datasets accordingly
Nx  <- Nx[order(year,sex,state,age),]
Dxs <- Dxs[order(year,sex,state,Cause,age),]
# merge population to Dx 
DxNx<- merge(Dxs,Nx,all = T)
# estimate age.specific mortality rates
DxNx                <- DxNx[,mx :=Dxs/Nx]
# maybe a good idea to fit a Kannisto model for the last ages
DxNx[is.na(mx),]$mx <- 0
DxNx[is.infinite(mx),]$mx <- 1
DxNx[DxNx$mx > 1,]$mx <- 1

unique(DxNx$Cause)

# Now transform Dxs to have a similar shape as DxNx
Dxs.cast <- dcast(DxNx,year+sex+state+age ~ Cause,value.var = 'mx')

gdata::keep(Dxs.cast,DxNx,sure=T)





