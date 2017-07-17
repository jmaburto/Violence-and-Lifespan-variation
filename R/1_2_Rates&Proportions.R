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
load('Data/Smooth_Deaths.RData')

#Calculate proportions of causes of death by age (remember to take care of 0 when applying to rates)
Dxs <- Dxs[,Dxs.prop := Dxs/sum(Dxs), by = list(year,sex,state,age)]
Dxs[is.na(Dxs.prop),]$Dxs.prop <- 0

#Get data for deaths and population
load('Data/Deaths_CONAPO.rdata')
load('Data/Population_CONAPO.rdata')
Nx     <- (basepryentMX)
Dx     <- (defspry)
gdata:: keep(Dxs,Nx,Dx,sure = T)

# Get homogeneous datasets
# rename variables
names(Nx) <- c('row','year','state.name','state','cvegeo','sex2','age','Nx')
names(Dx) <- c('row','year','state.name','state','cvegeo','sex2','age','Dx')
# Get a variable name as integer for sex
Nx$sex <- 2 # for females
Dx$sex <- 2 # for females
Nx$sex[Nx$sex2=='Hombres'] <- 1 # for males
Dx$sex[Dx$sex2=='Hombres'] <- 1 # for males
# Get the same order in all of them 
Dx <- data.table(Dx)
Nx <- data.table(Nx)
Dx <- Dx[,c('year','sex','state','age','Dx')]
Nx <- Nx[,c('year','sex','state','age','Nx')]
Dx <- Dx[year >= 1995 & year <= 2015,]
Nx <- Nx[year >= 1995 & year <= 2015,]
# order all datasets accordingly
Dx  <- Dx[order(year,sex,state,age),]
Nx  <- Nx[order(year,sex,state,age),]
Dxs <- Dxs[order(year,sex,state,Cause,age),]
# merge population to Dx 
DxNx<- merge(Dx,Nx,all = T)
# estimate age.specific mortality rates
DxNx                <- DxNx[,mx :=Dx/Nx]
DxNx[is.na(mx),]$mx <- 0
DxNx[is.infinite(mx),]$mx <- 0


# Now transform Dxs to have a similar shape as DxNx
Dxs.cast <- dcast(Dxs,year+sex+state+age ~ Cause,value.var = 'Dxs.prop')

# merge all data together and send the rest of causes when 0 to the cause 16
DxNxCOD           <- merge(DxNx,Dxs.cast,by = c('year','sex','state','age'), all = T)
colnames(DxNxCOD) <- c(colnames(DxNxCOD)[1:7], paste0('Cause_',c(1:14,16)))
DxNxCOD$Cause_16  <- (1 - rowSums(DxNxCOD[,8:21]))

# get mx by causes of death, check for NA
mx.COD       <- DxNxCOD[,8:22]*DxNxCOD$mx 
DT.mxCOD     <- cbind(DxNxCOD[,1:7],mx.COD)
gdata::keep(DT.mxCOD,sure=T)





