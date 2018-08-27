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
source('R/Functions.R')

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
Dxs <- Dxs[year >= 1995 & year <= 2015,]
# order all datasets accordingly
Dx  <- Dx[order(year,sex,state,age),]
Nx  <- Nx[order(year,sex,state,age),]
Dxs <- Dxs[order(year,sex,state,Cause,age),]

Dx <- Dx[state==0,]
Nx <- Nx[state==0,]
Dxs<- Dxs[state== 0 & Cause ==12,]


# merge population to Dx 
DxNx<- merge(Dx,Nx,all = T)
DxNxHom <- merge(DxNx,Dxs, all = T)
# estimate age.specific mortality rates
DxNxHom[,mx :=(Dx)/Nx]


# maybe a good idea to fit a Kannisto model for the last ages
sort(unique(DxNxHom[is.na(mx),]$age))
sort(unique(DxNxHom[is.infinite(mx),]$age))
sort(unique(DxNxHom[mx > 1,]$age))

#now fit a Kannisto model to all data
DxNxHom <- DxNxHom[order(year,sex,state,age),]
DxNxHom <- DxNxHom[,mxn := my.kannisto.fun(mx,x=80:94), by = list(year,sex,state)]
sort(unique(DxNxHom[is.na(mxn),]$age))
sort(unique(DxNxHom[is.infinite(mxn),]$age))
sort(unique(DxNxHom[mxn > 1,]$age))

DxNxHom[,hom.rates:= mxn*Dxs.prop]


### Get age.std rates
# Get the standard population in 2005

std.pop <- DxNx[year == 2005 & state == 0, sum(Nx), by = list(age)]/ DxNx[year == 2005 & state == 0, sum(Nx)]
std.pop <- c(std.pop$V1)
sum(std.pop)

DxNxHom[,std.hom:= hom.rates*std.pop, by = list(year,sex)]

fig.data <- DxNxHom[,sum(std.hom)*100000, by = list(year,sex)]
fig.data$sex <- as.factor(fig.data$sex)


library(ggplot2)
p <-ggplot(fig.data, aes(x = year,y = V1,colour=sex, group = factor(sex))) +
  ggtitle('Standardized homicide rates by sex') +
  geom_line(aes(),show.legend = F) +
  theme_light()+
  labs(y = "Homicide rate")+
  scale_colour_manual('Sex', values = c('red','blue')) 
  
p


pdf(file="Manuscript/AJPH Submission/RR Submission/ASMR.pdf",width=7,height=4,useDingbats = F)
p
dev.off()









