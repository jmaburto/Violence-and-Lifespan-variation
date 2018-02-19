# Results at the national level
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

source('R/2_1_LifeExpectancy&LifeDisparity.R')
source('R/Functions.R')
Decomp.ex <- local(get(load("Data/DecompResults_ex_List.RData")))
Decomp.ed <- local(get(load("Data/DecompResults_ed_List.RData")))


#get results for decomp in ex un a data.table
DT.Decomp.ex <- do.call(rbind,lapply(1:2,function(j,y){
  x <- y[[j]]
  w <- do.call(rbind,
               lapply(names(x), 
                      FUN = my_reshape.function, 
                      DecompIn = x))
  W <- cbind(sex = as.integer(j),w)
  W
},
y=Decomp.ex))

#get results for decomp in ed un a data.table
DT.Decomp.ed <- do.call(rbind,lapply(1:2,function(j,y){
  x <- y[[j]]
  w <- do.call(rbind,
               lapply(names(x), 
                      FUN = my_reshape.function, 
                      DecompIn = x))
  W <- cbind(sex = as.integer(j),w)
  W
},
y=Decomp.ed))



#DT       <- as.data.table(melt(D, 
#                               id.vars = list("state","year","age"),
#                               variable.name = "Cause"))


gdata::keep(DT.Decomp.ed,DT.Decomp.ex,DT.LTmx,DT.mxCOD,sure = T)
source('R/Functions_2.R')

#####################################################################################
#Groups used in the article:
# 1. Amenable to medical service: 1+2+3+4+6
# 2. Diabetes: 5
# 3. IHD:7
# 4. HIV:8
# 5. Lung cancer: 10
# 6. Cirrhosis: 11
# 7. Homicide: 12
# 8. Road traffic: 13 
# 9. Suicide: 9
# 10. All other causes: 14+15+16

###########################################

# adding region name columns is then easy
DT.Decomp.ed  <- cbind(region=region.recvec[as.character(DT.Decomp.ed$state)],DT.Decomp.ed)
DT.Decomp.ex  <- cbind(region=region.recvec[as.character(DT.Decomp.ex$state)],DT.Decomp.ex)
DT.LTmx       <- cbind(region=region.recvec[as.character(DT.LTmx$state)],DT.LTmx)
DT.mxCOD      <- cbind(region=region.recvec[as.character(DT.mxCOD$state)],DT.mxCOD)

# adding state name columns is then easy
DT.Decomp.ed  <- cbind(state.name=state.name.vec[as.character(DT.Decomp.ed$state)],DT.Decomp.ed)
DT.Decomp.ex  <- cbind(state.name=state.name.vec[as.character(DT.Decomp.ex$state)],DT.Decomp.ex)
DT.LTmx       <- cbind(state.name=state.name.vec[as.character(DT.LTmx$state)],DT.LTmx)
DT.mxCOD      <- cbind(state.name=state.name.vec[as.character(DT.mxCOD$state)],DT.mxCOD)


# Turn region and Sex into levelled/ordered factors
DT.Decomp.ed$region  <- factor(DT.Decomp.ed$region, levels = 0:3, labels = c('National','South', 'Central', 'North'))
DT.Decomp.ex$region  <- factor(DT.Decomp.ex$region, levels = 0:3, labels = c('National','South', 'Central', 'North'))
DT.LTmx$region       <- factor(DT.LTmx$region, levels = 0:3, labels = c('National','South', 'Central', 'North'))
DT.mxCOD$region      <- factor(DT.mxCOD$region, levels = 0:3, labels = c('National','South', 'Central', 'North'))

DT.Decomp.ed$sex  <- factor(DT.Decomp.ed$sex, levels = 1:2, labels = rev(c("Females", "Males")))
DT.Decomp.ex$sex  <- factor(DT.Decomp.ex$sex, levels = 1:2, labels = rev(c("Females", "Males")))
DT.LTmx$sex       <- factor(DT.LTmx$sex, levels = 1:2, labels = rev(c("Females", "Males")))
DT.mxCOD$sex      <- factor(DT.mxCOD$sex, levels = 1:2, labels = rev(c("Females", "Males")))

