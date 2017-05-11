

setwd("C:/Users/jmaburto/Documents/GitHub/Violence-and-Lifespan-variation")

source("R/Functions.R")

library(data.table)
library(reshape2)
library(latticeExtra)
#Groups used in the article:
# 1. Amenable to meical service: 1+2+3+4+6
# 2. Diabetes: 5
# 3. IHD:7
# 4. HIV:8
# 5. Lung cancer: 10
# 6. Cirrhosis: 11
# 7. Homicide: 12
# 8. Road traffic: 13 
# 9. Suicide: 9
# 10. All other causes: 14+15+16

# 1 <- males
# 2 <- females

## minimum mortality rates by state, year, sex, cause
#constrain to the original ones
data       <- local(get(load("Data/smoothed rates_ConstraintOrig.RData")))
#constrain to the smoothed total ones
#data       <- local(get(load("Data/smoothed rates_ConstraintSmooth.RData")))
# No constrain, sum over smoother causes
#data       <- local(get(load("Data/smoothed rates_No Constraint.RData")))

data                    <- data[data$year > 1994,]
data$sex2               <- 'm'
data[data$sex==2,]$sex2 <- 'f'
unique(data$sex2)
unique(data$year)
max(data$age)

# calculate life expectancy and edagger for each state 
e0.data                <- data[,list(value = LifeExpectancy(mx=mx,sex=sex2[1])), by = list(year,sex,state)]
e0.data$indicator      <- 'e0'
edagger.data           <- data[,list(value = edagger.frommx(mx=mx,sex=sex2[1])), by = list(year,sex,state)]
edagger.data$indicator <- 'edagger'
e0.data[which.min(e0.data$value),]
e0.data[which.max(e0.data$value),]

 xyplot(value ~ year|sex,groups = state,data = edagger.data,type="l",
        col= c(rep('grey',32)),lwd=2.5,between=list(x=1))
 
 xyplot(value ~ year|sex,groups = state,data = e0.data,type="l",
        col= c(rep('grey',32)),lwd=2.5,between=list(x=1))
 

