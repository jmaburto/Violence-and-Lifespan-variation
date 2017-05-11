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

# These data comes from INEGI data micro files
load('Data/Counts&Rates_1990-2015Mex.RData')

source('R/Functions.R')
# Get total for the country
National <- Data_Counts[, lapply(.SD, sum, na.rm=TRUE), by= list(year,sex,age), 
                        .SDcols=colnames(Data_Counts)[5:length(colnames(Data_Counts))]] 

National <- National[National$year > 1994 & National$year < 2016,]

####### Decompose everything
Sex <- unique(National$sex)
Years <- unique(National$year)
#i<-1
#j<-1995

National.decomp <- NULL
  for ( i in Sex){
    for (j in (Years[-length(Years)])){
      m1 <- as.matrix(National[National$year == j & National$sex==i, 5:20, with=F]/
                        National[National$year == j & National$sex==i,]$Pop)
      m2 <- as.matrix(National[National$year == (j+1) & National$sex==i, 5:20, with=F]/
                        National[National$year == (j+1) & National$sex==i,]$Pop)
      
      if (i == 1) Sx <- "m"
      if (i == 2) Sx <- "f"
      
      CoDecomp.e0             <-  mydecomp(func=e0frommxc, rates1 = c(m1),rates2 = c(m2), N=50, sex=Sx)
      CoDecomp.ed             <-  mydecomp(func=edaggerfrommxc, rates1 = c(m1),rates2 = c(m2), N=50, sex=Sx)
      dim(CoDecomp.e0)        <- dim(m1)
      dim(CoDecomp.ed)        <- dim(m1)
      dimnames(CoDecomp.e0)   <- dimnames(m1)
      dimnames(CoDecomp.ed)   <- dimnames(m1)
      row.names(CoDecomp.e0)  <- NULL
      row.names(CoDecomp.ed)  <- NULL
      
      l <- dim(m1)[1]
      R1 <- data.table(cbind(Year = rep(j,l), Sex = rep(i,l), measure= rep(1,l), CoDecomp.e0))
      R2 <- data.table(cbind(Year = rep(j,l), Sex = rep(i,l), measure= rep(2,l), CoDecomp.ed))
      R3 <- rbind(R1,R2)
      
      National.decomp <- rbind(National.decomp,R3)
    }
  }


# 1 is life expectancy
# 2 is life disparity

save(National.decomp,file="Data/National.decomp.RData")



