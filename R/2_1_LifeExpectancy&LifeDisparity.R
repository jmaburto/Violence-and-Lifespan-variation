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

source('R/1_2_Rates&Proportions.R')
source('R/Functions.R')
# Get total for the country
#mx <- DT.mxCOD[year==1995 & sex == 1 & state==0,]$mx
#sex = 'f'

#get lifetables with life expectancy,dx and e.dagger
DT.mxCOD$sex.ind <- DT.mxCOD$sex
DT.LTmx          <- DT.mxCOD[,list(age=LifeTable.DT(.SD)[,1],lx = LifeTable.DT(.SD)[,2],
                                  dx = LifeTable.DT(.SD)[,3],ex = LifeTable.DT(.SD)[,4],
                                  e.dagger = LifeTable.DT(.SD)[,5]), by = list(year,sex,state)]


save(DT.mxCOD,file = 'Data/Mex_mx_Anthony.RData')