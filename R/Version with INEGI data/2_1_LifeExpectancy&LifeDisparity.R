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

source('R/Version with INEGI data/1_2_Rates&Proportions.R')
source('R/Version with INEGI data/Functions.R')
# Get total for the country
#mx <- DT.mxCOD[year==1995 & sex == 1 & state==0,]$mx
#sex = 'f'

#get lifetables with life expectancy,dx and e.dagger
Dxs.cast$sex.ind <- Dxs.cast$sex
Dxs.cast <- data.table(Dxs.cast)
Dxs.cast$mx <- rowSums(Dxs.cast[,5:20])
DT.LTmx          <- Dxs.cast[,list(age=LifeTable.DT(.SD)[,1],lx = LifeTable.DT(.SD)[,2],
                                  dx = LifeTable.DT(.SD)[,3],ex = LifeTable.DT(.SD)[,4],
                                  e.dagger = LifeTable.DT(.SD)[,5]), by = list(year,sex.ind,state)]

