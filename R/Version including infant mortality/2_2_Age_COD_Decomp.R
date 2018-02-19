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
# Get total for the country
#mx <- DT.mxCOD[year==1995 & sex == 1 & state==0,]$mx
#sex = 'f'

#training data
#DT.mxCOD <- DT.mxCOD[state == 0,]
#training.data

#get decompositin of changes in life expectancy and life disparity by age and cause of death

# some useful indicators
states <- sort(unique(DT.mxCOD$state))
sexes  <- sort(unique(DT.mxCOD$sex))
years  <- sort(unique(DT.mxCOD$year))
ages   <- sort(unique(DT.mxCOD$age))
causes <- colnames(DT.mxCOD)[8:22]
Empty  <- matrix(0,nrow = length(ages), ncol = length(causes), dimnames = list(ages,causes)) 

#st <- 0
#yr <- 1995
# get matrices in lists to do faster the calculation
Mat.list <- lapply(sexes, function(sx,LTC2,years,Empty,ages,causes){
  LTC    <- LTC2[LTC2$sex == sx,]
Sex.List <- lapply(states, function(st,LTC,Empty,years,ages,causes){
  Mat    <- LTC[LTC$state == st,]
  YRlist <- lapply(years, function(yr,Mat,Empty,causes,ages){
    Mat2           <- as.matrix(Mat[Mat$year == yr,c(causes), with = F])
    rownames(Mat2) <- ages
    Empty[rownames(Mat2),colnames(Mat2)] <- Mat2
    Empty
  }, Mat = Mat, Empty = Empty, causes = causes, ages= ages)
  names(YRlist) <- years
  YRlist
}, LTC = LTC, years=years, Empty = Empty, causes = causes, ages=ages)
names(Sex.List) <- states
Sex.List
},LTC2 = DT.mxCOD, years=years, Empty = Empty, causes = causes, ages=ages)
names(Mat.list) <- sexes



# sex 
#x <- Mat.list[[as.character(1)]]
# state
#y <- x[[as.character(0)]]
# year 
#z <- y[[as.character(1995)]]
#i <- 1
#j <- 0
#years <- 1995:1996
#yr <- 1995

#install.packages('devtools')
#library(devtools)
#install_github('nathanvan/parallelsugar')

library(parallelsugar)

Decomp.results.list <- list()
state.list          <- list()

system.time(


for (i in sexes){
  print(i)
  x <- Mat.list[[as.character(i)]]
  for (j in states){
    print(j)
    y <- x[[as.character(j)]]
    
    Decomp.list <- mclapply(years[-length(years)],function(yr,y,e0frommxc,i){
      
      contrib           <- mydecomp(func = e0frommxc,
                          rates1 = c(y[[as.character(yr)]]),
                          rates2 = c(y[[as.character(yr+1)]]),N = 50,sex = i)
      dim(contrib)      <- dim(y[[as.character(yr)]])
      dimnames(contrib) <- dimnames(y[[as.character(yr)]])
      contrib },y= y,e0frommxc = e0frommxc, i = i, mc.cores = 4)
    names(Decomp.list)  <- years[-length(years)]
    state.list[[as.character(j)]] <- Decomp.list
  }
  Decomp.results.list[[as.character(i)]]  <- state.list
}
)
save(Decomp.results.list, file =  'Data/DecompResults_ex_List.RData')
gc()
# bruger   system forløbet 
# 44.07    60.37  6009.63 




Decomp.results.edagger <- list()
state.list          <- list()

system.time(
  
  
  for (i in sexes){
    print(i)
    x <- Mat.list[[as.character(i)]]
    for (j in states){
      print(j)
      y <- x[[as.character(j)]]
      
      Decomp.list <- mclapply(years[-length(years)],function(yr,y,e0frommxc,i){
        
        contrib           <- mydecomp(func = e0frommxc,
                                      rates1 = c(y[[as.character(yr)]]),
                                      rates2 = c(y[[as.character(yr+1)]]),N = 50,sex = i)
        dim(contrib)      <- dim(y[[as.character(yr)]])
        dimnames(contrib) <- dimnames(y[[as.character(yr)]])
        contrib },y= y,e0frommxc = edaggerfrommxc, i = i, mc.cores = 4)
      names(Decomp.list)  <- years[-length(years)]
      state.list[[as.character(j)]] <- Decomp.list
    }
    Decomp.results.edagger[[as.character(i)]]  <- state.list
  }
)
save(Decomp.results.edagger, file =  'Data/DecompResults_ed_List.RData')
gc()
