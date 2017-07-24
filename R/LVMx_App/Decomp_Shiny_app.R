library(shiny)
library(ggplot2)
library(data.table)
library(RColorBrewer)


setwd("C:/Users/jmaburto/Documents/GitHub/Violence-and-Lifespan-variation/R/LVMx_App/")


rsconnect::setAccountInfo(name='jmaburto',
                          token='7310E08D0D081D3C3CABCAA90D18045E',
                          secret='Vzlie6RN39/THGhWKatGf/C68yZp+RENdTdOl/ey')


 # source('C:/Users/jmaburto/Documents/GitHub/Violence-and-Lifespan-variation/R/2_3_Reshape_Results.R')
 # 
 # #transform datasets to have a faster response in shinny server
 # 
 # getData.function.g <- function(Data2,state ,initial,final){
 #    #AMS= #### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 4. Birth, 5. Other AMS,HIV
 # 
 #   Data <- cbind(Data2[,1:6], AMS = rowSums(Data2[,c(7,10,8,9,12,14)]), Diabetes = Data2[,c(11)], IHD = Data2[,c(13)],
 #                 LungCancer = Data2[,c(16)], Cirrhosis = Data2[,c(17)],Homicide = Data2[,c(18)],TAcc = Data2[,c(19)],
 #                 Rest = rowSums(Data2[,c(15,20,21)]))
 # 
 #   colnames(Data) <- c('Name','Region','Sex','State','year','age1',1:8)
 #   Data.melt               <- melt(Data, id.vars = 1:6,variable.name = 'Cause',value.name = 'Contribution')
 #   levels(Data.melt$Cause) <- c('AMS','Diabetes','IHD','Lung Cancer','Cirrhosis',
 #                                'Homicide','Traffic accidents','Rest')
 #   #Data.melt   <- Data.melt[Data.melt$year >= initial & Data.melt$year < final & Data.melt$Name == state, ]
 # 
 #   #Data.melt <- Data.melt[Data.melt$age1 < 100,]
 #   Labels.age            <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
 #                              '40-44','45-49','50-54','55-59','60-64','65-69',
 #                              #                          "70-74","75-79","80-84","85-89","90-94","95+")
 #                              "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105+")
 #   Data.melt$Age       <- (cut(Data.melt$age1+1, breaks=c(seq(0,105,5),Inf),labels=Labels.age))
 #   Data.melt5 <- Data.melt[,list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,year,Cause,Age)]
 # 
 #   #Data.melt5$Contribution <- round(Data.melt5$Contribution,2)
 #   Data.melt5
 # }
 # 
 # state.ind   <- state <- 'National'
 # initial.ind <- initial <- 2000
 # final.ind   <- final   <- 2010
 # Data.ex     <- getData.function.g(Data2 = DT.Decomp.ex,state = NULL,initial = NULL,final = NULL)
 # Data.ed     <- getData.function.g(Data2 = DT.Decomp.ed,state = NULL,initial = NULL,final = NULL)
 # 
 # 
 # getData2.function <- function(Data = DT.Decomp.ex){
 #   colnames(Data) <- c('Name','Region','Sex','State','year','age1',1:14,16)
 #   Data.melt               <- melt(Data, id.vars = 1:6,variable.name = 'Cause',value.name = 'Contribution')
 #   Data.melt[Data.melt$age1 >= 85,]$Cause <- "16"
 # 
 #   DT.out              <-  Data.melt[, list(Contribution = sum(Contribution)), by = list(Name,Sex,State,year,Cause)]
 # 
 #   DT.out$Contribution <- round(DT.out$Contribution,2)
 #   DT.out
 # }
 # 
 # Data.ex2 <- getData2.function(Data = DT.Decomp.ex)
 # Data.ed2 <- getData2.function(Data = DT.Decomp.ed)
 # 
 # 
 # 
 # 
 # setwd("C:/Users/jmaburto/Documents/GitHub/Violence-and-Lifespan-variation/R/LVMx_App/")
 # # run this line to update results and to update shinny app info
 # save(Data.ex,Data.ed,Data.ex2,Data.ed2,DT.LTmx, file = 'Shinny_data.RData')
 # 

runApp()
