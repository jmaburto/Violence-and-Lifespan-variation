# Results at the national level
rm(list=ls(all=TRUE))
setwd("C:/Users/jmaburto/Documents/GitHub/Violence-and-Lifespan-variation")

#### sex=1 <- males
#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 
#### 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer,
#### 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories
# devtools::install_github('hadley/ggplot2')

#### Grouping
#### AMS: 7+10+8+9+12, Diabetes, IHD, LUNG CANCER, CIRRHOSIS, HOMICIDES, TRAFFIC ACCIDENTS,
#### REST: 14+21+15+20


#load('R/LVMx_App/Shinny_data.RData')
source('R/2_3_Reshape_Results.R')

library(ggthemes)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(data.table)
library(reshape2)

#install.packages('plotly')
# run this in a day or 2
#.rs.restartR()
#devtools::install_github('hadley/ggplot2')
#devtools::install_github("ropensci/plotly")

state.ind   <- 'National'
initial.ind <- 1995
final.ind   <- 2005
#Data        <- DT.Decomp.ex
#Data2        <- DT.Decomp.ex

getData.function <- function(Data = DT.Decomp.ex,state = state.ind,initial = initial.ind,final = final.ind){
  
  colnames(Data) <- c('Name','Region','Sex','State','year','age1',1:14,16)
  Data.melt               <- melt(Data, id.vars = 1:6,variable.name = 'Cause',value.name = 'Contribution')
  levels(Data.melt$Cause) <- c('Infect & respiratory','Cancers','Circulatory','Birth conditions',
                               'Diabetes','Other AMS','IHD','HIV','Suicide','Lung Cancer','Cirrhosis',
                               'Homicide','Traffic accidents','Other HD', 'Rest')
  Labels.age            <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                             '40-44','45-49','50-54','55-59','60-64','65-69',
                             "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105+")
  Data.melt$Age       <- (cut(Data.melt$age1+1, breaks=c(seq(0,105,5),Inf),labels=Labels.age))
  Data.melt5 <- Data.melt[,list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,year,Cause,Age)]

  Data.fig   <- Data.melt5[Data.melt5$year >= initial & Data.melt5$year < final, ]
  Data.fig   <- Data.fig[, list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Age)]
  Data.fig   <- Data.fig[Data.fig$Name == state,]
  Data.fig$Contribution <- round(Data.fig$Contribution,2)
  Data.fig
}

getData.function.g <- function(Data2 = DT.Decomp.ex,state = state.ind,initial = initial.ind,final = final.ind){
  Data <- cbind(Data2[,1:6], AMS = rowSums(Data2[,c(7,10,8,9,12)]), Diabetes = Data2[,c(11)], IHD = Data2[,c(13)],
                LungCancer = Data2[,c(16)], Cirrhosis = Data2[,c(17)],Homicide = Data2[,c(18)],TAcc = Data2[,c(19)],
                Rest = rowSums(Data2[,c(14,21,15,20)]))
  
  colnames(Data) <- c('Name','Region','Sex','State','year','age1',1:8)
  Data.melt               <- melt(Data, id.vars = 1:6,variable.name = 'Cause',value.name = 'Contribution')
  levels(Data.melt$Cause) <- c('AMS','Diabetes','IHD','Lung Cancer','Cirrhosis',
                               'Homicide','Traffic accidents','Rest')
  Data.melt <- Data.melt[Data.melt$age1 < 100,]
  Labels.age            <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                             '40-44','45-49','50-54','55-59','60-64','65-69',
                             "70-74","75-79","80-84","85-89","90-94","95+")
  Data.melt$Age       <- (cut(Data.melt$age1+1, breaks=c(seq(0,95,5),Inf),labels=Labels.age))
  Data.melt5 <- Data.melt[,list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,year,Cause,Age)]
  
  Data.fig   <- Data.melt5[Data.melt5$year >= initial & Data.melt5$year < final, ]
  Data.fig   <- Data.fig[, list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Age)] 
  Data.fig   <- Data.fig[Data.fig$Name == state,]
  Data.fig$Contribution <- round(Data.fig$Contribution,2)
  Data.fig
}

Data.fig <- getData.function(Data = DT.Decomp.ex) 
Data.fig <- getData.function.g(Data2 = DT.Decomp.ex) 

unique(Data.fig$Cause)

display.brewer.pal(8,name = 'Pastel2')

display.brewer.all()
base2 <- c(1:15)
base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],rev(brewer.pal(8,name = 'Spectral'))[7],'lightgrey')

p <- ggplot(Data.fig, aes(x = Age, y = Contribution, fill = Cause)) +
  scale_fill_manual('Cause of death', values = base2) + 
  geom_bar(aes(group = Cause), stat = "identity",position = "stack")+
  facet_wrap(~Sex)+
  theme_light()+
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme(axis.title.y = element_text(size = 12, angle = 90))+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  geom_hline(yintercept = 0)+
 coord_flip()
p

ggplotly(p,width = 1100, height = 500)



Data.fig2 <- getData.function(Data = DT.Decomp.ed)

q <- ggplot(Data.fig2, aes(x = Age, y = Contribution, fill = Cause)) +
  scale_fill_manual('Cause of death', values = base2) + 
  geom_bar(aes(group = Cause), stat = "identity")+
  facet_wrap(~Sex)+
  theme_light()+
  theme(text = element_text(size=22),
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x = "Age group", y = "Contribution (years)")+
  coord_flip()
q


q <- ggplot(Data.fig2, aes(x = Age, y = Contribution, fill = Cause)) +
  geom_bar(stat = "identity",position = "stack")
  


ggplotly(q)



q <- ggplotly(q)


#life expectancy at birth and lifespan variation at birth for males at the national level


# #plot(1:7,1:7,col=toupper(base2),pch=16,cex=5)
# # determine order by eyeballing colors to causes (HT J. Schoeley)
# myColours1 <- base2
# 
# my.settings1 <- list(
#   superpose.polygon=list(col=myColours1[], border="transparent"),
#   strip.background=list(col="grey"),
#   strip.border=list(col="black")
# )
# 
# fig.labels <- CoD.name.vec2
# 
# National.e0 <-barchart(Age ~ x|Period , 
#                        data=National.Fig.5[National.Fig.5$Measure == 1 & National.Fig.5$Sex ==1 &
#                                              National.Fig.5$Age != '0-4' & National.Fig.5$Age != '80-84' &
#                                              National.Fig.5$Age != '85-89' & National.Fig.5$Age != '90-94' &
#                                              National.Fig.5$Age != '95-99' & National.Fig.5$Age != '100-104' &
#                                              National.Fig.5$Age != '105-109',], 
#                        groups=Cause,  ylab= 'Age group',xlab='',
#                        strip=T,main='Male life expectancy',
#                        stack=TRUE,box.ratio=7,
#                        between = list(x=.5),
#                        #layout = c(4, 3),
#                        xlim=c(-.3,.3),
#                        scales = list(x = list(alternating=1),y=list(alternating=1),cex=.6), 
#                        par.settings=my.settings1,
#                        key = list(x=.01,y=.45,background="white",title="Cause",
#                                   points=list(col=myColours1,pch=19),text=list(fig.labels)
#                                   ,cex=1),
#                        panel=function(x,y,...){                      
#                          panel.abline(v=seq(-.3,.3,.1),lwd=1,lty=3,col="darkgrey",...)
#                          panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
#                          panel.barchart(...,border="transparent",x,y)
#                          panel.abline(v=0,lwd=1,lty=1,col="black",...)
#                        })
# National.e0
# 
# National.edagger <-barchart(Age ~ x|Period , 
#                             data=National.Fig.5[National.Fig.5$Measure == 2 & National.Fig.5$Sex ==1 &
#                                                   National.Fig.5$Age != '0-4' & National.Fig.5$Age != '80-84' &
#                                                   National.Fig.5$Age != '85-89' & National.Fig.5$Age != '90-94' &
#                                                   National.Fig.5$Age != '95-99' & National.Fig.5$Age != '100-104' &
#                                                   National.Fig.5$Age != '105-109',],
#                             groups=Cause,  ylab= "Age group",xlab="Contribution",
#                             strip=T,main='Male life disparity',
#                             stack=TRUE,box.ratio=7,
#                             between = list(x=.5),
#                             #layout = c(4, 3),
#                             xlim=c(-.13,.13),
#                             scales = list(x = list(alternating=1),y=list(alternating=1),cex=.6), 
#                             par.settings=my.settings1,
#                             panel=function(x,y,...){                      
#                               panel.abline(v=seq(-.3,.3,.05),lwd=1,lty=3,col="darkgrey",...)
#                               panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
#                               panel.barchart(...,border="transparent",x,y)
#                               panel.abline(v=0,lwd=1,lty=1,col="black",...)
#                             })
# National.edagger

